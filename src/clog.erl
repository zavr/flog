%%-----------------------------------------------------------------------------
%% Author: Andrew Gopienko
%% Created: 25.04.2009
%% Description: Cached log
%%-----------------------------------------------------------------------------
-module(clog).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/common.hrl").

%% ----------------------------------------------------------------------------
%% Defines
%% ----------------------------------------------------------------------------
-define(MAX_LINES,        20).
-define(IDLE_TIME_SEC,    1).
-define(WRAP_LINES_CHECK, 1000).  %% Каждые 1000-ую запись проверять размер лога
-define(WRAP_LOG_SIZE,    50).    %% 50 МБ

%% Server state
-record(state, {last_write=0,
                lines_written=0,
                wrap_lines_check=?WRAP_LINES_CHECK,
                wrap_log_size_byte=?WRAP_LOG_SIZE,
                max_lines=?MAX_LINES,
                delay_ms=?IDLE_TIME_SEC*1000,
                delay_us=?IDLE_TIME_SEC*1000000}).

%% ----------------------------------------------------------------------------
%% External exports
%% ----------------------------------------------------------------------------
-export([start_link/0, info/1, error/1, debug/1, debug/2, debug/3]).

%% --------------------------------------------------------------------
%% gen_server callbacks
%% --------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ====================================================================
%% External functions
%% ====================================================================
start_link()->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ====================================================================
% API functions
%% ====================================================================
info(Request) when is_list(Request) ->
    case is_info() of
  true -> gen_server:cast(?MODULE, {info, Request});
  false -> ok
    end.

error(Request) when is_list(Request) ->
    gen_server:cast(?MODULE, {error, Request}).

debug(Request) when is_list(Request) ->
    debug([], Request).

debug(Opts, Request) when is_list(Request) ->
    case is_debug(Opts) of
  true -> gen_server:cast(?MODULE, {debug, Request});
  false -> ok
    end.

debug(Opts, Fmt, Args) when is_list(Fmt), is_list(Args) ->
    case is_debug(Opts) of
  true -> gen_server:cast(?MODULE, {debug, ?FMT(Fmt, Args)});
  false -> ok
    end.

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    put(mq, []),
    WrapLinesCheck = config:get(log_wrap_lines_check, ?WRAP_LINES_CHECK),
    WrapLogSizeMB  = config:get(log_wrap_size,        ?WRAP_LOG_SIZE),
    LogMaxLines    = config:get(log_write_max_lines,  ?MAX_LINES),
    LogDelayMs     = config:get(log_write_delay_sec,  ?IDLE_TIME_SEC)*1000,
    State = #state{wrap_lines_check   = WrapLinesCheck,
                   wrap_log_size_byte = WrapLogSizeMB*1024*1024,
                   max_lines          = LogMaxLines,
                   delay_ms           = LogDelayMs,
                   delay_us           = LogDelayMs*1000,
                   last_write         = now()},
    {ok, State, LogDelayMs}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = {error, unexpected_call},
    {reply, Reply, State, State#state.delay_ms}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({Action, Data}, State) when Action == info;
                                        Action == error;
                                        Action == debug ->
    LogStr = mk_log_str(Action, Data),
    NewState = write_log(State, LogStr),
    NewState2 = check_wrap(NewState),
    {noreply, NewState2, get_idle(NewState2)};

handle_cast(_Msg, State) ->
    {noreply, State, get_idle(State)}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(timeout, State) ->
    NewState = write_log(State, []),
    {noreply, NewState, State#state.delay_ms};

handle_info(_Info, State) ->
    {noreply, State, get_idle(State)}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%% Local
%% --------------------------------------------------------------------
get_idle(#state{last_write=LW, delay_us=DelayUs}=State) ->
    case timer:now_diff(now(), LW) of
  Diff when Diff >= DelayUs -> 1;
  _ -> State#state.delay_ms
    end.

time_to_write(#state{last_write=LW, delay_us=DelayUs}) ->
    case timer:now_diff(now(), LW) of
  Diff when Diff >= DelayUs -> true;
  _                         -> false
    end.

lines_to_write(#state{max_lines=ML}, LogStr) ->
    Mq = get(mq),
    NewMq = case LogStr of
          [] -> Mq ;
          _  -> Mq ++ [LogStr]
            end,
    case length(NewMq) of
  Len when Len >= ML ->
      put(mq, []),
      {NewMq,[]};
  _ ->
      put(mq, NewMq),
      {[], NewMq}
    end.


check_wrap(#state{lines_written=LW,wrap_lines_check=WLC}=State) ->
    NewLW =
    if LW+1 >= WLC ->
        catch(flog:wrap_log_size(State#state.wrap_log_size_byte)),
        0;
    true ->
        LW+1
    end,
    State#state{lines_written=NewLW}.

%% --------------------------------------------------------------------
write_log(State, LogStr) ->
    {Lines,Mq} = lines_to_write(State, LogStr),
    case Lines of
  [] ->
      case time_to_write(State) of
    true -> do_write(State, Mq);
    false -> State
      end;
  _ ->
      do_write(State, Lines)
    end.


do_write(State, []) ->
    State#state{last_write=now()};
do_write(State, LogLines) ->
%    io:format("~p~n", [LogLines]),
    flog:write(LogLines),
    put(mq, []),
    State#state{last_write=now()}.

%% --------------------------------------------------------------------
is_info() ->
    case config:get(log_level, info) of
  error -> false; %% no info on ERROR level
  _     -> true
    end.

%% --------------------------------------------------------------------
is_debug(Opts) ->
    case config:get(log_level, info) of
  {debug,all} -> true;
  {debug,partial} -> check_debug_opts(Opts);
  _ -> false  %% no log_level in config | info | error | other
    end.

%% --------------------------------------------------------------------
%% Opts: [] | only if {debug, all}
%%       atom() | {phone,"Phone"}
%% --------------------------------------------------------------------
check_debug_opts([]) -> false;
check_debug_opts(Opts) ->
    case config:get(Opts, false) of
  L when is_list(L) -> proplists:get_value(debug, L, false);
  Val when is_boolean(Val) -> Val;
  _ -> false
    end.

%% ====================================================================
mk_log_str(Type, Rep) ->
%    io_lib:format(write_time(now(), Type) ++ format_report(Rep), []).
    [write_time(now(), Type) | format_report(Rep)].


write_time({_Mega,_Sec,Micro}=Now, Type) ->
    {{Y, Mo, D},{H, Mi, S}} = calendar:now_to_local_time(Now),
    io_lib:format("~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w.~.6.0w ~w ~n    ",
                  [Y, Mo, D, H, Mi, S, Micro, Type]).


format_report(Rep) when is_list(Rep) ->
    case string_p(Rep) of
  true -> io_lib:format("~s~n",[Rep]);
  _    -> format_rep(Rep,"")
    end;
format_report(Rep) ->
    io_lib:format(" ~p~n",[Rep]).

format_rep([{Tag,{utf8,Data}}|Rep],Offset) ->
    Utf32Data =
      unicode:characters_to_list(unicode:characters_to_binary(Data, unicode),
                                 unicode),
    [io_lib:format("~s~p: ~ts~n",
                   [Offset,Tag,Utf32Data]) | format_rep(Rep,"    ")];
format_rep([{Tag,Data}|Rep],Offset) ->
    [io_lib:format("~s~p: ~p~n", [Offset,Tag,Data]) | format_rep(Rep,"    ")];
format_rep([Other|Rep],Offset) ->
    [io_lib:format("~s~p~n", [Offset,Other]) | format_rep(Rep,"    ")];
format_rep(_,_) ->
    [].


string_p([]) ->
    false;
string_p(Term) ->
    string_p1(Term).

string_p1([H|T]) when is_integer(H), H >= $\s, H < 255 ->
    string_p1(T);
string_p1([$\n|T]) -> string_p1(T);
string_p1([$\r|T]) -> string_p1(T);
string_p1([$\t|T]) -> string_p1(T);
string_p1([$\v|T]) -> string_p1(T);
string_p1([$\b|T]) -> string_p1(T);
string_p1([$\f|T]) -> string_p1(T);
string_p1([$\e|T]) -> string_p1(T);
string_p1([H|T]) when is_list(H) ->
    case string_p1(H) of
  true -> string_p1(T);
  _    -> false
    end;
string_p1([]) -> true;
string_p1(_) ->  false.
