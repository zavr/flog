%%%----------------------------------------------------------------------
%%% File    : ejabberd_logger_h.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose :
%%% Created : 23 Oct 2003 by Alexey Shchepin <alexey@sevcom.net>
%%%----------------------------------------------------------------------

-module(flog).

-include_lib("kernel/include/file.hrl").
-include("../include/common.hrl").

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2,
         code_change/3]).

% Old API, redirect to clog
-export([info/1, error/1, debug/1, debug/2, debug/3]).

% API
-export([write/1]).
-export([reopen_log/0,wrap_log_size/1,chk_daemon/0]).

-record(state, {fd, file, last_date, daemon=true}).

%----------------------------------------------------------------------
% API functions
%----------------------------------------------------------------------
write(Lines) when is_list(Lines) ->
    error_logger:info_report(write, Lines).


wrap_log_size(MaxLogSize) when is_integer(MaxLogSize) ->
    gen_event:call(error_logger, ?MODULE, {wrap_size, MaxLogSize}).

%----------------------------------------------------------------------
% Old API functions
%----------------------------------------------------------------------
info(Arg) -> clog:info(Arg).

error(Arg) ->
    clog:error(Arg).

debug(Arg) -> clog:debug(Arg).
debug(Arg1,Arg2) -> clog:debug(Arg1,Arg2).
debug(Arg1,Arg2,Arg3) -> clog:debug(Arg1,Arg2,Arg3).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_event
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          Other
%%----------------------------------------------------------------------
init([App]) ->
    Daemon = chk_daemon(),
    LF = case application:get_env(App, flog_path) of
       {ok,F} -> F;
       _      -> "./priv/logs/flog.log"
         end,
%    io:format("~p~n", [{flog,lf,LF}]),
    case file:open(LF, [write, raw, append]) of
  {ok, Fd} ->
     {Date,_} = erlang:localtime(),
     {ok, #state{fd = Fd, file = LF, last_date=Date, daemon=Daemon}};
  Error ->
%          error_logger:tty(true),
      error_logger:error_report([{flog_file, LF}, Error]),
%          error_logger:tty(false),
      Error
    end.


chk_daemon() ->
   case {init:get_argument(noshell), init:get_argument(detached)} of
     {error, error} -> false;
     _ -> true
   end.

%%----------------------------------------------------------------------
%% Func: handle_event/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler
%%----------------------------------------------------------------------
handle_event({_,_,{_Pid,write,Rep}}, State) ->
%    F = fun({_Type,Rep}) -> Rep ;
%           (Rep) -> Rep end,
%    RepStrip = lists:map(F, Rep),
    file:write(State#state.fd, unicode:characters_to_binary(Rep, unicode)),
%    io:format("flog: file:write()=~p~n", [Rc]),
    printf(Rep, State#state.daemon),
    {ok, State};
handle_event(Event, State) ->
    write_event(State#state.fd, {now(), Event}),
    {ok, State}.

%%----------------------------------------------------------------------
%% Func: handle_call/2
%% Returns: {ok, Reply, State}                                |
%%          {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%          {remove_handler, Reply}
%%----------------------------------------------------------------------
handle_call({wrap_size, MaxLogSize}, State) ->
    case file:read_file_info(State#state.file) of
  {ok, FI} when  FI#file_info.size > MaxLogSize ->
      file:close(State#state.fd),
      rotate_log(State#state.file),
      case file:open(State#state.file, [write, raw, append]) of
    {ok, Fd} -> {ok, ok, State#state{fd = Fd}};
    Error    -> Error
      end;
  _ ->
      {ok, ok, State}
    end;

handle_call(_Request, State) ->
    Reply = {error, unexpected_call},
    {ok, Reply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler
%%----------------------------------------------------------------------
handle_info({'EXIT', _Fd, _Reason}, _State) ->
    remove_handler;
handle_info({emulator, _GL, reopen}, State) ->
    file:close(State#state.fd),
    rotate_log(State#state.file),
    case file:open(State#state.file, [write, raw, append]) of
  {ok, Fd} ->
     {ok, State#state{fd = Fd}};
  Error ->
      Error
    end;
handle_info({emulator, GL, Chars}, State) ->
    write_event(State#state.fd, {now(), {emulator, GL, Chars}}),
    {ok, State};
handle_info(_Info, State) ->
    {ok, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

reopen_log() ->
    error_logger ! {emulator, noproc, reopen}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

% Copied from erlang_logger_file_h.erl
write_event(Fd, {Time, {error, _GL, {Pid, Format, Args}}}) ->
    T = write_time(Time),
    case catch io_lib:format(add_node(Format,Pid), Args) of
  S when is_list(S) ->
      file:write(Fd, io_lib:format(T ++ S, []));
  _ ->
      F = add_node("e: ~p - ~p~n", Pid),
      file:write(Fd, io_lib:format(T ++ F, [Format,Args]))
    end;
write_event(Fd, {Time, {emulator, _GL, Chars}}) ->
    T = write_time(Time),
    case catch io_lib:format(Chars, []) of
  S when is_list(S) ->
      file:write(Fd, io_lib:format(T ++ S, []));
  _ ->
      file:write(Fd, io_lib:format(T ++ "e: ~w ~n", [Chars]))
    end;
write_event(Fd, {Time, {info, _GL, {Pid, Info, _}}}) ->
    T = write_time(Time),
    file:write(Fd, io_lib:format(T ++ add_node("~w~n",Pid), [Info]));
write_event(Fd, {Time, {error_report, _GL, {Pid, std_error, Rep}}}) ->
    T = write_time(Time),
    S = format_report(Rep),
    file:write(Fd, io_lib:format(T ++ S ++ add_node("", Pid), []));
write_event(Fd, {Time, {info_report, _GL, {Pid, std_info, Rep}}}) ->
    T = write_time(Time, "INFO"),
    S = format_report(Rep),
    file:write(Fd, io_lib:format(T ++ S ++ add_node("", Pid), []));
write_event(Fd, {Time, {info_report, _GL, {Pid, error, Rep}}}) ->
    T = write_time(Time, "ERROR"),
    S = format_report(Rep),
    Res = io_lib:format(T ++ S ++ add_node("", Pid), []),
    file:write(Fd, Res),
    T ++ S;
write_event(Fd, {Time, {info_report, _GL, {Pid, info, Rep}}}) ->
    T = write_time(Time, "INFO"),
    S = format_report(Rep),
    Res = io_lib:format(T ++ S ++ add_node("", Pid), []),
    file:write(Fd, Res),
    T ++ S;
write_event(Fd, {Time, {info_report, _GL, {Pid, debug, Rep}}}) ->
    T = write_time(Time, "DEBUG"),
    S = format_report(Rep),
    Res = io_lib:format(T ++ S ++ add_node("", Pid), []),
    file:write(Fd, Res),
    T ++ S;
write_event(Fd, {Time, {info_msg, _GL, {Pid, Format, Args}}}) ->
    T = write_time(Time, "INFO"),
    case catch io_lib:format(add_node(Format,Pid), Args) of
  S when is_list(S) ->
      file:write(Fd, io_lib:format(T ++ S, []));
  _ ->
      F = add_node("e: ~w - ~w~n", Pid),
      file:write(Fd, io_lib:format(T ++ F, [Format,Args]))
    end;
write_event(_, _) ->
    ok.


format_report(Rep) when is_list(Rep) ->
    case string_p(Rep) of
  true -> io_lib:format("~s~n",[Rep]);
  _    -> format_rep(Rep,"")
    end;
format_report(Rep) ->
    io_lib:format(" ~p~n",[Rep]).

format_rep([{Tag,Data}|Rep],Offset) ->
    io_lib:format("~s~p: ~p~n", [Offset,Tag,Data]) ++ format_rep(Rep,"    ");
format_rep([Other|Rep],Offset) ->
    io_lib:format("~s~p~n", [Offset,Other]) ++ format_rep(Rep,"    ");
format_rep(_,_) ->
    [].

add_node(X, Pid) when is_atom(X) ->
    add_node(atom_to_list(X), Pid);
add_node(X, Pid) when node(Pid) /= node() ->
    lists:concat([X,"** at node ",node(Pid)," **~n"]);
add_node(X, _) ->
    X.

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

write_time(Time) -> write_time(Time, "e").

write_time({_Mega,_Sec,Micro}=Now, Type) ->
    {{Y, Mo, D},{H, Mi, S}} = calendar:now_to_local_time(Now),
    io_lib:format("~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w.~.6.0w ~s ",
                  [Y, Mo, D, H, Mi, S, Micro, Type]).

%% Rename the log file if it the filename exists
%% This is needed in systems when the file must be closed before rotation (Windows).
%% On most Unix-like system, the file can be renamed from the command line and
%%the log can directly be reopened.
rotate_log(FilePath) ->
    {{YY,MMM,DD},{HH,MM,SS}} = calendar:now_to_local_time(now()),
    Path = filename:dirname(FilePath),
    File = filename:basename(FilePath),
    Old = ?FMT("~s/~4.10.0B-~2.10.0B-~2.10.0B_~2.10.0B-~2.10.0B-~2.10.0B_~s",
               [Path,YY,MMM,DD,HH,MM,SS,File]),
    file:rename(FilePath, Old),
    ok.


printf(S, false) ->
    Toks = string:tokens(lists:flatten(S), "\n"),
    lists:foreach( fun(X) -> io:format("~ts~n", [X]) end, Toks);
printf(_S, _) -> ok.

