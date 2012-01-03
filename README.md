# clog & flog

Удобная система логирования для OTP-приложений.

# Requirements (Что нужно)

* erlang
* rebar

# Example (Пример)

    clog:start_link(),
    flog:info("info"),
    flog:error("error"),
    flog:debug("debug").


## TODO

Разобраться с параметрами конфигупации