# clog & Flog

Удобная система логирования для OTP-приложений.

# Requirements (Что нужно)

* erlang
* rebar

# Example (Пример)

    clog:start_link(),
    flog:info("info"),
    flog:errro("info"),
    flog:debug("info"),


## TODO

    Разобраться с параметрами конфигупации