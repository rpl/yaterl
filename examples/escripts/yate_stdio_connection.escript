#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname yate_stdio_connection -noinput -pa /media/WORKS/ALCA/ERLANG/YATE/yaterl/ebin/ -config /media/WORKS/ALCA/ERLANG/YATE/yaterl/test_stdio
-module(yate_stdio_connection).

-export([main/1]).

main(_) ->
    error_logger:tty(false),
    error_logger:logfile({open, "/tmp/stdio.logfile"}),
    application:start(sasl),

    application:start(yaterl),
    timer:sleep(infinity).

