#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname yate_stdio_connection -noinput -pa /media/WORKS/ALCA/ERLANG/YATE/yaterl/ebin/ -config test_stdio
-module(yate_stdio_connection).

-export([main/1]).

main(_) ->
    error_logger:tty(false),
    error_logger:logfile({open, "/tmp/stdio.logfile"}),
    application:start(sasl),
    yaterl_config:yaterl_sup_mode(stdio_connection_only),
    yaterl_config:remote_yate_connection_mgr({"yate_connection_manager", 
                                       "sheldon"}),
    application:start(yaterl),
    timer:sleep(infinity).

