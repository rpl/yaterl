#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname yate_connection_manager -pa /media/WORKS/ALCA/ERLANG/YATE/yaterl/ebin/ -noinput -config test_manager
-module(yate_run_manager).

-export([main/1]).

main(_) ->
  error_logger:tty(false),
  error_logger:logfile({open, "/tmp/manager.logfile"}),
  application:start(sasl),
  application:start(yaterl),
  timer:sleep(infinity).

