-module(mockup_gen_yate_mod).

-export([
         handle_install_message/1,
         handle_watch_message/1
        ]).

-include("../include/yate.hrl").

handle_install_message(YateEvent) ->
    yate_message:reply(YateEvent).

handle_watch_message(_YateEvent) ->
    ok.
