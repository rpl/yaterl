-module(mockup_gen_yate_mod).

-export([
         handle_install_message/1,
         handle_watch_message/1
        ]).

-include("../include/yate.hrl").

handle_install_message(YateEvent) ->
    YateEvent1=YateEvent#yate_event{direction=answer},
    yate_message:set_processed(YateEvent1).

handle_watch_message(_YateEvent) ->
    ok.
