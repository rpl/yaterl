-module(yate_test_route).

-behaviour(yaterl_gen_mod).

-export([
         connection_available/0,
         subscribe_config/0,
         subscribe_error/2,
         handle_install_message/1,
         main/1
        ]).

connection_available() ->
    start_subscribe_sequence.

subscribe_config() ->
    error_logger:error_msg("SUBSCRIBE CONFIG"),
    [{"call.route", install, "80"}].
%     {"call.route", install, 80, {filters, [{"module", "conference"}]}}].

subscribe_error(_LastResponse, _LastRequest) ->
    error_logger:error_msg("SUBSCRIBE ERROR... EXITING"),
    timer:sleep(10000),
    erlang:halt(1).


handle_install_message(YateMessage) ->
    case {yate_message:name(YateMessage),
          yate_event:direction(YateMessage),
          yate_message:param(called, YateMessage)} of
        {"call.route", incoming, "123"} ->
            route_to_dial(YateMessage);
        _AnyOther -> log(YateMessage)
    end.


log(YateMessage) ->
    error_logger:info_msg("IGNORED Msg: ~p~n", [YateMessage]),
    yaterl_gen_mod:ack(YateMessage).
    

route_to_dial(YateMessage) ->
    YateReply = yate_message:retvalue("tone/busy",YateMessage),
    yaterl_gen_mod:reply(YateReply).

main(_) ->
    error_logger:tty(false),
    error_logger:logfile({open, "/tmp/yaterl_route_service.logfile"}),
    application:start(sasl),
    application:start(yaterl),
    application:start(yaterl_route_service),
    timer:sleep(infinity).
