-module(yate_test_route).

-behaviour(gen_yate_mod).

-export([
         handle_install_message/1,
         main/1
        ]).

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
    gen_yate_mod:ack_yate_messge(YateMessage).
    

route_to_dial(YateMessage) ->
    YateReply = yate_message:retvalue("tone/busy",YateMessage),
    gen_yate_mod:reply_to_yate_message(YateReply).

main(_) ->
    error_logger:tty(false),
    error_logger:logfile({open, "/tmp/yaterl_route_service.logfile"}),
    application:start(sasl),
    application:start(yaterl),
    application:start(yaterl_route_service),
    timer:sleep(infinity).
