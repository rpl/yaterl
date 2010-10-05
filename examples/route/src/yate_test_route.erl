-module(yate_test_route).

-behaviour(yaterl_gen_mod).

-compile(export_all).

-export([
         main/1,
         connection_available/0,
         subscribe_completed/0,
         subscribe_error/2,
         handle_install_message/1
        ]).

config() ->
    LogDir = "/tmp/",
    BaseLogFileName = [LogDir,atom_to_list(?MODULE),"_",os:getpid()],
    yaterl_config:log_files(BaseLogFileName++".log", 
                            BaseLogFileName++"_sasl.log"),
    yaterl_config:yaterl_custom_module_name(?MODULE),
    ok.

main(_) ->
    ok = config(),
    ok = application:start(sasl),
    ok = application:start(yaterl),
    timer:sleep(infinity).

connection_available() ->
    ConfigList = [{"call.route", install, 80}],
    yaterl_gen_mod:start_subscribe_sequence(ConfigList).

subscribe_completed() ->
    error_logger:info_msg("SUBSCRIBING COMPLETED").

subscribe_error(_LastResponse, _LastRequest) ->
    error_logger:error_msg("SUBSCRIBE ERROR... EXITING"),
    init:stop(1).

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

