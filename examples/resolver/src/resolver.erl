-module(resolver).

-behaviour(yaterl_gen_mod).

-compile(export_all).

-export([
         connection_available/0,
         subscribe_completed/0,
         subscribe_error/2,
         handle_install_message/1,
         main/1
        ]).

-include("resolver.hrl").

connection_available() ->
    ConfigList = [{setlocal, "restart", "true"},
                  {"call.execute", install, "80"},
                  {"chan.hangup", install, "120", {filters, [{"cause_sip", "408"}]}}],
    yaterl_gen_mod:start_subscribe_sequence(ConfigList).

subscribe_completed() ->
    error_logger:info_msg("SUBSCRIBING COMPLETED").

subscribe_error(_LastResponse, _LastRequest) ->
    error_logger:error_msg("SUBSCRIBE ERROR... EXITING"),
    init:stop(1).


handle_install_message(YateMessage) ->
    incoming = yate_event:direction(YateMessage),
    Name = yate_message:name(YateMessage),
    process_message(Name, YateMessage).

handle_watch_message(_YateMessage) ->
    %% NOTE: return value ignored
    ok.

log(YateMessage) ->
    error_logger:info_msg("IGNORED Msg: ~p~n", [YateMessage]),
    yaterl_gen_mod:ack(YateMessage).

%%% NOTE: escript entry point    
main(_) ->
    config(),
    ok = application:start(sasl),
    ok = application:start(yaterl),
    timer:sleep(infinity).

%%% NOTE: logging and yaterl configurations
config() ->
    LogDir = "/tmp/",
    BaseLogFileName = [LogDir,atom_to_list(?MODULE),"_",os:getpid()],
    yaterl_config:log_level(error),
    yaterl_config:log_files(BaseLogFileName++".log", 
                            BaseLogFileName++"_sasl.log"),

    yaterl_config:yaterl_custom_module_name(?MODULE),
    ok.

%%%%%%%%% INTERNALS

 -include_lib("kernel/include/inet.hrl").

process_message("call.execute", YateMessage) ->
    CallTo = yate_message:param(callto, YateMessage),
    case resolved_callto(CallTo) of
        {ok, Value} -> 
            error_logger:info_msg("MATCHED (new value: ~p)~n", [Value]),
            YateMessage2 = yate_message:param(callto, Value, YateMessage),
            yaterl_gen_mod:ack(YateMessage2);
        {error, nomatch} -> 
            error_logger:info_msg("NOMATCH on ~p~n", [YateMessage]),
            yaterl_gen_mod:ack(YateMessage)
    end.

resolved_callto(CallTo) ->
    error_logger:info_msg("PROCESSING CALLTO: ~p~n", [CallTo]),
    case re:run(CallTo, ?CALLTO_REGEXP, [global,{capture,all_but_first,list}]) of
        nomatch -> 
            {error, nomatch};
        {match, [["sip/", Part2, Part3, Host_or_Ip, Part5]]} -> 
            {ok, lists:flatten(["sip/", Part2, Part3, do_resolve(Host_or_Ip), Part5])}; 
        {match, _ } ->
            {error, nomatch}
    end.

do_resolve(Host_or_Ip) ->
    {ok, Res} = inet:gethostbyname(Host_or_Ip),
    [{I1,I2,I3,I4}|_Rest] = Res#hostent.h_addr_list,
    io_lib:format("~w.~w.~w.~w", [I1,I2,I3,I4]).
