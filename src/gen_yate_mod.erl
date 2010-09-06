-module(gen_yate_mod).

-export([
         behaviour_info/1,

         send_yate_message/1,
         reply_to_yate_message/1,
         ack_yate_message/1
        ]).


%%====================================================================
%% Behaviour Definition Callback
%%====================================================================

behaviour_info(callbacks) ->
    [{handle_watch_message, 1},
     {handle_install_message, 1}];
behaviour_info(_Other) ->
    undefined.

%%====================================================================
%% API
%%====================================================================

send_yate_message(YateMessage) ->
    YateConnMgr = yaterl_config:yate_connection_mgr(),
    Data = yate_encode:to_binary(YateMessage),
    YateConnMgr:send_binary_data(Data).

reply_to_yate_message(YateMessage) ->
    YateConnMgr = yaterl_config:yate_connection_mgr(),
    Reply = yate_message:reply(YateMessage, true),
    Data = yate_encode:to_binary(Reply),
    YateConnMgr:send_binary_data(Data).

ack_yate_message(YateMessage) ->
    YateConnMgr = yaterl_config:yate_connection_mgr(),
    Reply = yate_message:reply(YateMessage, false),
    Data = yate_encode:to_binary(Reply),
    YateConnMgr:send_binary_data(Data).

