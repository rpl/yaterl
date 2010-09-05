-module(gen_yate_mod).

-export([
         behaviour_info/1,

         send_yate_message/1,
         reply_yate_message/1,
         ack_yate_message/1
        ]).


%%====================================================================
%% Behaviour Definition Callback
%%====================================================================

behaviour_info(callbacks) ->
    [{received_watch_message, 1},
     {received_install_message, 1}];
behaviour_info(_Other) ->
    undefined.

%%====================================================================
%% API
%%====================================================================

send_yate_message(YateMessage) ->
    YateConnMgr = yaterl_config:yate_connection_mgr(),
    YateConnMgr:send_binary_data(yate_encode:to_binary(YateMessage)).

reply_yate_message(YateMessage) ->
    YateConnMgr = yaterl_config:yate_connection_mgr(),
    YateConnMgr:send_binary_data(yate_encode:to_binary(YateMessage)).

ack_yate_message(YateMessage) ->
    YateConnMgr = yaterl_config:yate_connection_mgr(),
    YateConnMgr:send_binary_data(yate_encode:to_binary(YateMessage)).
    
