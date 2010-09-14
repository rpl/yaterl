-module(yaterl_gen_mod).

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
    Data = yate_encode:to_binary(YateMessage),
    yaterl_connection_mgr:send_binary_data(Data).

reply_to_yate_message(YateMessage) ->
    Reply = yate_message:reply(YateMessage, true),
    _Data = yate_encode:to_binary(Reply).

ack_yate_message(YateMessage) ->
    Reply = yate_message:reply(YateMessage, false),
    _Data = yate_encode:to_binary(Reply).

