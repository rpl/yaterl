-module(yaterl_config).

-export([
         yate_connection_mgr/0,
         yate_connection_mgr/1,

         yate_event_mgr/0,
         yate_event_mgr/1,

         yate_registering_mgr/0,
         yate_registering_mgr/1,

         yate_incoming_event_processing_srv/0,
         yate_incoming_event_processing_srv/1,

         yate_message_registering_configlist/0,
         yate_message_registering_configlist/1,

         get_key/2,
         set_key/2
        ]).

yate_connection_mgr() ->
    get_key(yate_connection_mgr, yate_connection_mgr).

yate_connection_mgr(Value) ->
    set_key(yate_connection_mgr, Value).

yate_event_mgr() ->
    get_key(yate_event_mgr, yate_event_mgr).

yate_event_mgr(Value) ->
    set_key(yate_event_mgr, Value).

yate_registering_mgr() ->
    get_key(yate_registering_mgr, yate_registering_mgr).

yate_registering_mgr(Value) ->
    set_key(yate_registering_mgr, Value).

yate_incoming_event_processing_srv() ->
    get_key(yate_incoming_event_processing_srv, yate_incoming_event_processing_srv).

yate_incoming_event_processing_srv(Value) ->
    set_key(yate_incoming_event_processing_srv, Value).

yate_message_registering_configlist() ->
    get_key(yate_message_registering_configlist, []).
    
yate_message_registering_configlist(Value) ->
    %% [{"message.name", install, ModuleName,
    %%                   watch, [ModuleList]}]
    %%
    %%  ModuleName not_memeber_of ModuleList
    set_key(yate_message_registering_configlist, Value).

get_key(Key, Default) ->
    case application:get_env(yaterl, Key) of
        undefined -> Default;
        {ok, CustomValue} -> CustomValue
    end.

set_key(Key, Value) ->
    application:set_env(yaterl, Key, Value).
