-module(yaterl_config).

-export([
         log_level/0,
         log_level/1,

         whereis_yate_connection_mgr/0,
         whereis_yate_connection_mgr/1,

         yate_connection_maxbytesline/0,
         yate_connection_maxbytesline/1,

         yaterl_sup_mode/0,
         yaterl_sup_mode/1,

         yate_custom_module_config/0,
         yate_custom_module_config/1,

         yate_custom_module_name/0,
         yate_custom_module_name/1,

         yate_message_subscribe_configlist/0,
         yate_message_subscribe_configlist/1,

         get_key/2,
         set_key/2
        ]).

log_level() ->
    get_key(log_level, error).

log_level(Value) ->
    set_key(log_level, Value).

whereis_yate_connection_mgr() ->
    {NodeName, HostName} = get_key(whereis_yate_connection_mgr, {self, localhost}),
    RealNodeName = case {NodeName,
                         is_list(NodeName)} of
                       {self, false} -> [H | _T ] = string:tokens(
                                                      atom_to_list(node()), "@"
                                                     ),
                                        H;
                       {CustomNodeName, true} -> CustomNodeName
                   end,
    RealHostName = case {HostName,
                         is_list(HostName)} of
                       {localhost, false} -> net_adm:localhost();
                       {CustomHostName, true} -> CustomHostName
                   end,
    {RealNodeName, RealHostName}.

whereis_yate_connection_mgr(Value) ->
    set_key(whereis_yate_connection_mgr, Value).

yate_connection_maxbytesline() ->
    get_key(yate_connection_maxbytesline, 80000).

yate_connection_maxbytesline(Value) ->
    set_key(yate_connection_maxbytesline, Value).

yaterl_sup_mode() ->
    get_key(yaterl_sup_mode, all_in_one).

yaterl_sup_mode(Mode) ->
    set_key(yaterl_sup_mode, Mode).

yate_custom_module_config() ->
    get_key(yate_custom_module_config, {undefined, []}).

yate_custom_module_config(Value) ->
    set_key(yate_custom_module_config, Value).

yate_custom_module_name() ->
    {CustomModuleName, _ConfigList} = yate_custom_module_config(),
    CustomModuleName.

yate_custom_module_name(Value) ->
    {_CustomModuleName, ConfigList} = yate_custom_module_config(),
    yate_custom_module_config({Value, ConfigList}).

yate_message_subscribe_configlist() ->
    {_CustomModuleName, ConfigList} = yate_custom_module_config(),
    ConfigList.
    
yate_message_subscribe_configlist(Value) ->
    %% [{"message.name", install, ModuleName,
    %%                   watch, [ModuleList]}]
    %%
    %%  ModuleName not_memeber_of ModuleList
    {CustomModuleName, _ConfigList} = yate_custom_module_config(),
    yate_custom_module_config({CustomModuleName, Value}).



get_key(Key, Default) ->
    case application:get_env(yaterl, Key) of
        undefined -> Default;
        {ok, CustomValue} -> CustomValue
    end.

set_key(Key, Value) ->
    application:set_env(yaterl, Key, Value).
