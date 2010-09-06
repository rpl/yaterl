-module(yate_subscribe_mgr_SUITE).

-compile(export_all).

-include_lib("ct.hrl").
-include("../include/yate.hrl").
-include("local_test_helpers.hrl").

%%--------------------------------------------------------------------
%% Function: suite() -> DefaultData
%% DefaultData: [tuple()]
%% Description: Require variables and set default values for the suite
%%--------------------------------------------------------------------
suite() -> [{timetrap,{seconds,10}}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config) -> Config
%% Config: [tuple()]
%% A list of key/value pairs, holding the test case configuration.
%% Description: Initiation before the whole suite
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    yaterl_config:yate_subscribe_mgr(yate_subscribe_mgr),
    yaterl_config:yate_incoming_event_srv(mockup_yate_incoming_event_srv),
    yaterl_config:yate_connection_mgr(mockup_yate_connection_mgr),
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config) -> _
%% Config: [tuple()]
%% A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after the whole suite
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

all() -> [
          new_connection_started_initialization_sequence
         ].
    

new_connection_started_initialization_sequence(_Config) ->
    yaterl_config:yate_message_subscribe_configlist(
      [
       { "test.message", install },
       { "test2.message", watch }
      ]),
    % 1) start event manager mockup:
    mockup_yate_connection_mgr:start_link(),    
    % 2) start subscribe manager:
    yate_subscribe_mgr:start_link(),
    % 3) do a fake start message subscribe call
    yate_subscribe_mgr:start_subscribe_sequence(),
    % 4) assert event manager mockup receive an outgoing registration message
    Data1 = mockup_yate_connection_mgr:pop_outgoing_data(),
    <<"%%>install::test.message">> = Data1,
    'SUBSCRIBE' = ?GET_SRV_STATUS(yate_subscribe_mgr),
    % 5) send a fake registration message reply
    YateEventReply1 = #yate_event{
      direction=answer,
      type=install,
      attrs=[{name, "test.message"},{success, "true"}]
     },
    yate_subscribe_mgr:handle_yate_event(YateEventReply1),
    % 6) assert next registration message
    Data2 = mockup_yate_connection_mgr:pop_outgoing_data(),
    <<"%%>watch:test2.message">> = Data2,
    'SUBSCRIBE' = ?GET_SRV_STATUS(yate_subscribe_mgr),
    
    % 7) send next fake registration and last state on empty registration queue
    YateEventReply2 = #yate_event{
      direction=answer,
      type=install,
      attrs=[{name, "test2.message"},{success, "true"}]
     },
    yate_subscribe_mgr:handle_yate_event(YateEventReply2),

    test_server:sleep(2000),
    'COMPLETED' = ?GET_SRV_STATUS(yate_subscribe_mgr),
    ok.
