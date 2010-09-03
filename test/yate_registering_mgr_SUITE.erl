-module(yate_registering_mgr_SUITE).

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
    yaterl_config:yate_registering_mgr(yate_registering_mgr),
    yaterl_config:yate_event_mgr(mockup_registering_yate_event_mgr),
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
    yaterl_config:yate_message_registering_configlist(
      [
       { "test.message", install, a_fake_module_handler, watch, [] },
       { "test2.message", install, a_fake_module_handler, watch, [another_fake_module] }
      ]),
    % 1) start event manager mockup:
    mockup_registering_yate_event_mgr:start_link(),    
    % 2) start registering manager:
    yate_registering_mgr:start_link(),
    % 3) do a fake start message registering call
    mockup_registering_yate_event_mgr:new_connection_available(),
    % 4) assert event manager mockup receive an outgoing registration message
    YateEvent1 = mockup_registering_yate_event_mgr:pop_outgoing_data(),
    true = yate_event:is_install(YateEvent1),
    "test.message" = yate_event:attr(name, YateEvent1),
    'REGISTERING' = ?GET_SRV_STATUS(yate_registering_mgr),
    % 5) send a fake registration message reply
    YateEventReply1 = #yate_event{
      direction=answer,
      type=install,
      attrs=[{name, "test.message"},{success, "true"}]
     },
    mockup_registering_yate_event_mgr:handle_yate_event(YateEventReply1),
    % 6) assert next registration message
    YateEvent2 = mockup_registering_yate_event_mgr:pop_outgoing_data(),
    true = yate_event:is_install(YateEvent1),
    "test2.message" = yate_event:attr(name, YateEvent2),
    'REGISTERING' = ?GET_SRV_STATUS(yate_registering_mgr),
    
    % 7) send next fake registration and last state on empty registration queue
    YateEventReply2 = #yate_event{
      direction=answer,
      type=install,
      attrs=[{name, "test2.message"},{success, "true"}]
     },
    mockup_registering_yate_event_mgr:handle_yate_event(YateEventReply2),

    test_server:sleep(2000),
    'COMPLETED' = ?GET_SRV_STATUS(yate_registering_mgr),
    ok.
