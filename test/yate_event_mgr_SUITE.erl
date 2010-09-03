-module(yate_event_mgr_SUITE).

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
    yaterl_config:yate_event_mgr(yate_event_mgr),
    yaterl_config:yate_connection_mgr(mockup_yate_connection_mgr),
    yaterl_config:yate_registering_mgr(mockup_yate_registering_mgr),
    yaterl_config:yate_incoming_event_srv(
      mockup_yate_incoming_event_srv
     ),
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
          new_connection_started_initialization_sequence,
          send_yate_event,
          receive_yate_event
         ].
    
new_connection_started_initialization_sequence(_Config) ->
    % 1) start event manager and event registering mgr mockup
    yate_event_mgr:start_link(),
    mockup_yate_registering_mgr:start_link(),
    % 2) start mockup yate connection mgr
    mockup_yate_connection_mgr:start_link(),
    mockup_yate_connection_mgr:notify_new_connection_available(),
    % assert start_message_registering called on yate_registering_mgr mockup
    true = mockup_yate_registering_mgr:is_start_message_registering_called(),
    ok.

send_yate_event(_Config) ->
    % 1) start event manager
    yate_event_mgr:start_link(),
    % 2) start mockup yate connection mgr
    mockup_yate_connection_mgr:start_link(),
    % 3) send a yate event
    YateEvent = "FakeYateEvent",
    yate_event_mgr:send_yate_event(YateEvent),
    YateEvent = mockup_yate_connection_mgr:pop_outgoing_data(),
    ok.

receive_yate_event(_Config) ->
    % 1) start event manager 
    yate_event_mgr:start_link(),
    % 2) start mockup yate connection mgr
    mockup_yate_incoming_event_srv:mockup_start_link(),
    mockup_yate_connection_mgr:start_link(),
    % 3) receive a yate event
    YateEvent = "FakeYateEvent",
    mockup_yate_connection_mgr:fake_received_yate_event(YateEvent),
    true = mockup_yate_incoming_event_srv:is_spawned(YateEvent),
    ok.
