-module(yate_connection_mgr_SUITE).

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
    yaterl_config:yate_incoming_event_srv(mockup_yate_incoming_event_srv),
    yaterl_config:yate_subscribe_mgr(mockup_yate_subscribe_mgr),
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
          send_binary_data,
          receive_binary_data
         ].
    

new_connection_started_initialization_sequence(_Config) ->
    % 1) start connection manager:
    %
    %     yate_connection_mgr:start_link -> gen_server:start_link
    %     gen_server:start_link -> yate_connection_mgr:init
    yate_connection_mgr:start_link(),
    mockup_yate_subscribe_mgr:start_link(),
    % assert unconnected manager
    false = yate_connection_mgr:is_connected(),
    {ok, undefined} = yate_connection_mgr:get_yate_connection(),
    % 2) start a fake yate connection server:
    %   yate_connection:start_link -> gen_server:start_link
    %   gen_server:start_link -> yate_connection:init
    %   yate_connection:init -> yate_connection_mgr:set_yate_connection
    mockup_yate_connection:start_link(),
    %      assert successful yate connection registering
    true = yate_connection_mgr:is_connected(),
    {ok, {local, mockup_yate_connection}} = yate_connection_mgr:get_yate_connection(),
    %      test connection manager call new_connection_available on the mockup 
    %      yate event manager
    true = mockup_yate_subscribe_mgr:is_start_subscribe_sequence_called(),
    ok.

send_binary_data(_Config) ->
    % 1) start connection manager
    %    set yate_event_manager_module to the fake event manager server
    yate_connection_mgr:start_link(),
    mockup_yate_subscribe_mgr:start_link(),
    % 2) start a fake yate connection server
    mockup_yate_connection:start_link(),
    % 3) send yate event from the connection manager
    YateEvent = yate_event:new(watch, [{name, "test.event"}]),
    yate_connection_mgr:send_binary_data(yate_encode:to_binary(YateEvent)),
    %    and test received data from fake yate connection server
    <<"%%>watch:test.event">> = mockup_yate_connection:pop_outgoing_data(),
    ok.
    
receive_binary_data(_Config) ->
    % 1) start connection manager
    %      set yate_event_manager_module to the fake event manager server
    yate_connection_mgr:start_link(),
    mockup_yate_subscribe_mgr:start_link(),
    % 2) start a fake event manager server
   mockup_yate_incoming_event_srv:mockup_start_link(),
    % 3) start a fake yate connection server
    mockup_yate_connection:start_link(),
    % 4) receive fake yate event from the fake connection server
    mockup_yate_connection:received_binary_data(<<"%%<watch:test.event:true">>),
    %      test decoded yate event received from the fake yate event manager
    true = mockup_yate_incoming_event_srv:
        is_spawned_and_run_called(<<"%%<watch:test.event:true">>),
    ok.
