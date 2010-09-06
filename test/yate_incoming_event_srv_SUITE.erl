-module(yate_incoming_event_srv_SUITE).

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
    yaterl_config:yate_incoming_event_srv(yate_incoming_event_srv),
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
          handle_subscribe_answer_events,
          handle_message
         ].
    

handle_subscribe_answer_events(_Config) ->
    mockup_yate_subscribe_mgr:start_link(),
    Data1 = <<"%%<install:100:call.route:true">>,
    YateEvent1 = yate_decode:from_binary(Data1),
    {ok, Pid1} = yate_incoming_event_srv:start(Data1),
    yate_incoming_event_srv:run(Pid1),
    mockup_yate_subscribe_mgr:wait_for_event(YateEvent1),
    ok.

handle_message(_Config) ->
    mockup_yate_subscribe_mgr:start_link(),
    Data1 = <<"%%>message:100:112:call.execute:1111">>,
    {ok, Pid1} = yate_incoming_event_srv:start(Data1),
    yate_incoming_event_srv:run(Pid1),
    test_server:sleep(3000),
    ok. %ct:fail(testing).
