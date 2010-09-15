-module(yaterl_servers_SUITE).

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
          % should load a gen_yate_mod application environment and 
          %   yate_subscribe_mgr should resolve 
          configure_yaterl_gen_mod,
          % should start subscribing sequence on new connection available
          %   and configured
          message_subscribing_errors,
          message_subscribing_sequence,
          % should route subscribed message to gen_yate_mod callbacks
          message_routing,
          % should survive error deciding messages
          yate_decoding_errors,
          % should acknowledge install subscribed messages on processing errors
          acknowledge_on_processing_errors
         ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SPEC-2: yaterl_subscribe_mgr should resolve yaterl_gen_mod handling %%%
%%%         as configured                                               %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

configure_yaterl_gen_mod(_Config) ->
    yaterl_config:yaterl_custom_module_config(
       {undefined, [{"call.execute", watch},
                    {"call.route", install, "80"},
                    {"engine.status", install}]}
     ),

    yaterl_logger:start_link(),
    yaterl_subscribe_mgr:start_link(),
    
    FakeIncomingYateMessage1 = yate_message:new("call.execute"),
    watch = yaterl_subscribe_mgr:resolve_custom_module(FakeIncomingYateMessage1),

    FakeIncomingYateMessage2 = yate_message:new("call.route"),
    install = yaterl_subscribe_mgr:resolve_custom_module(FakeIncomingYateMessage2),
    
    FakeIncomingYateMessage3 = yate_message:new("engine.status"),
    install = yaterl_subscribe_mgr:resolve_custom_module(FakeIncomingYateMessage3),

    FakeIncomingYateMessage4 = yate_message:new("nonsubscribed.message"),
    unknown = yaterl_subscribe_mgr:resolve_custom_module(FakeIncomingYateMessage4),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SPEC-3: yaterl_subscribe_mgr should handle yate message subscribing %%%
%%%         as configured                                               %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

message_subscribing_errors(_Config) ->
    SubscribeConfigList = [{"call.execute", watch},
                   {"call.route", install, "80"},
                   {"engine.status", install}],
    
    yaterl_config:yaterl_custom_module_config(
      {undefined, SubscribeConfigList}
     ),
    
    start_yaterl_servers(),

    process_flag(trap_exit, true),

    Name = "call.execute",
    YateEvent = yate_event:new(watch, [{name, Name}]),
    assert_yate_outgoing_data(yate_encode:to_binary(YateEvent)),
    Reply = io_lib:format("%%<watch:~s:false", [Name]),
    BinReply = list_to_binary(Reply),
    yaterl_connection_forwarder:received_binary_data(BinReply),
    
    %%% yaterl_subscribe_mgr die on subscribe errors
    receive {'EXIT', _Pid, Reason} ->
            ct:pal("yaterl_subscribe_mgr EXIT WITH: ~p~n", [Reason])
    end,

    ok.

    
message_subscribing_sequence(_Config) ->
    SubscribeConfigList = [{"call.execute", watch},
                   {"call.route", install, "80"},
                   {"engine.status", install}],
    
    yaterl_config:yaterl_custom_module_config(
      {undefined, SubscribeConfigList}
     ),
    
    start_yaterl_servers(),

    assert_subscribe_sequence(SubscribeConfigList),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SPEC-4: should route subscribed message to gen_yate_mod callbacks %%%
%%%         as configured                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       
message_routing(_Config) ->
    yaterl_gen_mod_forwarder:start_link(),
    yaterl_gen_mod_forwarder:register(),

    SubscribeConfigList = [{"call.execute", watch},
                   {"call.route", install, "80"},
                   {"engine.status", install}],

    yaterl_config:yaterl_custom_module_config(
      {yaterl_gen_mod_forwarder, SubscribeConfigList}
     ),

    start_yaterl_servers(),    

    assert_subscribe_sequence(SubscribeConfigList),

    yaterl_connection_forwarder:received_binary_data(<<"%%>message:10:11:call.execute:11">>),
    assert_route_to_yaterl_gen_mod({watch, "call.execute"}),
    
    yaterl_connection_forwarder:received_binary_data(<<"%%>message:10:11:call.route:11">>),
    assert_route_to_yaterl_gen_mod({install, "call.route"}),

    yaterl_connection_forwarder:received_binary_data(<<"%%>message:10:11:engine.status:11">>),
    assert_route_to_yaterl_gen_mod({install, "engine.status"}),    

    ok.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SPEC-5: should survive error decoding messages                    %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       
yate_decoding_errors(_Config) ->
    yaterl_gen_mod_forwarder:start_link(),
    yaterl_gen_mod_forwarder:register(),

    SubscribeConfigList = [{"call.execute", watch},
                   {"call.route", install, "80"},
                   {"engine.status", install}],

    yaterl_config:yaterl_custom_module_config(
      {yaterl_gen_mod_forwarder, SubscribeConfigList}
     ),

    start_yaterl_servers(),        

    assert_subscribe_sequence(SubscribeConfigList),

    yaterl_connection_forwarder:received_binary_data(<<"%%>messe:10:11:call.execute:11">>),

    %%% NOTE: if any server started with start_link will crash this test fails
    test_server:sleep(500),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SPEC-6: should acknowledge install subscribed messages on processing errors %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

acknowledge_on_processing_errors(_Config) ->
    SubscribeConfigList = [{"call.execute", watch},
                   {"call.route", install, "80"},
                   {"engine.status", install}],

    yaterl_config:yaterl_custom_module_config(
      {undefined, SubscribeConfigList}
     ),

    start_yaterl_servers(),            

    assert_subscribe_sequence(SubscribeConfigList),

    Msg1 = <<"%%>message:10:11:call.route:11">>,
    AckMsg1 = yate_message:reply(yate_decode:from_binary(Msg1)),

    yaterl_connection_forwarder:received_binary_data(Msg1),
    assert_yate_outgoing_data(yate_encode:to_binary(AckMsg1)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TEST HELPERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_yaterl_servers() ->
    yaterl_logger:start_link(),
    yaterl_subscribe_mgr:start_link(),
    yaterl_connection_mgr:start_link(),

    yaterl_connection_forwarder:start_link(),
    yaterl_connection_forwarder:register(),
    yaterl_connection_forwarder:connect_to(yaterl_connection_mgr).
    

assert_yate_outgoing_data(Data) ->
    ct:pal("YATE OUTGOING DATA (Expect: ~p~n", [Data]),
    receive Data ->
            ok
    after 500 ->
            ct:pal("RECEIVED MESSAGES: ~p~n", [test_server:messages_get()]),
            ct:fail(expected_data_never_received)
    end.

assert_route_to_yaterl_gen_mod({CallbackType, MessageName}) ->
    receive {CallbackType, YateMessage} ->
            case yate_message:name(YateMessage) of
                MessageName -> ok;
                _ -> ct:fail(unexpected_yate_message)
            end
    after 500 ->
            ct:fail(expected_gen_yate_mod_callback_never_called)
    end.

assert_subscribe_sequence([]) ->
    ok;
assert_subscribe_sequence([H|T]) ->
    assert_subscribe_message(H),
    assert_subscribe_sequence(T).

assert_subscribe_message({Name, watch}) ->
    YateEvent = yate_event:new(watch, [{name, Name}]),
    assert_yate_outgoing_data(yate_encode:to_binary(YateEvent)),
    Reply = io_lib:format("%%<watch:~s:true", [Name]),
    BinReply = list_to_binary(Reply),
    yaterl_connection_forwarder:received_binary_data(BinReply),
    ok;
assert_subscribe_message({Name, install, Priority}) ->
    YateEvent = yate_event:new(install, [{name, Name},{priority, Priority}]),
    assert_yate_outgoing_data(yate_encode:to_binary(YateEvent)),
    Reply = io_lib:format("%%<install:~s:~s:true", [Priority,Name]),
    BinReply = list_to_binary(Reply),
    yaterl_connection_forwarder:received_binary_data(BinReply),
    ok;
assert_subscribe_message({Name, install}) ->
    YateEvent = yate_event:new(install, [{name, Name}]),
    assert_yate_outgoing_data(yate_encode:to_binary(YateEvent)),
    Reply = io_lib:format("%%<install::~s:true", [Name]),
    BinReply = list_to_binary(Reply),
    yaterl_connection_forwarder:received_binary_data(BinReply),
    ok.
