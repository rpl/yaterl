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
    yaterl_config:yate_connection_mgr(yate_connection_mgr),
    yaterl_config:yate_incoming_event_srv(yate_incoming_event_srv),
    yaterl_config:yate_subscribe_mgr(yate_subscribe_mgr),
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
          % should support different log levels
          yaterl_log_levels,
          % should load a gen_yate_mod application environment and 
          %   yate_subscribe_mgr should resolve 
          configure_gen_yate_mod,
          % should start subscribing sequence on new connection available
          %   and configured
          message_subscribing_sequence,
          % should route subscribed message to gen_yate_mod callbacks
          message_routing,
          % should survive error deciding messages
          yate_decoding_errors,
          % should acknowledge install subscribed messages on processing errors
          acknowledge_on_processing_errors
         ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SPEC-1: configurable log levels %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

yaterl_log_levels(_Config) ->
    % configure requested log_level
    % register custom error_logger handler to catch logging events
    %   log_level=disabled => 

    DisabledLogLevel = disabled,
    % start error_logger_forwarder test helper module
    error_logger_forwarder:register(),
    % configure initial log level
    yaterl_config:log_level(DisabledLogLevel),
    % start yaterl_logger
    yaterl_logger:start_link(),

    % configured log_level and initial yaterl_logger log_level 
    % should be the same
    DisabledLogLevel = yaterl_config:log_level(),
    DisabledLogLevel = yaterl_logger:log_level(),

    % call yaterl_logger
    SendMsg = fun() ->
                      yaterl_logger:info_msg("Info Test"),
                      yaterl_logger:warning_msg("Warn Test"),
                      yaterl_logger:error_msg("Error Test")
              end,

    test_log_level(disabled, SendMsg, []),

    test_log_level(info, SendMsg, [
                                   {info_msg, "Info Test", []},
                                   {error, "Warn Test", []},
                                   {error, "Error Test", []}
                                  ]),

    test_log_level(warning, SendMsg, [
                                   {error, "Warn Test", []},
                                   {error, "Error Test", []}
                                  ]),
    
    test_log_level(error, SendMsg, [
                                    {error, "Error Test", []}
                                   ]),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SPEC-2: yate_subscribe_mgr should resolve gen_yate_mod handling %%%
%%%         as configured                                           %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

configure_gen_yate_mod(_Config) ->
    yaterl_config:yate_custom_module_config(
       {undefined, [{"call.execute", watch},
                    {"call.route", install, 80},
                    {"engine.status", install}]}
     ),

    yaterl_logger:start_link(),
    yate_subscribe_mgr:start_link(),
    
    FakeIncomingYateMessage1 = yate_message:new("call.execute"),
    watch = yate_subscribe_mgr:resolve_custom_module(FakeIncomingYateMessage1),

    FakeIncomingYateMessage2 = yate_message:new("call.route"),
    install = yate_subscribe_mgr:resolve_custom_module(FakeIncomingYateMessage2),
    
    FakeIncomingYateMessage3 = yate_message:new("engine.status"),
    install = yate_subscribe_mgr:resolve_custom_module(FakeIncomingYateMessage3),

    FakeIncomingYateMessage4 = yate_message:new("nonsubscribed.message"),
    unknown = yate_subscribe_mgr:resolve_custom_module(FakeIncomingYateMessage4),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SPEC-3: yate_subscribe_mgr should handle yate message subscribing %%%
%%%         as configured                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
message_subscribing_sequence(_Config) ->
    yaterl_config:yate_custom_module_config(
      {undefined, [{"call.execute", watch},
                   {"call.route", install, 80},
                   {"engine.status", install}]}
     ),
    
    start_yaterl_servers(),

    assert_yate_outgoing_data(<<"%%>watch:call.execute">>),
    yate_connection_forwarder:received_binary_data(<<"%%<watch:call.execute:true">>),
    assert_yate_outgoing_data(<<"%%>install:80:call.route">>),
    yate_connection_forwarder:received_binary_data(<<"%%<install:80:call.route:true">>),
    assert_yate_outgoing_data(<<"%%>install::engine.status">>),
    yate_connection_forwarder:received_binary_data(<<"%%<install::engine.status:true">>),

    ok.


assert_subscribe_sequence([H|T]) ->
%% [{"call.execute", watch},
%%  {"call.route", install, 80},
%%  {"engine.status", install}]
    


    assert_yate_outgoing_data(<<"%%>install:80:call.route">>),
    yate_connection_forwarder:received_binary_data(<<"%%<install:80:call.route:true">>),
    assert_yate_outgoing_data(<<"%%>install::engine.status">>),
    yate_connection_forwarder:received_binary_data(<<"%%<install::engine.status:true">>),

assert_subscribe_message({Name, watch}) ->
    YateEvent = yate_event:new(watch, [{name, Name}]),
    assert_yate_outgoing_data(yate_encode:to_binary(YateEvent)),
    BinReply = <<"%%<watch:", list_to_binary(Name)/binary, ":true">>,
    yate_connection_forwarder:received_binary_data(BinReply),
    ok;
assert_subscribe_message({Name, install, Priority}) ->
    YateEvent = yate_event:new(watch, [{name, Name}]),
    assert_yate_outgoing_data(yate_encode:to_binary(YateEvent)),
    BinReply = <<"%%<watch:", list_to_binary(Name)/binary, ":true">>,
    yate_connection_forwarder:received_binary_data(BinReply),
    ok;
assert_subscribe_message({Name, install}) ->
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SPEC-4: should route subscribed message to gen_yate_mod callbacks %%%
%%%         as configured                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       
message_routing(_Config) ->
    gen_yate_mod_forwarder:start_link(),
    gen_yate_mod_forwarder:register(),

    yaterl_config:yate_custom_module_config(
      {gen_yate_mod_forwarder, [{"call.execute", watch},
                   {"call.route", install, 80},
                   {"engine.status", install}]}
     ),

    start_yaterl_servers(),    

    yate_connection_forwarder:received_binary_data(<<"%%<watch:call.execute:true">>),
    yate_connection_forwarder:received_binary_data(<<"%%<install:80:call.execute:true">>),
    assert_yate_outgoing_data(<<"%%>install::engine.status">>),
    yate_connection_forwarder:received_binary_data(<<"%%<install::engine.status:true">>),
    _ = test_server:messages_get(),

    yate_connection_forwarder:received_binary_data(<<"%%>message:10:11:call.execute:11">>),
    assert_route_to_gen_yate_mod({watch, "call.execute"}),
    
    yate_connection_forwarder:received_binary_data(<<"%%>message:10:11:call.route:11">>),
    assert_route_to_gen_yate_mod({install, "call.route"}),

    yate_connection_forwarder:received_binary_data(<<"%%>message:10:11:engine.status:11">>),
    assert_route_to_gen_yate_mod({install, "engine.status"}),    

    ok.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SPEC-5: should survive error decoding messages                    %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       
yate_decoding_errors(_Config) ->
    gen_yate_mod_forwarder:start_link(),
    gen_yate_mod_forwarder:register(),

    yaterl_config:yate_custom_module_config(
      {gen_yate_mod_forwarder, [{"call.execute", watch},
                   {"call.route", install, 80},
                   {"engine.status", install}]}
     ),

    start_yaterl_servers(),        

    yate_connection_forwarder:received_binary_data(<<"%%<watch:call.execute:true">>),
    yate_connection_forwarder:received_binary_data(<<"%%<install:80:call.route:true">>),
    assert_yate_outgoing_data(<<"%%>install::engine.status">>),
    yate_connection_forwarder:received_binary_data(<<"%%<install::engine.status:true">>),
    _ = test_server:messages_get(),

    yate_connection_forwarder:received_binary_data(<<"%%>messe:10:11:call.execute:11">>),

    %%% NOTE: if any server started with start_link will crash this test fails
    test_server:sleep(500),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SPEC-6: should acknowledge install subscribed messages on processing errors %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

acknowledge_on_processing_errors(_Config) ->
    yaterl_config:yate_custom_module_config(
      {undefined, [{"call.execute", watch},
                   {"call.route", install, 80},
                   {"engine.status", install}]}
     ),

    start_yaterl_servers(),            

    yate_connection_forwarder:received_binary_data(<<"%%<watch:call.execute:true">>),
    yate_connection_forwarder:received_binary_data(<<"%%<install:80:call.route:true">>),
    assert_yate_outgoing_data(<<"%%>install::engine.status">>),
    yate_connection_forwarder:received_binary_data(<<"%%<install::engine.status:true">>),
    _ = test_server:messages_get(),

    Msg1 = <<"%%>message:10:11:call.route:11">>,
    AckMsg1 = yate_message:reply(yate_decode:from_binary(Msg1)),

    yate_connection_forwarder:received_binary_data(Msg1),
    assert_yate_outgoing_data(yate_encode:to_binary(AckMsg1)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TEST HELPERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_yaterl_servers() ->
    yaterl_logger:start_link(),
    yate_subscribe_mgr:start_link(),
    yate_connection_mgr:start_link(),

    yate_connection_forwarder:start_link(),
    yate_connection_forwarder:register(),
    yate_connection_forwarder:connect_to(yate_connection_mgr).
    

assert_yate_outgoing_data(Data) ->
    ct:pal("YATE OUTGOING DATA (Expect: ~p~n", [Data]),
    receive Data ->
            ok
    after 500 ->
            ct:pal("RECEIVED MESSAGES: ~p~n", [test_server:messages_get()]),
            ct:fail(expected_data_never_received)
    end.

assert_route_to_gen_yate_mod({CallbackType, MessageName}) ->
    receive {CallbackType, YateMessage} ->
            case yate_message:name(YateMessage) of
                MessageName -> ok;
                _ -> ct:fail(unexpected_yate_message)
            end
    after 500 ->
            ct:fail(expected_gen_yate_mod_callback_never_called)
    end.

test_log_level(Level, SendEventsFun, ExpectedLoggerEvents) ->
    yaterl_logger:log_level(Level),
    Level = yaterl_logger:log_level(),
    SendEventsFun(),    
    expected_error_logger_events(ExpectedLoggerEvents).
        

expected_error_logger_events([]) ->
    receive Any ->
            ct:pal("Unexpected: ~p", [Any]),
            ct:fail(unexpected_error_logger_events)
    after 500 ->
            ok
    end;
expected_error_logger_events({Level, Msg, DataList}) ->
    receive {Level, _, { _, Msg, DataList}} ->
            ok
    after 500 ->
            ct:fail(expected_logger_event_never_received)
    end;
expected_error_logger_events([H]) ->
    expected_error_logger_events(H);
expected_error_logger_events([H|T]) ->            
    expected_error_logger_events(H),
    expected_error_logger_events(T).
