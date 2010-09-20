-module(yaterl_logger_SUITE).

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
          % should support different log levels
          yaterl_log_levels
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TEST HELPERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
