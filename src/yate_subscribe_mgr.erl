%%%-------------------------------------------------------------------
%%% File    : yaterl_registering_mgr.erl
%%% Author  : rpl <>
%%% Description : 
%%%
%%% Created :  2 Sep 2010 by rpl <>
%%%-------------------------------------------------------------------
-module(yate_subscribe_mgr).

-behaviour(gen_fsm).

%% API
-export([
         start_link/0,
         start_subscribe_sequence/0,
         handle_yate_event/1,
         resolve_custom_module/1
        ]).

%% gen_fsm callbacks
-export([init/1, 
         'STARTED'/2, 'SUBSCRIBE'/2,
         handle_sync_event/4,
         terminate/3, code_change/4]).

-define(SERVER, ?MODULE).

-record(state, {subscribe_queue, subscribe_config}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> ok,Pid} | ignore | {error,Error}
%% Description:Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.  
%%--------------------------------------------------------------------
start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

start_subscribe_sequence() ->
    gen_fsm:send_event(?SERVER, start_subscribe_sequence).

handle_yate_event(YateEvent) ->
    gen_fsm:send_event(?SERVER, {handle_yate_event, YateEvent}).

resolve_custom_module(YateEvent) ->
    gen_fsm:sync_send_all_state_event(?SERVER, {resolve_custom_module, YateEvent}).


%%====================================================================
%% gen_fsm callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, StateName, State} |
%%                         {ok, StateName, State, Timeout} |
%%                         ignore                              |
%%                         {stop, StopReason}                   
%% Description:Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/3,4, this function is called by the new process to 
%% initialize. 
%%--------------------------------------------------------------------
init([]) ->
    {ok, 'STARTED', #state{subscribe_config=yaterl_config:yate_message_subscribe_configlist()}}.

%%--------------------------------------------------------------------
%% Function: 
%% state_name(Event, State) -> {next_state, NextStateName, NextState}|
%%                             {next_state, NextStateName, 
%%                                NextState, Timeout} |
%%                             {stop, Reason, NewState}
%% Description:There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same name as
%% the current state name StateName is called to handle the event. It is also 
%% called if a timeout occurs. 
%%--------------------------------------------------------------------
'STARTED'(start_subscribe_sequence, State) ->
    error_logger:info_msg("start_subscribe_sequence~n"),
    {NextState, NewStateData} = case start_request_queue(State) of
                                    {continue, StateData} -> {'SUBSCRIBE', StateData};
                                    {finish, StateData} -> {'COMPLETED', StateData}
                                end,
    {next_state, NextState, NewStateData}.

'SUBSCRIBE'({handle_yate_event, _YateEvent}, State) ->
    {NextState, NewStateData} = case run_request_queue(State) of 
        {continue, StateData} -> {'SUBSCRIBE', StateData};
        {finish, StateData} -> {'COMPLETED', StateData}
    end,
    {next_state, NextState, NewStateData}.

%%--------------------------------------------------------------------
%% Function:
%% state_name(Event, From, State) -> {next_state, NextStateName, NextState} |
%%                                   {next_state, NextStateName, 
%%                                     NextState, Timeout} |
%%                                   {reply, Reply, NextStateName, NextState}|
%%                                   {reply, Reply, NextStateName, 
%%                                    NextState, Timeout} |
%%                                   {stop, Reason, NewState}|
%%                                   {stop, Reason, Reply, NewState}
%% Description: There should be one instance of this function for each
%% possible state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/2,3, the instance of this function with the same
%% name as the current state name StateName is called to handle the event.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: 
%% handle_event(Event, StateName, State) -> {next_state, NextStateName, 
%%						  NextState} |
%%                                          {next_state, NextStateName, 
%%					          NextState, Timeout} |
%%                                          {stop, Reason, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
%%handle_event(_Event, StateName, State) ->
%%    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_sync_event(Event, From, StateName, 
%%                   State) -> {next_state, NextStateName, NextState} |
%%                             {next_state, NextStateName, NextState, 
%%                              Timeout} |
%%                             {reply, Reply, NextStateName, NextState}|
%%                             {reply, Reply, NextStateName, NextState, 
%%                              Timeout} |
%%                             {stop, Reason, NewState} |
%%                             {stop, Reason, Reply, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
%%handle_sync_event(Event, From, StateName, State) ->
%%    Reply = ok,
%%    {reply, Reply, StateName, State}.

handle_sync_event({resolve_custom_module, YateEvent}, _From, StateName, StateData) ->
    SubscribeConfig = StateData#state.subscribe_config,
    Reply = case proplists:lookup(yate_message:name(YateEvent), SubscribeConfig) of
                none -> unknown;
                {_MessageName, install, Priority} -> install;
                {_MessageName, install} -> install;
                {_MessageName, watch} -> watch
    end,
    {reply, Reply, StateName, StateData}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_info(Info,StateName,State)-> {next_state, NextStateName, NextState}|
%%                                     {next_state, NextStateName, NextState, 
%%                                       Timeout} |
%%                                     {stop, Reason, NewState}
%% Description: This function is called by a gen_fsm when it receives any
%% other message than a synchronous or asynchronous event
%% (or a system message).
%%--------------------------------------------------------------------
%%handle_info(_Info, StateName, State) ->
%%    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, StateName, State) -> void()
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Function:
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

start_request_queue(State) ->
    Queue = queue:from_list(State#state.subscribe_config),
    error_logger:info_msg("SUBSCRIBE QUEUE: ~p~n", [Queue]),
    SubscribeState = State#state{subscribe_queue=Queue},
    run_request_queue(SubscribeState).

run_request_queue(State) ->
    {Out, NewQueue} = queue:out(State#state.subscribe_queue),
    NewState = State#state{subscribe_queue = NewQueue},
    error_logger:info_msg("SEND FROM SUBSCRIBE QUEUE: ~p~n", [Out]),
    case Out of
        empty -> {finish, NewState }; 
        {value, V} -> send_subscribe_request(V),
                      {continue, NewState}
    end.
    

send_subscribe_request({MessageName, install}) ->
    YateEvent = yate_event:new(install, [{name, MessageName}]),
    send_to_yate(YateEvent),
    ok;
send_subscribe_request({MessageName, install, Priority}) ->
    YateEvent = yate_event:new(install, [{name, MessageName},{priority, Priority}]),
    send_to_yate(YateEvent),
    ok;
send_subscribe_request({MessageName, watch}) ->
    YateEvent = yate_event:new(watch, [{name, MessageName}]),
    send_to_yate(YateEvent),
    ok.

send_to_yate(YateEvent) ->
    error_logger:info_msg("SEND TO YATE: ~p~n", [YateEvent]),
    YateConnectionManager = yaterl_config:yate_connection_mgr(),
    YateConnectionManager:send_binary_data(yate_encode:to_binary(YateEvent)).
