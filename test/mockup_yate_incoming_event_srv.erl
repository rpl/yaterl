-module(mockup_yate_incoming_event_srv).

-behaviour(gen_server).

-include_lib("ct.hrl").
-include("../include/yate.hrl").
-include("local_test_helpers.hrl").

%% Mockup Test helper API
-export([
         mockup_start_link/0,
         is_spawned_and_run_called/1
        ]).

%% API
-export([
         start/1,
         run/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).

-define(SERVER, ?MODULE).

-record(state, {expected_yate_event, yate_event, run_called,
                waitfor_spawned_from}).

%%====================================================================
%% Test helpers API
%%====================================================================

mockup_start_link() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

is_spawned_and_run_called(YateEvent) ->
    gen_server:call(?SERVER, {wait_spawned_and_run, YateEvent}),
    gen_server:call(?SERVER, is_run_called).


%%====================================================================
%% API
%%====================================================================

start(YateEvent) ->
    ?CT_LOG(YateEvent),
    gen_server:cast(?SERVER, {start, YateEvent}),
    {ok, fake_pid}.

run(Pid) ->
    ?CT_LOG("Called RUN on Spawned Server"),
    gen_server:cast(?SERVER, {run, Pid}),
    ok.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    {ok, #state{}}.

handle_cast({start, YateEvent}, State) ->
    NewState = State#state{yate_event=YateEvent},
    {noreply, NewState};
handle_cast({run, _Pid}, State) ->
    NewState = State#state{run_called=true},
    Reply = (State#state.yate_event =:= State#state.yate_event),
    gen_server:reply(State#state.waitfor_spawned_from, Reply),
    {noreply, NewState}.
handle_call({wait_spawned_and_run, YateEvent}, From, State) ->
    NewState = State#state{waitfor_spawned_from=From,
                          expected_yate_event=YateEvent},
    {noreply, NewState};
handle_call(is_run_called, _From, State) ->
    Reply = State#state.run_called,
    {reply, Reply, State}.


