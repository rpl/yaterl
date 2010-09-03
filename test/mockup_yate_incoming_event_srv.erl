-module(mockup_yate_incoming_event_srv).

-behaviour(gen_server).

-include_lib("ct.hrl").
-include("../include/yate.hrl").
-include("local_test_helpers.hrl").

%% Mockup Test helper API
-export([
         mockup_start_link/0
        ]).

%% API
-export([
         start/1,
         is_spawned/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).

-define(SERVER, ?MODULE).

-record(state, {yate_event,
                waitfor_spawned_from}).

%%====================================================================
%% Test helpers API
%%====================================================================

mockup_start_link() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% API
%%====================================================================

start(YateEvent) ->
    ?CT_LOG(YateEvent),
    gen_server:cast(?SERVER, {start, YateEvent}).

is_spawned(YateEvent) ->
    gen_server:call(?SERVER, {wait_spawned, YateEvent}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    {ok, #state{}}.

handle_cast({start, YateEvent}, State) ->
    Reply = (YateEvent =:= State#state.yate_event),
    gen_server:reply(State#state.waitfor_spawned_from, Reply),
    {noreply, State}.

handle_call({wait_spawned, YateEvent}, From, State) ->
    NewState = State#state{waitfor_spawned_from=From,
                          yate_event=YateEvent},
    {noreply, NewState}.


