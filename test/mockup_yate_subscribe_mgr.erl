%% yate_event_mgr: yate event manager
%%
%% Copyright (C) 2009-2010 - Alca Società Cooperativa <info@alcacoop.it>
%%
%% Author: Luca Greco <luca.greco@alcacoop.it>
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% General Public License for more details.
%%
%% You should have received a copy of the GNU Lessel General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.

%% @author Luca Greco <luca.greco@alcacoop.it>
%% @copyright 2009-2010 Alca Societa' Cooperativa

%% @doc 'yate_event_mgr' is a gen_srv erlang process that handle incoming yate events
-module(mockup_yate_subscribe_mgr).

-behaviour(gen_server).

%% API
-export([
         start_link/0
        ]).

%% internal callbacks
-export([
         start_subscribe_sequence/0,
         handle_yate_event/1,
         resolve_custom_module/1
        ]).

%% test helper API
-export([
         is_start_subscribe_sequence_called/0,
         wait_for_event/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% mockup state
-record(state, {start_subscribe_sequence_called,
               wait_for_event_from,
               yate_event}).

%% import yate_event record definition, and other utils (yate exceptions helpers)  
%% @headerfile "../include/yate.hrl"
-include("../include/yate.hrl").

%%====================================================================
%% Test helpers API
%%====================================================================

is_start_subscribe_sequence_called() ->
    gen_server:call(?SERVER, is_start_subscribe_sequence_called).

wait_for_event(YateEvent1) ->
    gen_server:call(?SERVER, {wait_for_event, YateEvent1}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_subscribe_sequence() ->
    gen_server:call(?SERVER, start_subscribe_sequence_called).    

handle_yate_event(YateEvent) ->
    gen_server:call(?SERVER, {handle_yate_event, YateEvent}).

resolve_custom_module(YateEvent) ->
    {install, mockup_gen_yate_mod, watch, [mockup_gen_yate_mod]}.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    {ok, #state{}}.

handle_call(start_subscribe_sequence_called, From, State) ->
    NewState = State#state{start_subscribe_sequence_called=true},
    {reply, ok, NewState};
handle_call(is_start_subscribe_sequence_called, _From, State) ->
    Reply = State#state.start_subscribe_sequence_called,
    {reply, Reply, State};
handle_call({wait_for_event, YateEvent1}, From, State) ->
    NewState = State#state{wait_for_event_from=From, yate_event=YateEvent1},
    {noreply, NewState};
handle_call({handle_yate_event, YateEvent}, From, State) ->
    YateEvent = State#state.yate_event,
    gen_server:reply(State#state.wait_for_event_from, ok),
    {reply, ok, State}.

handle_info({nodedown, Node}, State) ->
    error_logger:info_msg("NODE DOWN: ~w~n", [Node]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------



