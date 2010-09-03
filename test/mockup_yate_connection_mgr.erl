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
-module(mockup_yate_connection_mgr).

-behaviour(gen_server).

%% API
-export([
         start_link/0
        ]).

%% internal callbacks
-export([
         send_yate_event/1
        ]).

%% test helper API
-export([
         notify_new_connection_available/0,
         pop_outgoing_data/0,
         fake_received_yate_event/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% mockup state
-record(state, {start_message_registering_called,
                wait_outgoing_data_from,
                outgoing_data}).

%% import yate_event record definition, and other utils (yate exceptions helpers)  
%% @headerfile "../include/yate.hrl"
-include("../include/yate.hrl").

%%====================================================================
%% Test helpers API
%%====================================================================

notify_new_connection_available() ->
    gen_server:call(?SERVER, notify_new_connection_available).

pop_outgoing_data() ->
    gen_server:call(?SERVER, wait_outgoing_data),
    gen_server:call(?SERVER, pop_outgoing_data).

fake_received_yate_event(YateEvent) ->
    gen_server:call(?SERVER, {fake_received_yate_event, YateEvent}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

send_yate_event(YateEvent) ->
    gen_server:call(?SERVER, {send_yate_event, YateEvent}).    

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    {ok, #state{outgoing_data=queue:new()}}.

handle_call(notify_new_connection_available, _From, State) ->
    YateEventMgr_Module = yaterl_config:yate_event_mgr(),
    YateEventMgr_Module:new_connection_available(),
    {reply, ok, State};
handle_call(wait_outgoing_data, From, State) ->
    NewState = State#state{wait_outgoing_data_from=From},
    {noreply, NewState, infinity};
handle_call(pop_outgoing_data, _From, State) ->
    {{value, Reply}, OutgoingData} = queue:out(State#state.outgoing_data),
    NewState = State#state{outgoing_data=OutgoingData},
    {reply, Reply, NewState};
handle_call({fake_received_yate_event, YateEvent}, _From, State) ->
    YateEventMgr_Module = yaterl_config:yate_event_mgr(),
    YateEventMgr_Module:handle_yate_event(YateEvent),
    {reply, ok, State};
handle_call({send_yate_event, YateEvent}, _From, State) ->
    NewState = State#state{outgoing_data=queue:in(YateEvent, 
                                                  State#state.outgoing_data)},
    gen_server:reply(State#state.wait_outgoing_data_from, ok),
    {reply, ok, NewState}.

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



