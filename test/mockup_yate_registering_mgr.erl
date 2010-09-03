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
-module(mockup_yate_registering_mgr).

-behaviour(gen_server).

%% API
-export([
         start_link/0
        ]).

%% internal callbacks
-export([
         start_message_registering/0
        ]).

%% test helper API
-export([
         is_start_message_registering_called/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% mockup state
-record(state, {start_message_registering_called}).

%% import yate_event record definition, and other utils (yate exceptions helpers)  
%% @headerfile "../include/yate.hrl"
-include("../include/yate.hrl").

%%====================================================================
%% Test helpers API
%%====================================================================

is_start_message_registering_called() ->
    gen_server:call(?SERVER, is_start_message_registering_called).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_message_registering() ->
    gen_server:call(?SERVER, start_message_registering_called).    

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    {ok, #state{}}.

handle_call(start_message_registering_called, From, State) ->
    NewState = State#state{start_message_registering_called=true},
    {reply, ok, NewState};
handle_call(is_start_message_registering_called, _From, State) ->
    Reply = State#state.start_message_registering_called,
    {reply, Reply, State}.

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



