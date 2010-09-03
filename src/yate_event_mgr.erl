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
-module(yate_event_mgr).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
         send_yate_event/1
        ]).

%% internal callbacks
-export([
         new_connection_available/0,
         handle_yate_event/1
        ]).

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% @type state() = tuple().
%% ```#state{}'''
-record(state, {}).

%% import yate_event record definition, and other utils (yate exceptions helpers)  
%% @headerfile "../include/yate.hrl"
-include("yate.hrl").

%%====================================================================
%% API
%%====================================================================

%% @doc: Starts the server
%% @spec: () -> {ok,Pid} | ignore | {error,Error}
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc: receive binary data from current active connection and return immediatelly
%% @spec: (Data::binary()) -> ok
handle_yate_event(YateEvent) ->
    error_logger:info_msg("yate_event_mgr received new yate event: ~p~n", [YateEvent]),
    gen_server:cast(?SERVER, {handle_yate_event, YateEvent}).

new_connection_available() ->
    %% TODO: get from config
    error_logger:info_msg("yate_event_mgr new_connection_available... ~n"),
    gen_server:cast(?SERVER, new_connection_available).

%% @doc: send a yate event and return immediatelly
%% @spec: (YateEvent::yate_event()) -> ok
send_yate_event(YateEvent) ->
    gen_server:cast(?SERVER, {send_yate_event, YateEvent}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc: <b>[GEN_SERVER CALLBACK]</b> Initiates the server
%% @spec: ([]) -> {ok, State} | {ok, State, Timeout} | ignore | {stop, Reason}
init([]) ->
    {ok, #state{}}.

%% @doc: <b>[GEN_SERVER CALLBACK]</b> Handling cast messages
%%
%% <b>send_event</b>: send a yate event to the current active connection
%% and return immediatelly
%%      
%% <b>received_binary_data</b>: receive forwarded binary data from the 
%% active connection
%%
%% @spec: (Msg::Request, State) -> Reply
%% where
%%   Request = {send_yate_event, YateEvent} | {received_binary_data, Data}
%%   YateEvent = yate_event()
%%   Data = binary()
%%   Reply = {noreply, State}
handle_cast(new_connection_available, State) ->
    request_yate_message_registering(),
    {noreply, State};
handle_cast({send_yate_event, YateEvent}, State) ->
    send_to_yate_connection_mgr(YateEvent),
    {noreply, State};
handle_cast({handle_yate_event, YateEvent}, State) ->
    spawn_incoming_event_srv(YateEvent),
    {noreply, State}.

%% @doc: <b>[GEN_SERVER CALLBACK]</b> Handling all non call/cast messages
%%
%% <b>nodedown</b>: monitored connection node down
%%
%% @spec: (Msg::Request, State) -> Reply
%% where
%%   Request = {nodedown, Node} 
%%   Node = atom()
%%   Reply = {noreply, State}
handle_info({nodedown, Node}, State) ->
    error_logger:info_msg("NODE DOWN: ~w~n", [Node]),
    {noreply, State}.

%% @doc: <b>[GEN_SERVER CALLBACK]</b> Handling terminate sequence. 
%% It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% 
%% @spec: (_Reason, _State) -> ok
terminate(_Reason, _State) ->
    ok.

%% @doc: <b>[GEN_SERVER CALLBACK]</b> Convert process state when code is changed
%% @spec: (OldVsn, State, Extra) -> {ok, NewState}
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

send_to_yate_connection_mgr(YateEvent) ->
    YateConnectionMgr_ModuleName = yaterl_config:yate_connection_mgr(),
    YateConnectionMgr_ModuleName:send_yate_event(YateEvent).

request_yate_message_registering() ->
    YateRegisteringMgr_ModuleName = yaterl_config:yate_registering_mgr(),
    YateRegisteringMgr_ModuleName:start_message_registering().

spawn_incoming_event_srv(YateEvent) ->
    IncomingProcessingSrv = yaterl_config:yate_incoming_event_srv(),
    Pid = IncomingProcessingSrv:start(YateEvent),
    IncomingProcessingSrv:run(Pid).

