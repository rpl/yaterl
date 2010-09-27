%% yaterl_connection_mgr: yaterl connection manager
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

%% @doc '{@module}' is a gen_srv erlang process that coordinate
%%      application access to yate connections
-module(yaterl_connection_mgr).

-behaviour(gen_server).

%% API
-export([
         start_link/0,         
         get_yate_connection/0,
         is_connected/0,
         send_binary_data/1
        ]).

%% internal callbacks
-export([
         received_binary_data/1,
         set_yate_connection/2
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% @type state() = tuple().
%% ```#state{yate_connection}'''
-record(state, {yate_connection}).

%%====================================================================
%% API
%%====================================================================

%% @doc: Starts the server
%% @spec: () -> {ok,Pid} | ignore | {error,Error}
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc: set current yate connection
%% @spec: (YateConnection_Position, YateConnection_ModuleName::string()) -> ok 
%% where
%%    YateConnection_Position = local | {remote, YateConnection_NodeName::string()}
set_yate_connection(local, YateConnection_Module) ->
    Reply = gen_server:call(?SERVER, {set_yate_connection,
                                      YateConnection_Module}),
    Reply;
set_yate_connection({remote, YateConnection_NodeName}, YateConnection_Module) ->
    Reply = gen_server:call(?SERVER, {set_yate_connection,                                                                       
                                      YateConnection_NodeName,
                                      YateConnection_Module
                                     }),
    Reply.

%% @doc: get current yate connection
%% @spec: () -> Result
%% where
%%   Result = { NodeName , NodeModule }
get_yate_connection() ->
    gen_server:call(?SERVER, get_yate_connection).

%% @doc: check if there is a managed connection
%% @spec: () -> true | false
is_connected() ->
    gen_server:call(?SERVER, is_connected).

%% @doc: send a yate event and return immediatelly
%% @spec: (Data::binary()) -> ok
send_binary_data(Data) ->
    gen_server:cast(?SERVER, {send_binary_data, Data}).

%% @doc: receive binary data from current active connection and return immediatelly
%% @spec: (Data::binary()) -> ok
received_binary_data(Data) ->
    gen_server:cast(?SERVER, {received_binary_data, Data}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc: <b>[GEN_SERVER CALLBACK]</b> Initiates the server
%% @spec: ([]) -> {ok, State} | {ok, State, Timeout} | ignore | {stop, Reason}
init([]) ->
    {ok, #state{}}.

%% @doc: <b>[GEN_SERVER CALLBACK]</b> Handling call messages
%% @see get_yate_connection/0
%% @see is_connected/0
%% @see set_yate_connection/2  
handle_call(get_yate_connection, _From, State) ->
    Reply = {ok, State#state.yate_connection},
    {reply, Reply, State};
handle_call(is_connected, _From, State) ->
    Reply = State#state.yate_connection =/= undefined,
    {reply, Reply, State};
handle_call({set_yate_connection, YateConnection_ModuleName}, 
            _From, State) ->
    yaterl_logger:info_msg("yate_connection_mgr set_yate_connection local: ~w~n", 
                          [YateConnection_ModuleName]),
    NewState = State#state{yate_connection = {local, YateConnection_ModuleName}},
    connection_available_handling(),
    Reply = ok,
    {reply, Reply, NewState};
handle_call({set_yate_connection, YateConnection_NodeName, YateConnection_ModuleName}, 
            _From, State) ->
    yaterl_logger:info_msg("yate_connection_mgr set_yate_connection remote: ~w - ~w~n", 
                          [YateConnection_NodeName, YateConnection_ModuleName]),
    NewState = State#state{yate_connection = {remote, YateConnection_NodeName,
                                              YateConnection_ModuleName}},
    erlang:monitor_node(YateConnection_NodeName, true),
    connection_available_handling(),
    Reply = ok,
    {reply, Reply, NewState}.

%% @doc: <b>[GEN_SERVER CALLBACK]</b> Handling cast messages
%% @see send_binary_data/1
%% @see received_binary_data/1
handle_cast({send_binary_data, Data}, State) ->
    yaterl_logger:info_msg("yate_connection_mgr SEND: ~s~n", [Data]),
    send_to_yate_connection(State#state.yate_connection, Data),
    {noreply, State};
handle_cast({received_binary_data, Data}, State) ->
    yaterl_logger:info_msg("yate_connection_mgr RECEIVED: ~s~n", [Data]),
    process_incoming_data(Data),
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
    yaterl_logger:error_msg("NODE DOWN: ~w~n", [Node]),
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

send_to_yate_connection({local, YateConnection_ModuleName}, Data) ->
    YateConnection_ModuleName:send_binary_data(Data);
send_to_yate_connection({remote, YateConnection_NodeName,
                                    YateConnection_ModuleName}, Data) ->
    rpc:cast(YateConnection_NodeName, YateConnection_ModuleName,
             send_binary_data, [Data]).

process_incoming_data(Data) ->
    {ok, Pid} = yaterl_incoming_event_srv:start(Data),
    yaterl_incoming_event_srv:run(Pid).

connection_available_handling() ->
    CustomModule = yaterl_config:yaterl_custom_module_name(),
    Action = CustomModule:connection_available(),
    case Action of
        do_nothing ->
            ok;
        start_subscribe_sequence ->
            start_yate_message_subscribe_sequence()
    end.

start_yate_message_subscribe_sequence() ->
    yaterl_subscribe_mgr:start_subscribe_sequence().

