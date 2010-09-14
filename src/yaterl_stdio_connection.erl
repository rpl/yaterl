%% yaterl_stdio_connection: yaterl stdio connection server
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

%% @doc '{@module}' is a gen_server erlang process that 
%%      open and manage a stdio connection to YATE VOIP server.
-module(yaterl_stdio_connection).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
         is_managed/0,
         get_manager/0,
         send_binary_data/1,
         received_binary_data/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% @type state() = tuple().
%% ```#state{yate_port, yaterl_connection_mgr}'''
-record(state, {yate_port, yaterl_connection_mgr}).

%%====================================================================
%% API
%%====================================================================

%% @doc: Starts the server
%% @spec: () -> {ok,Pid} | ignore | {error,Error}
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc: Check if this connection is registered to a connection manager
%% @spec: () -> true | false
is_managed() ->
    gen_server:call(?SERVER, is_managed).

%% @doc: Return the current registered connection manager
%% @spec: () -> Manager | undefined
%% where
%%    Manager = {remote, YateConnectionMgr_FullNodeName::string()} | local
get_manager() ->
    gen_server:call(?SERVER, get_manager).

%% @doc: Send binary data to yate
%% @spec: (Data::binary()) -> ok
send_binary_data(Data) ->
    gen_server:call(?SERVER, { send_binary_data, Data }).

%% @doc: Received binary data to yate (routed to yaterl_connection_mgr)
%% @spec: (Data::binary()) -> ok
received_binary_data(Data) ->
    gen_server:cast(?SERVER, { received_binary_data, Data }).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc: <b>[GEN_SERVER CALLBACK]</b> Initiates the server
%% @spec: ([]) -> {ok, State} | {ok, State, Timeout} | ignore | {stop, Reason}
init([]) ->
    {YateConnectionMgr_NodeName, YateConnectionMgr_HostName, MaxBytesLine} = load_config_from_env(),
    Port = open_yate_port(MaxBytesLine),
    YateConnectionMgr = register_to_yaterl_connection_mgr(YateConnectionMgr_NodeName, 
                                               YateConnectionMgr_HostName),
    State = #state{yate_port=Port, yaterl_connection_mgr=YateConnectionMgr},
    {ok, State}.

%% @doc: <b>[GEN_SERVER CALLBACK]</b> Handling call messages
%%
%% @see send_binary_data/1
%% @see is_managed/0
%% @see get_manager/0
handle_call({send_binary_data, Data}, _From, State) ->
    yaterl_logger:info_msg("SEND TO YATE ON STDIO: ~p~n", [Data]),
    io:fwrite(standard_error, "SENDING: ~s~n", [Data]),
    true = port_command(State#state.yate_port, <<Data/binary, "\n">>),
    {reply, ok, State};
handle_call(is_managed, _From, State) ->
    {reply, State#state.yaterl_connection_mgr =/= undefined, State};
handle_call(get_manager, _From, State) ->
    {reply, {ok, State#state.yaterl_connection_mgr}, State}.

%% @doc: <b>[GEN_SERVER CALLBACK]</b> Handling cast messages
%%       
%% @see received_binary_data/1
handle_cast({received_binary_data, Data}, State) ->
    send_receive_data_to_yaterl_connection_mgr(State#state.yaterl_connection_mgr, Data),
    {noreply, State}.

%% @doc: <b>[GEN_SERVER CALLBACK]</b> Handling all non call/cast messages
%%
%% <b>nodedown</b>: monitored connection node down
%%
%% <b>port data</b>: receiving data from yate port
%%
%% <b>port eof</b>: closed event from yate port
%%
%% @spec: (Msg::Request, State) -> Reply
%% where
%%   Request = {nodedown, Node} | {Port, {data, {Eol, Data}}} |
%%             {Port, eof}
%%   Node = atom()
%%   Port = port()
%%   Data = binary()
%%   Eol  = eol | noeol
%%   Reply = {noreply, State}
handle_info({_Port, {data, {Eol, Data}}}, State) when Eol==eol; Eol==noeol ->
    yaterl_logger:info_msg("RECEIVED FROM YATE ON STDIO: ~p~n", [Data]),
    io:fwrite(standard_error, "READ: ~s~n", [Data]),
    received_binary_data(Data),
    {noreply, State};
handle_info({_Port, eof}, State) ->
    io:fwrite(standard_error, "EXITING...~n", []),
    {stop, "YATE closed stdio socket", State};

handle_info({nodedown, Node}, State) ->
    io:fwrite(standard_error, "ERROR: Yate Control Server node '~w' down.~n",
              [Node]),
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

load_config_from_env() ->
    {YateConnectionMgr_NodeName, 
     YateConnectionMgr_HostName} = yaterl_config:whereis_yaterl_connection_mgr(),
    MaxBytesLine = yaterl_config:yaterl_connection_maxbytesline(),
    {YateConnectionMgr_NodeName, YateConnectionMgr_HostName, MaxBytesLine}.

open_yate_port(MaxBytesLine) ->
    % Open a port on stdin/stdout to talk with yate
    _Port = open_port({fd, 0, 1}, [stream,binary,{line, MaxBytesLine},eof]).

send_receive_data_to_yaterl_connection_mgr(local, Data) ->
    yaterl_connection_mgr:received_binary_data(Data);
send_receive_data_to_yaterl_connection_mgr({remote, FullNodeName}, Data) ->
    rpc:cast(FullNodeName, yaterl_connection_mgr, received_binary_data, [Data]).  

%%% All-in-one configuration
register_to_yaterl_connection_mgr(self, localhost) ->
    % Return a local yaterl_connection_mgr descriptor
    ok = yaterl_connection_mgr:set_yate_connection(local, ?MODULE),
    local;
%%% Remote Yate Event Manager configuration
register_to_yaterl_connection_mgr(YateConnectionMgr_NodeName, YateConnectionMgr_HostName) ->
    YateConnectionMgr_FullNodeName = list_to_atom(string:join([YateConnectionMgr_NodeName, 
                                                  YateConnectionMgr_HostName], "@")),
    % Ping yaterl_connection_mgr node
    case net_adm:ping(YateConnectionMgr_FullNodeName) of
        pong -> ok;
        pang -> yaterl_logger:error_msg("ERROR: Yate Event Manager node '~s' is down~n", 
                                       [YateConnectionMgr_FullNodeName]),
                exit(yaterl_connection_mgr_nodedown)            
    end,
    % Register to yaterl_connection_mgr nodedown events
    erlang:monitor_node(YateConnectionMgr_FullNodeName, true),
    rpc:call(YateConnectionMgr_FullNodeName, yaterl_connection_mgr, 
             set_yate_connection, [{remote, node()}, ?MODULE]),
    % Return a remote yaterl_connection_mgr descriptor
    {remote, YateConnectionMgr_FullNodeName}.

