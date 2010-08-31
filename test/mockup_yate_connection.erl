-module(mockup_yate_connection).

-behaviour(gen_server).

%% Mockup Test helper API
-export([
         pop_outgoing_data/0
        ]).

%% API
-export([
         start_link/0,
         send_binary_data/1,
         received_binary_data/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% @type state() = tuple().
%% ```#state{yate_port, yate_control}'''
-record(state, {yate_port, yate_connection_mgr, 
                
                %% Mockup Test helper state
                outgoing_data, wait_outgoing_data_from}).

%%====================================================================
%% Test helpers API
%%====================================================================

pop_outgoing_data() ->
    gen_server:call(?SERVER, wait_outgoing_data),
    gen_server:call(?SERVER, pop_outgoing_data).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

is_managed() ->
    gen_server:call(?SERVER, is_managed).

get_manager() ->
    gen_server:call(?SERVER, get_manager).

send_binary_data(Data) ->
    gen_server:call(?SERVER, { send_binary_data, Data }).

received_binary_data(Data) ->
    gen_server:cast(?SERVER, { received_binary_data, Data }).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    {YateConnectionMgr_NodeName, YateConnectionMgr_HostName, MaxBytesLine} = load_config_from_env(),
    Port = open_yate_port(MaxBytesLine),
    YateConnectionMgr = register_to_yate_connection_mgr(YateConnectionMgr_NodeName, 
                                               YateConnectionMgr_HostName),
    State = #state{yate_port=Port, yate_connection_mgr=YateConnectionMgr,
                  outgoing_data=queue:new()},
    {ok, State}.

handle_call({send_binary_data, Data}, _From, State) ->
    NewState = State#state{outgoing_data=queue:in(Data, 
                                                  State#state.outgoing_data)},
    gen_server:reply(State#state.wait_outgoing_data_from, ok),
    {reply, ok, NewState};
handle_call(wait_outgoing_data, From, State) ->
    NewState = State#state{wait_outgoing_data_from=From},
    {noreply, NewState, infinity};
handle_call(pop_outgoing_data, _From, State) ->
    {{value, Reply}, OutgoingData} = queue:out(State#state.outgoing_data),
    NewState = State#state{outgoing_data=OutgoingData},
    {reply, Reply, NewState};
handle_call(is_managed, _From, State) ->
    {reply, State#state.yate_connection_mgr =/= undefined, State};
handle_call(get_manager, _From, State) ->
    {reply, {ok, State#state.yate_connection_mgr}, State}.


handle_cast({received_binary_data, Data}, State) ->
    send_receive_data_to_yate_connection_mgr(State#state.yate_connection_mgr, Data),    
    {noreply, State}.

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

load_config_from_env() ->
    [H | _T ] = string:tokens(atom_to_list(node()), "@"),
    YateConnectionMgr_NodeName = self,
    YateConnectionMgr_HostName = localhost,
    MaxBytesLine = 80000,
    {YateConnectionMgr_NodeName, YateConnectionMgr_HostName, MaxBytesLine}.

open_yate_port(_MaxBytesLine) ->
    % Open a port on stdin/stdout to talk with yate
    _Port = fake_port.

send_receive_data_to_yate_connection_mgr(local, Data) ->
    yate_connection_mgr:received_binary_data(Data);
send_receive_data_to_yate_connection_mgr({remote, FullNodeName}, Data) ->
    rpc:cast(FullNodeName, yate_connection_mgr, received_binary_data, [Data]).  

%%% All-in-one configuration
register_to_yate_connection_mgr(self, localhost) ->
    % Return a local yate_connection_mgr descriptor
    ok = yate_connection_mgr:set_yate_connection(local, ?MODULE),
    local;
%%% Remote Yate Event Manager configuration
register_to_yate_connection_mgr(YateConnectionMgr_NodeName, YateConnectionMgr_HostName) ->
    YateConnectionMgr_FullNodeName = list_to_atom(string:join([YateConnectionMgr_NodeName, 
                                                  YateConnectionMgr_HostName], "@")),
    % Ping yate_control node
    case net_adm:ping(YateConnectionMgr_FullNodeName) of
        pong -> ok;
        pang -> error_logger:error_msg("ERROR: Yate Event Manager node '~s' is down~n", 
                                       [YateConnectionMgr_FullNodeName]),
                exit(yate_connection_mgr_nodedown)            
    end,
    % Register to yate_control_srv nodedown events
    erlang:monitor_node(YateConnectionMgr_FullNodeName, true),
    rpc:call(YateConnectionMgr_FullNodeName, yate_connection_mgr, 
             set_yate_connection, [{remote, YateConnectionMgr_FullNodeName}, ?MODULE]),
    % Return a remote yate_connection_mgr descriptor
    {remote, YateConnectionMgr_FullNodeName}.

