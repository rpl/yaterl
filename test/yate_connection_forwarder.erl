-module(yate_connection_forwarder).

-behaviour(gen_server).

%% API.
-export([
         start_link/0,
         register/0,
         connect_to/1,
         received_binary_data/1
        ]).

%% Internal export for yate_connection.
-export([
         send_binary_data/1
        ]).

%% Internal gen_server callbacks
%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2,
         terminate/2]).

-record(state, {registered_pid, mgr}).

%% API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register() ->
    gen_server:call(?MODULE, {register, self()}).

connect_to(ModuleName) ->
    gen_server:call(?MODULE, {connect_to, ModuleName}).

received_binary_data(Data) ->
    gen_server:call(?MODULE, {received_binary_data, Data}).

send_binary_data(Data) ->
    gen_server:call(?MODULE, {send_binary_data, Data}).

%% GEN_SERVER CALLBACKS

init([]) ->
    {ok, #state{}}.
 
handle_call({register, Pid}, _From, State) ->
    NewState=State#state{registered_pid=Pid},
    {reply, ok, NewState};
handle_call({connect_to, ModuleName}, _From, State) ->
    ok = ModuleName:set_yate_connection(local, ?MODULE),
    NewState=State#state{mgr=ModuleName},
    {reply, ok, NewState};
handle_call({received_binary_data, Data}, _From, State) ->
    Mgr=State#state.mgr,
    Mgr:received_binary_data(Data),
    {reply, ok, State};
handle_call({send_binary_data, Data}, _From, State) ->
    Pid = State#state.registered_pid,
    Pid ! Data,
    {reply, ok, State}.

handle_info(_, State) ->
    {ok,State}.

terminate(_Reason, State) ->
    State.
