-module(yaterl_gen_mod_forwarder).

-behaviour(gen_server).

%% API.
-export([
         start_link/0,
         register/0
        ]).

%% Internal export for gen_yate_mod
-export([
         connection_available/0,
         subscribe_config/0,
         subscribe_error/2,
         handle_install_message/1,
         handle_watch_message/1
        ]).

%% Internal gen_server callbacks
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

-record(state, {registered_pid}).

%% API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register() ->
    gen_server:call(?MODULE, {register, self()}).

%% GEN_YATE_MOD CALLBACKS

connection_available() ->
    gen_server:call(?MODULE, connection_available).

subscribe_config() ->
    gen_server:call(?MODULE, subscribe_config).

subscribe_error(LastRequested, LastReceived) ->
    gen_server:call(?MODULE, {subscribe_error, LastRequested, LastReceived}).

handle_install_message(YateMessage) ->
    gen_server:call(?MODULE, {handle_install_message, YateMessage}).

handle_watch_message(YateMessage) ->
    gen_server:cast(?MODULE, {handle_watch_message, YateMessage}).

%% GEN_SERVER CALLBACKS

init([]) ->
    {ok, #state{}}.
 
handle_call({register, Pid}, _From, State) ->
    NewState=State#state{registered_pid=Pid},
    {reply, ok, NewState};
handle_call(connection_available, From, State) ->
    Pid = State#state.registered_pid,
    Pid ! {connection_available, From},    
    {noreply, State, infinity};    
handle_call(subscribe_config, From, State) ->
    Pid = State#state.registered_pid,
    Pid ! {subscribe_config, From},    
    {noreply, State, infinity};    
handle_call({subscribe_error, LastRequested, LastReceived}, From, State) ->
    Pid = State#state.registered_pid,
    Pid ! {subscribe_error, LastRequested, LastReceived, From},    
    {noreply, State, infinity};    
handle_call({handle_install_message, YateMessage}, From, State) ->
    Pid = State#state.registered_pid,
    Pid ! {install, YateMessage, From},    
    {noreply, State, infinity}.

handle_cast({handle_watch_message, YateMessage}, State) ->
    Pid = State#state.registered_pid,
    Pid ! {watch, YateMessage},
    {noreply, State}.

handle_info(_, State) ->
    {ok,State}.

terminate(_Reason, State) ->
    State.
