%%%-------------------------------------------------------------------
%%% File    : yaterl_logging.erl
%%% Author  : rpl <>
%%% Description : 
%%%
%%% Created :  9 Sep 2010 by rpl <>
%%%-------------------------------------------------------------------
-module(yaterl_logger).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
         log_level/0,
         log_level/1
        ]).

%% Yaterl Internal callbacks
-export([
         warning_msg/1,
         warning_msg/2,
         info_msg/1,
         info_msg/2,
         error_msg/1,
         error_msg/2
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, 
         terminate/2, code_change/3]).

-define(LOG_LEVELS, [disabled, error, warning, info]).

-define(SERVER, ?MODULE).

-record(state, {log_level=undefined}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

log_level() ->
    gen_server:call(?SERVER, log_level).

log_level(Value) ->
    gen_server:call(?SERVER, {log_level, Value}).

error_msg(Msg) ->
    gen_server:cast(?SERVER, {error_msg, Msg}).

error_msg(Msg, DataList) ->
    gen_server:cast(?SERVER, {error_msg, Msg, DataList}).

warning_msg(Msg) ->
    gen_server:cast(?SERVER, {warning_msg, Msg}).

warning_msg(Msg, DataList) ->
    gen_server:cast(?SERVER, {warning_msg, Msg, DataList}).

info_msg(Msg) ->
    gen_server:cast(?SERVER, {info_msg, Msg}).

info_msg(Msg, DataList) ->
    gen_server:cast(?SERVER, {info_msg, Msg, DataList}).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    LogLevel = yaterl_config:log_level(),
    {ok, #state{log_level=LogLevel}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(log_level, _From, State) ->
    Reply = State#state.log_level,
    {reply, Reply, State};
handle_call({log_level, Value}, _From, State) ->
    NewState = State#state{log_level=Value},
    Reply = Value,
    {reply, Reply, NewState}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({error_msg, Msg}, State) ->
    check_log_level_and_run(error, State#state.log_level, Msg, []),
    {noreply, State};
handle_cast({error_msg, Msg, DataList}, State) ->
    check_log_level_and_run(error, State#state.log_level, Msg, DataList),
    {noreply, State};
handle_cast({warning_msg, Msg}, State) ->
    check_log_level_and_run(warning, State#state.log_level, Msg, []),
    {noreply, State};
handle_cast({warning_msg, Msg, DataList}, State) ->
    check_log_level_and_run(warning, State#state.log_level, Msg, DataList),
    {noreply, State};
handle_cast({info_msg, Msg}, State) ->
    check_log_level_and_run(info, State#state.log_level, Msg, []),
    {noreply, State};
handle_cast({info_msg, Msg, DataList}, State) ->
    check_log_level_and_run(info, State#state.log_level, Msg, DataList),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

check_log_level_and_run(RequiredLevel, CurrentLevel, Msg, DataList) ->
    case {RequiredLevel, CurrentLevel} of
        {error, disabled} -> ok;
        {error, _} -> error_logger:error_msg(Msg, DataList), ok;
        {warning, disabled} -> ok;
        {warning, error} -> ok;
        {warning, _ } -> error_logger:warning_msg(Msg, DataList), ok;
        {info, disabled} -> ok;
        {info, error} -> ok;
        {info, warning} -> ok;
        {info, _ } -> error_logger:info_msg(Msg, DataList), ok
    end.    

