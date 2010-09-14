%% yate_stdio_connection: yate stdio connection server
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

%% @doc '{@module}' is a gen_server erlang process that filter
%%      yaterl logging as configured in log_level yaterl config attributes
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

%% @doc: Starts the server
%% @spec: () -> {ok,Pid} | ignore | {error,Error}
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc: Get the current log_level
%% @spec: () -> disabled | error | warn | info
log_level() ->
    gen_server:call(?SERVER, log_level).

%% @doc: Set the current log_level
%% @spec: (Value) -> ok
%% where
%%   Value = disabled | error | warn | info
log_level(Value) ->
    gen_server:call(?SERVER, {log_level, Value}).

%% @doc: process an error_msg
%% @spec: (Msg::string()) -> ok
error_msg(Msg) ->
    gen_server:cast(?SERVER, {error_msg, Msg, []}).

%% @doc: process an error_msg
%% @spec: (Msg::string(), DataList) -> ok
error_msg(Msg, DataList) ->
    gen_server:cast(?SERVER, {error_msg, Msg, DataList}).

%% @doc: process a warning_msg
%% @spec: (Msg::string()) -> ok
warning_msg(Msg) ->
    gen_server:cast(?SERVER, {warning_msg, Msg, []}).

%% @doc: process a warning_msg
%% @spec: (Msg::string(), DataList) -> ok
warning_msg(Msg, DataList) ->
    gen_server:cast(?SERVER, {warning_msg, Msg, DataList}).

%% @doc: process an info_msg
%% @spec: (Msg::string()) -> ok
info_msg(Msg) ->
    gen_server:cast(?SERVER, {info_msg, Msg, []}).

%% @doc: process an info_msg
%% @spec: (Msg::string(), DataList) -> ok
info_msg(Msg, DataList) ->
    gen_server:cast(?SERVER, {info_msg, Msg, DataList}).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc: <b>[GEN_SERVER CALLBACK]</b> Initiates the server
%% @spec: ([]) -> {ok, State} | {ok, State, Timeout} | ignore | {stop, Reason}
init([]) ->
    LogLevel = yaterl_config:log_level(),
    {ok, #state{log_level=LogLevel}}.

%% @doc: <b>[GEN_SERVER CALLBACK]</b> Handling call messages
%%       
%% @see log_level/0
%% @see log_level/1
handle_call(log_level, _From, State) ->
    Reply = State#state.log_level,
    {reply, Reply, State};
handle_call({log_level, Value}, _From, State) ->
    NewState = State#state{log_level=Value},
    Reply = Value,
    {reply, Reply, NewState}.

%% @doc: <b>[GEN_SERVER CALLBACK]</b> Handling cast messages
%%       
%% @see error_msg/1 
%% @see error_msg/2
%% @see warning_msg/1 
%% @see warning_msg/2
%% @see info_msg/1 
%% @see info_msg/2
handle_cast({error_msg, Msg, DataList}, State) ->
    check_log_level_and_run(error, State#state.log_level, Msg, DataList),
    {noreply, State};
handle_cast({warning_msg, Msg, DataList}, State) ->
    check_log_level_and_run(warning, State#state.log_level, Msg, DataList),
    {noreply, State};
handle_cast({info_msg, Msg, DataList}, State) ->
    check_log_level_and_run(info, State#state.log_level, Msg, DataList),
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

