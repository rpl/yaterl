%% yaterl_sup: yaterl supervisor
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

%% @doc '{@module}' is an OTP supervisor, it will start yaterl servers
%%      as configured by yater_sup_mode config attribute.
-module(yaterl_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc: Starts yaterl_sup supervisor as configured 
%%       by yaterl_sup_mode
%% @spec: (Mode) -> {ok,Pid} | ignore | {error,Error}
%% where
%%    Mode = all_in_one | manager_only | stdio_connection_only
start_link(Mode) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Mode]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% @doc: <b>[SUPERVISOR CALLBACK]</b> Initiates the supervisor
%% @spec: ([]) -> {ok, State} | {ok, State, Timeout} | ignore | {stop, Reason}
init([Mode]) ->
    {ok, { {one_for_one, 5, 10}, get_childs_spec(Mode) } }.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

get_childs_spec(manager_only) ->
    YaterlLoggerSpec = ?CHILD(yaterl_logger, worker),
    YateConnectionMgrSpec = ?CHILD(yaterl_connection_mgr, worker),
    YateSubscribeMgrSpec = ?CHILD(yaterl_subscribe_mgr, worker),
    [YaterlLoggerSpec, YateConnectionMgrSpec, YateSubscribeMgrSpec];
get_childs_spec(stdio_connection_only) ->
    YaterlLoggerSpec = ?CHILD(yaterl_logger, worker),
    YateStdioConnectionSpec = ?CHILD(yaterl_stdio_connection, worker),
    [YaterlLoggerSpec, YateStdioConnectionSpec];
get_childs_spec(all_in_one) ->
    YaterlTracerSpec = ?CHILD(yaterl_tracer, worker),
    YaterlLoggerSpec = ?CHILD(yaterl_logger, worker),
    YateConnectionMgrSpec = ?CHILD(yaterl_connection_mgr, worker),
    YateStdioConnectionSpec = ?CHILD(yaterl_stdio_connection, worker),
    YateSubscribeMgrSpec = ?CHILD(yaterl_subscribe_mgr, worker),
    [YaterlTracerSpec, YaterlLoggerSpec, YateSubscribeMgrSpec,
     YateConnectionMgrSpec, YateStdioConnectionSpec]. 

