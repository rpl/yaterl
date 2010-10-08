%% yaterl_config: yaterl config module
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

%% @doc '{@module}' is used internally to get and set yaterl configuration attributes
-module(yaterl_config).

-export([
         log_level/0,
         log_level/1,

         log_files/2,

         tracing_enabled/0,
         tracing_config/0,
         tracing_config/3,

         whereis_yaterl_connection_mgr/0,
         whereis_yaterl_connection_mgr/1,

         yaterl_connection_maxbytesline/0,
         yaterl_connection_maxbytesline/1,

         yaterl_sup_mode/0,
         yaterl_sup_mode/1,

         yaterl_custom_module_name/0,
         yaterl_custom_module_name/1
        ]).

%% @doc: Get the current log_level (defaults to error)
%% @spec: () -> disabled | error | warn | info
log_level() ->
    get_key(log_level, error).

%% @doc: Set the current log_level 
%% @spec: (Value) -> ok
%% where
%%   Value = disabled | error | warn | info
log_level(Value) ->
    set_key(log_level, Value).

%% @doc: Set the application and sasl log files
%% @spec: (AppLogFile::string(), SaslLogFile::string()) -> ok
log_files(AppLogFile, SaslLogFile) when is_list(AppLogFile), is_list(SaslLogFile)->
    error_logger:tty(false),
    error_logger:logfile({open, AppLogFile}),
    application_controller:change_application_data([{application, sasl, []}],
                      [
                       {sasl, [{sasl_error_logger, {file, SaslLogFile}}]}
                      ]),
    ok.

tracing_enabled() ->
    {undefined, undefined, undefined} =/= tracing_config().

tracing_config() ->
    get_key(tracing, {undefined, undefined, undefined}).

tracing_config(Name,Title,Filename) ->
    set_key(tracing, {Name, Title, Filename}).

%% @doc: Get the yaterl_connection_mgr location
%% @spec: () -> ConnectionMgr_Location
%% where
%%   ConnectionMgr_Location = {NodeName::string(), HostName::string()}
whereis_yaterl_connection_mgr() ->
    {_NodeName, _HostName} = get_key(whereis_yaterl_connection_mgr, {self, localhost}).
    %% RealNodeName = case {NodeName,
    %%                      is_list(NodeName)} of
    %%                    {self, false} -> [H | _T ] = string:tokens(
    %%                                                   atom_to_list(node()), "@"
    %%                                                  ),
    %%                                     H;
    %%                    {CustomNodeName, true} -> CustomNodeName
    %%                end,
    %% RealHostName = case {HostName,
    %%                      is_list(HostName)} of
    %%                    {localhost, false} -> net_adm:localhost();
    %%                    {CustomHostName, true} -> CustomHostName
    %%                end,
    %% {RealNodeName, RealHostName}.

%% @doc: Set the yaterl_connection_mgr location
%% @spec: (Value) -> ok
%% where
%%   Value = {NodeName::string(), HostName::string()} | {self, localhost} | undefined
whereis_yaterl_connection_mgr(Value) ->
    set_key(whereis_yaterl_connection_mgr, Value).

%% @doc: Get the current maxbytesline value (defaults to 80000)
%% @spec: () -> Value::integer()
yaterl_connection_maxbytesline() ->
    get_key(yaterl_connection_maxbytesline, 80000).

%% @doc: Set the current maxbytesline value 
%% @spec: (Value) -> ok
%% where
%%   Value = integer() | undefined
yaterl_connection_maxbytesline(Value) ->
    set_key(yaterl_connection_maxbytesline, Value).

%% @doc: Get the current yaterl_sup_mode (defaults to all_in_one)
%% @spec: () -> all_in_one | manager_only | stdio_connection_only
yaterl_sup_mode() ->
    get_key(yaterl_sup_mode, all_in_one).

%% @doc: Set the current yaterl_sup_mode
%% @spec: (Value) -> ok
%% where
%%   Value = all_in_one | manager_only | stdio_connection_only | undefined
yaterl_sup_mode(Mode) ->
    set_key(yaterl_sup_mode, Mode).

%% @doc: Get the current custom module name
%% @spec: () -> ModuleName::atom()
yaterl_custom_module_name() ->
    get_key(yaterl_custom_module, undefined).

%% @doc: Set the current custom module name
%% @spec: (Value::atom()) -> ok
yaterl_custom_module_name(Value) ->
    set_key(yaterl_custom_module, Value).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

get_key(Key, Default) ->
    case application:get_env(yaterl, Key) of
        undefined -> Default;
        {ok, CustomValue} -> CustomValue
    end.

set_key(Key, Value) ->
    application:set_env(yaterl, Key, Value).
