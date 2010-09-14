%% yaterl_app: yaterl OTP application entry point
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

%% @doc '{@module}' is an OTP application entry point, it will start
%%      yaterl_sup supervisor as configured by yater_sup_mode config attribute.
-module(yaterl_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

%% @doc: <b>[APPLICATION CALLBACK]</b> Starts yaterl_sup supervisor as configured 
%%       by yaterl_sup_mode
start(_StartType, []) ->
    SupStartMode = yaterl_config:yaterl_sup_mode(),
    yaterl_sup:start_link(SupStartMode).

%% @doc: <b>[APPLICATION CALLBACK]</b> Stop OTP application
stop(_State) ->
    ok.
