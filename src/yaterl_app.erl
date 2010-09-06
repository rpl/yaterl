-module(yaterl_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, []) ->
    SupStartMode = yaterl_config:yaterl_sup_mode(),
    yaterl_sup:start_link(SupStartMode).

stop(_State) ->
    ok.
