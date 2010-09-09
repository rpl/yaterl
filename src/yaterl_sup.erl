
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

start_link(Mode) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Mode]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Mode]) ->
    {ok, { {one_for_one, 5, 10}, get_childs_spec(Mode) } }.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

get_childs_spec(manager_only) ->
    YaterlLoggerSpec = ?CHILD(yaterl_logger, worker),
    YateConnectionMgrSpec = ?CHILD(yate_connection_mgr, worker),
    YateSubscribeMgrSpec = ?CHILD(yate_subscribe_mgr, worker),
    [YaterlLoggerSpec, YateConnectionMgrSpec, YateSubscribeMgrSpec];
get_childs_spec(stdio_connection_only) ->
    YaterlLoggerSpec = ?CHILD(yaterl_logger, worker),
    YateStdioConnectionSpec = ?CHILD(yate_stdio_connection, worker),
    [YaterlLoggerSpec, YateStdioConnectionSpec];
get_childs_spec(all_in_one) ->
    SpecList = get_childs_spec(stdio_connection_only) ++ get_childs_spec(manager_only),
    ordspec:to_list(ordspec:from_list(SpecList)).
