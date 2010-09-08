%%%-------------------------------------------------------------------
%%% File    : yate_incoming_event_srv.erl
%%% Author  : rpl <>
%%% Description : 
%%%
%%% Created :  3 Sep 2010 by rpl <>
%%%-------------------------------------------------------------------
-module(yate_incoming_event_srv).

-behaviour(gen_server).

%% API
-export([
         start/1,
         start_link/1,
         run/1
        ]).

%% gen_server callbacks
-export([init/1, handle_cast/2, terminate/2, code_change/3]).

-record(state, {data, yate_event}).

-include("yate.hrl").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start(Data) ->
    gen_server:start(?MODULE, [Data], []).

start_link(Data) ->
    gen_server:start_link(?MODULE, [Data], []).

run(Pid) ->
    gen_server:cast(Pid, run).

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
init([Data]) ->
    {ok, #state{data=Data}}.

handle_cast(run, State) ->
    Data = State#state.data,
    YateEvent = yate_decode:from_binary(Data),
    processing_yate_event(YateEvent),
    {stop, normal, State}.

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

processing_yate_event(YateEvent) ->
    Type = YateEvent#yate_event.type,
    processing_by_type(Type, YateEvent).

%% TODO: processing set_local and error events
processing_by_type(watch, YateEvent) ->
    route_to_yate_subscribe_mgr(YateEvent);
processing_by_type(install, YateEvent) ->
    route_to_yate_subscribe_mgr(YateEvent);
processing_by_type(unwatch, YateEvent) ->
    route_to_yate_subscribe_mgr(YateEvent);
processing_by_type(uninstall, YateEvent) ->
    route_to_yate_subscribe_mgr(YateEvent);
processing_by_type(error, YateEvent) ->
    error_logger:info_msg("YATE ERROR: ~p~n", [YateEvent]);
processing_by_type(message, YateEvent) ->
    ResolvedRoute = resolve_custom_module(YateEvent),
    case ResolvedRoute of 
        unknown -> ok; %%% TODO: LOG AND EXIT            
        {ModuleName, SubscribeType} -> route_to_custom_module(YateEvent,
                                                              ModuleName, 
                                                              SubscribeType)
    end.

resolve_custom_module(YateEvent) ->
    ModuleName = yaterl_config:yate_custom_module_name(),
    SubscribeType = yate_subscribe_mgr:resolve_custom_module(YateEvent),
    {ModuleName, SubscribeType}.

route_to_yate_subscribe_mgr(YateEvent) ->
    YateSubscribeMgr = yaterl_config:yate_subscribe_mgr(),
    YateSubscribeMgr:handle_yate_event(YateEvent).

route_to_custom_module(YateEvent, InstallModule, install) ->
    %%% TODO: CALL, REPLY AND EXIT
    ct:pal("call"),
    route_to_install_module(YateEvent, InstallModule),
    ok;
route_to_custom_module(YateEvent, WatchModule, watch) ->
    %%% TODO: CAST AND EXIT
    ct:pal("cast"),
    route_to_watch_module(YateEvent, WatchModule),
    ok.

route_to_install_module(YateEvent, InstallModule) ->
    ReplyData = InstallModule:handle_install_message(YateEvent),
    YateConnectionMgr = yaterl_config:yate_connection_mgr(),
    YateConnectionMgr:send_binary_data(ReplyData).

route_to_watch_module(YateEvent, WatchModule) ->
    WatchModule:handle_watch_message(YateEvent).
