%% yaterl_stdio_connection: yaterl stdio connection server
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

%% @doc '{@module}' is a gen_server erlang process that 
%%      will be spawned internally by yate_connection_mgr to process
%%      incoming data.
-module(yaterl_incoming_event_srv).

-behaviour(gen_server).

%% API
-export([
         start/1,
         run/1,
         connection_available/1,
         subscribe_completed/1,
         subscribe_error/3
        ]).

%% gen_server callbacks
-export([init/1, handle_cast/2, terminate/2, code_change/3]).

-record(state, {data, yate_event}).

-include("yate.hrl").

%%====================================================================
%% API
%%====================================================================

%% @doc: Starts the server
%% @spec: (Data::binary()) -> {ok,Pid} | ignore | {error,Error}
start(Data) ->
    gen_server:start(?MODULE, [Data], []).

%% @doc: Run yate event processing
%% @spec: (Pid::pid()) -> ok
run(Pid) ->
    gen_server:cast(Pid, run).

subscribe_error(Pid, LastRequest, LastReceived) ->
    gen_server:cast(Pid, {subscribe_error, LastRequest, LastReceived}).

subscribe_completed(Pid) ->
    gen_server:cast(Pid, subscribe_completed).

connection_available(Pid) ->
    gen_server:cast(Pid, connection_available).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc: <b>[GEN_SERVER CALLBACK]</b> Initiates the server
%% @spec: ([]) -> {ok, State} | {ok, State, Timeout} | ignore | {stop, Reason}
init([Data]) ->
    {ok, #state{data=Data}}.

%% @doc: <b>[GEN_SERVER CALLBACK]</b> Handling cast messages
%%       
%% @see run/0
handle_cast(run, State) ->
    Data = State#state.data,
    YateEvent = yate_decode:from_binary(Data),
    processing_yate_event(YateEvent),
    {stop, normal, State};
handle_cast(connection_available, State) ->
    yaterl_tracer:add_message("YATE","yaterl_gen_mod","connection_available"),
    CustomModule = yaterl_config:yaterl_custom_module_name(),
    CustomModule:connection_available(),
    {stop, normal, State};
handle_cast(subscribe_completed, State) ->
    yaterl_tracer:add_message("YATE","yaterl_gen_mod","subscribe_completed"),
    CustomModule = yaterl_config:yaterl_custom_module_name(),
    CustomModule:subscribe_completed(),
    {stop, normal, State};
handle_cast({subscribe_error, LastRequest, LastResponse}, State) ->
    yaterl_logger:error_msg("SUBSCRIBE ERROR:~nlast_request=~p~nlast_received=~p~n",
                            [LastRequest, LastResponse]),
    yaterl_tracer:add_message("yaterl_subscribe_mgr","yaterl_gen_mod","subscribe_error"),
    %%% TODO: add a note
    CustomModule = yaterl_config:yaterl_custom_module_name(),
    CustomModule:subscribe_error(LastRequest, LastResponse),
    {stop, normal, State}.

%% @doc: <b>[GEN_SERVER CALLBACK]</b> Handling terminate sequence. 
%% It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% 
%% @spec: (_Reason, _State) -> ok
terminate(Reason, State) ->
    case Reason of
        normal -> ok;
        _ ->
            Text = io_lib:format("\nReason: ~p\nState: ~p\n", [Reason, State]),
            yaterl_tracer:add_note("YATE #FF0000", "left", ["<b>terminate yaterl_incoming_event_srv ", Text, "</b>"]),
            yaterl_tracer:add_message("YATE","YATE", "ack_before_die"),
            ack_yate_message_before_die(State), 
            ok
    end.

%% @doc: <b>[GEN_SERVER CALLBACK]</b> Convert process state when code is changed
%% @spec: (OldVsn, State, Extra) -> {ok, NewState}
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

ack_yate_message_before_die(State) ->
    YateEvent = yate_decode:from_binary(State#state.data),
    
    {_HandlerModule, SubscribeType} = resolve_custom_module(YateEvent),
    Direction = yate_event:direction(YateEvent),
    case {SubscribeType, Direction} of
        {install, incoming} -> yaterl_logger:warning_msg("ACK MESSAGE BEFORE DIE: ~p~n", 
                                         [State#state.data]),
                               Ack = yate_message:reply(YateEvent),
                               Data = yate_encode:to_binary(Ack),
                               yaterl_connection_mgr:send_binary_data(Data);
        _ -> ok
    end.

processing_yate_event(YateEvent) ->
    Type = YateEvent#yate_event.type,
    processing_by_type(Type, YateEvent).

%% TODO: processing set_local and error events
processing_by_type(watch, YateEvent) ->
    route_to_yaterl_subscribe_mgr(YateEvent);
processing_by_type(install, YateEvent) ->
    route_to_yaterl_subscribe_mgr(YateEvent);
processing_by_type(setlocal, YateEvent) ->
    route_to_yaterl_subscribe_mgr(YateEvent);
processing_by_type(unwatch, YateEvent) ->
    route_to_yaterl_subscribe_mgr(YateEvent);
processing_by_type(uninstall, YateEvent) ->
    route_to_yaterl_subscribe_mgr(YateEvent);
processing_by_type(error, YateEvent) ->
    yaterl_logger:error_msg("RECEIVED YATE ERROR: ~p~n", [YateEvent]);
processing_by_type(message, YateEvent) ->
    ResolvedRoute = resolve_custom_module(YateEvent),
    case ResolvedRoute of 
        {ModuleName, unknown} -> 
            route_to_custom_module(install, ModuleName, YateEvent);
            %%% TODO: LOG       
        {ModuleName, SubscribeType} -> route_to_custom_module(SubscribeType,
                                                              ModuleName, 
                                                              YateEvent)
    end.

resolve_custom_module(YateEvent) ->
    ModuleName = yaterl_config:yaterl_custom_module_name(),
    SubscribeType = yaterl_subscribe_mgr:resolve_custom_module(YateEvent),
    {ModuleName, SubscribeType}.

route_to_yaterl_subscribe_mgr(YateEvent) ->
    yaterl_tracer:add_note("YATE", "left", io_lib:format("~p", [YateEvent])),
    yaterl_tracer:add_message("YATE","yaterl_subscribe_mgr","handle_yate_event"),
    yaterl_subscribe_mgr:handle_yate_event(YateEvent).

route_to_custom_module(install, InstallModule, YateEvent) ->
    yaterl_logger:info_msg("call custom handler"),
    yaterl_tracer:add_note("YATE", "left", io_lib:format("~p", [YateEvent])),
    yaterl_tracer:add_message("YATE","yaterl_gen_mod","handle_install_message"),
    route_to_install_module(YateEvent, InstallModule),
    ok;
route_to_custom_module(watch, WatchModule, YateEvent) ->
    yaterl_logger:info_msg("cast custom handler"),
    yaterl_tracer:add_note("YATE", "left", io_lib:format("~p", [YateEvent])),
    yaterl_tracer:add_message("YATE","yaterl_gen_mod","handle_watch_message"),
    route_to_watch_module(YateEvent, WatchModule),
    ok.

route_to_install_module(YateEvent, InstallModule) ->
    InstallHandlerReply = InstallModule:handle_install_message(YateEvent),
    case InstallHandlerReply of
        {yate_binary_reply, ReplyData} -> 
            yaterl_tracer:add_message("yaterl_gen_mod","YATE","send"),
            yaterl_connection_mgr:send_binary_data(ReplyData);
        AnyOther -> yaterl_logger:info_msg("IGNORED InstallHandlerReply: ~p~n", [AnyOther])
    end.

route_to_watch_module(YateEvent, WatchModule) ->
    WatchModule:handle_watch_message(YateEvent).
