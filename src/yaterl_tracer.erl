-module(yaterl_tracer).

-behaviour(gen_server).

%% API
-export([start_link/0, start_trace/0, stop_trace/0, add_message/3, add_note/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER,?MODULE).

-record(state, {sequence_diagram}).

-record(sequence_diagram, {name, title, file}).

%%% API

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_trace() ->
    case yaterl_config:tracing_enabled() of
        false -> ok;
        true ->
            {Name, Title, Filename} = yaterl_config:tracing_config(),
            gen_server:call(?SERVER, {start_trace,Name,Title,Filename})
    end.

stop_trace() ->
    case yaterl_config:tracing_enabled() of
        false -> ok;
        true -> gen_server:call(?SERVER, stop_trace)
    end.

add_message(Src,Dst,Msg) ->
    case yaterl_config:tracing_enabled() of
        false -> ok;
        true -> gen_server:cast(?SERVER, {add_message,Src,Dst,Msg})
    end.

add_note(Ref,Side,Txt) ->
    case yaterl_config:tracing_enabled() of
        false -> ok;
        true -> gen_server:cast(?SERVER, {add_note,Ref,Side,Txt})
    end.

%%% OTP CALLBACKS

init([]) ->
    {ok, #state{}}.

handle_call({start_trace,Name,Title,Filename}, _From, State) ->
    SequenceDiagram = sequence_diagram_open(Name,Title,Filename),
    NewState = State#state{sequence_diagram=SequenceDiagram},
    {reply, ok, NewState};
handle_call(stop_trace, _From, State) ->
    sequence_diagram_close(State#state.sequence_diagram),
    NewState = State#state{sequence_diagram=undefined},
    {reply, ok, NewState}.

handle_cast({add_message,Src,Dst,Msg}, State) ->
    sequence_diagram_write(State#state.sequence_diagram,
                           [Src," -> ",Dst,": ",Msg,"\n"]),
    {noreply, State};
handle_cast({add_note,Ref,Side,Txt}, State) ->
    sequence_diagram_write(State#state.sequence_diagram,
                           ["\nnote ",Side," of ",Ref,"\n",
                            Txt,"\nend note\n\n"]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    sequence_diagram_close(State#state.sequence_diagram),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions

sequence_diagram_open(Name, Title, Filename) ->
    {ok, File} = file:open(Filename, [write]),
    ok = file:write(File, ["@startuml ",Name,".png\n"]),
    ok = file:write(File, ["\ntitle ",Title,"\n"]),
    #sequence_diagram{name=Name, title=Title, file=File}.
    
sequence_diagram_write(undefined, _Txt) ->
    ignored;
sequence_diagram_write(_SequenceDiagram=#sequence_diagram{file=File}, Txt) ->
    ok = file:write(File, Txt).

sequence_diagram_close(undefined) ->
    ignored;
sequence_diagram_close(SequenceDiagram=#sequence_diagram{file=File}) ->
    sequence_diagram_write(SequenceDiagram, "\n@enduml\n"),
    ok = file:close(File).
    
