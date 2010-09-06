%% yate_message: yate message helper module
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

%% @doc 'yate_message' is an helper module for yate message manipulation
-module(yate_message).

%% import yate_event record definition, and other utils (yate exceptions helpers)  
%% @headerfile "../include/yate.hrl"
-include("yate.hrl").

%% API
-export([
         new/0,
         new/1,
         new/2,
         new/3,

         id/1,
         time/1,
         name/1,
         retvalue/1,
         param/2,

         id/2,
         time/2,
         name/2,
         retvalue/2,
         param/3,
         
         is_processed/1,
         set_processed/1,
         set_processed/2,
         reply/1,
         reply/2
        ]).

%%====================================================================
%% API
%%====================================================================

%% @doc: Create a new empty yate message
%% @spec: () -> YateMessage::yate_event()
new() ->
    new("", [], "").

%% @doc: Create a named yate message
%% @spec: (Name::string()) -> YateMessage::yate_event()
new(Name) ->
    new(Name, [], "").

%% @doc: Create a populated yate message (empty retvalue)
%% @spec: (Name::string(), Params::List) -> YateMessage::yate_event()
%% where
%%   List = [Param]
%%   Param = {Key::atom(), Value::string()}
new(Name, Params) ->
    new(Name, Params, "").

%% @doc: Create a populated yate message
%% @spec: (Name::string(), Params::List, RetValue::string()) -> YateMessage::yate_event()
%% where
%%   List = [Param]
%%   Param = {Key::atom(), Value::string()}
new(Name, Params, RetValue) ->
    %% generate random id and set time
    RandomId = integer_to_list(round(math:pow(2, 48))),
    {Megaseconds,_,_} = erlang:now(),
    NowTime = integer_to_list(Megaseconds),
    #yate_event{
                 type=message,
                 direction=outgoing,
                 attrs=[
                        {id, RandomId},
                        {time, NowTime},
                        {name, Name},
                        {retvalue, RetValue}
                       ],
                 params=Params
               }.

%% @doc: Get yate message id
%% @spec: (YateMessage::yate_event()) -> Value::string()
id(Message) ->
    proplists:get_value(id, Message#yate_event.attrs).

%% @doc: Get yate message time
%% @spec: (YateMessage::yate_event()) -> Value::string()
time(Message) ->
    proplists:get_value(time, Message#yate_event.attrs).

%% @doc: Get yate message name
%% @spec: (YateMessage::yate_event()) -> Value::string()
name(Message) ->
    proplists:get_value(name, Message#yate_event.attrs).

%% @doc: Get yate message retvalue
%% @spec: (YateMessage::yate_event()) -> Value::string()
retvalue(Message) ->
    proplists:get_value(retvalue, Message#yate_event.attrs).

%% @doc: Get yate message parameter's value 
%% @spec: (Key::atom(), YateMessage::yate_event()) -> Value::string()
param(Key, Message) ->
    proplists:get_value(Key, Message#yate_event.params).

%% @doc: Change a yate message parameter's value 
%% @spec: (Key::atom(), Value::string(), YateMessage::yate_event()) -> 
%%        NewYateMessage::yate_event()
param(Key, Value, Message) ->
    change_message_param(Key, Value, Message).

%% @doc: Change a yate message id 
%% @spec: (Id::string(), YateMessage::yate_event()) -> 
%%        NewYateMessage::yate_event()
id(Id, Message) ->
    yate_event:change_event_attribute(id, Id, Message).

%% @doc: Change a yate message name
%% @spec: (Name::string(), YateMessage::yate_event()) -> 
%%        NewYateMessage::yate_event()
name(Name, Message) ->
    yate_event:change_event_attribute(name, Name, Message).

%% @doc: Change a yate message retvalue
%% @spec: (RetValue::string(), YateMessage::yate_event()) -> 
%%        NewYateMessage::yate_event()
retvalue(RetValue, Message) ->
    yate_event:change_event_attribute(retvalue, RetValue, Message).

%% @doc: Change a yate message time
%% @spec: (Time::string(), YateMessage::yate_event()) -> 
%%        NewYateMessage::yate_event()
time(Time, Message) ->
    yate_event:change_event_attribute(time, Time, Message).

%% @doc: Check if a yate message is processed
%% @spec: (YateMessage::yate_event()) -> true | false
is_processed(Message) ->
    proplists:get_value(processed, Message#yate_event.attrs) =:= "true".

%% @doc: Set a yate message processed flag to true
%% @spec: (YateMessage::yate_event()) -> NewYateMessage::yate_event()
set_processed(Message) ->
    yate_event:change_event_attribute(processed, "true", Message).

%% @doc: Set a yate message processed flag to "true"/true or "false"/false
%% @spec: (Value::Bool, YateMessage::yate_event()) -> NewYateMessage::yate_event()
%%
%% where
%%       Bool =  true | false 
set_processed("true", Message) ->
    yate_event:change_event_attribute(processed, "true", Message);
set_processed("false", Message) ->
    yate_event:change_event_attribute(processed, "false", Message);
set_processed(true, Message) ->
    yate_event:change_event_attribute(processed, "true", Message);
set_processed(false, Message) ->
    yate_event:change_event_attribute(processed, "false", Message).

reply(Message) ->
    Msg1 = Message#yate_event{direction=answer},
    set_processed(false, Msg1).

reply(Message, Processed) ->
    Msg1 = Message#yate_event{direction=answer},
    set_processed(Processed, Msg1).
    

%%====================================================================
%% Internal functions
%%====================================================================

%% INTERNAL: change parameter's value  
change_message_param(Key, NewValue, Message) ->
    NewAttrs = proplists:delete(Key, Message#yate_event.params),
    Message#yate_event{params=[ {Key, NewValue} | NewAttrs ]}.    
    
