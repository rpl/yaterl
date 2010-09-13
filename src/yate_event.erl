%% yate_event: yate event helper module
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

%% @doc 'yate_event' is an helper module for yate event manipulation
-module(yate_event).

%% import yate_event record definition, and other utils (yate exceptions helpers)  
%% @headerfile "../include/yate.hrl"
-include("yate.hrl").

%% API
-export([
         new/1,
         new/2,

         is_watch/1,
         is_install/1,
         is_unwatch/1,
         is_uninstall/1,
         is_setlocal/1,
         is_message/1,
         is_error/1,

         type/1,
         type/2,
         direction/1,
         direction/2,

         attr/2,
         attr/3,
         attrs/1,
         attrs/2
        ]).

%% NOTE: private function used in yate_message helpers
-export([change_event_attribute/3]).

%%====================================================================
%% API
%%====================================================================

%% @doc: Create a new yate event of 'EventType' type
%% @spec: (EventType::atom()) -> YateEvent::yate_event()
new(EventType) ->
    new(EventType, []).

%% @doc: Create a new yate event of 'EventType' type and 'Attrs' attributes
%% @spec: (EventType::atom(), Attrs::List) -> YateEvent::yate_event()
%% where
%%   List = [Attr]
%%   Attr = {Key::atom(), Value::string()}
new(EventType, Attrs) ->
    #yate_event{
     type=EventType,
     direction=outgoing,
     attrs=Attrs,
     params=[]
    }.

%% @doc: Test for watch event
%% @spec: (YateEvent::yate_event()) -> true | false
is_watch(YateEvent) ->
    YateEvent#yate_event.type =:= watch.

%% @doc: Test for install event
%% @spec: (YateEvent::yate_event()) -> true | false
is_install(YateEvent) ->
    YateEvent#yate_event.type =:= install.

%% @doc: Test for unwatch event
%% @spec: (YateEvent::yate_event()) -> true | false
is_unwatch(YateEvent) ->
    YateEvent#yate_event.type =:= unwatch.

%% @doc: Test for uninstall event
%% @spec: (YateEvent::yate_event()) -> true | false
is_uninstall(YateEvent) ->
    YateEvent#yate_event.type =:= uninstall.

%% @doc: Test for setlocal event
%% @spec: (YateEvent::yate_event()) -> true | false
is_setlocal(YateEvent) ->
    YateEvent#yate_event.type =:= setlocal.

%% @doc: Test for message event
%% @spec: (YateEvent::yate_event()) -> true | false
is_message(YateEvent) ->
    YateEvent#yate_event.type =:= message.

%% @doc: Test for error event
%% @spec: (YateEvent::yate_event()) -> true | false
is_error(YateEvent) ->
    YateEvent#yate_event.type =:= error.

%% @doc: Get type from a yate event
%% @spec: (YateEvent::yate_event()) -> 
%%            watch | unwatch | install | uninstall | setlocal | 
%%            output | error | message
type(YateEvent) ->
    YateEvent#yate_event.type.

%% @doc: Change yate event type
%% @spec: (Value::string(), YateEvent::yate_event()) -> 
%%        NewYateEvent::yate_event()
type(Value, YateEvent) ->
    YateEvent#yate_event{type=Value}.

%% @doc: Get attribute value from a yate event
%% @spec: (YateEvent::yate_event()) -> incoming | answer | outgoing
direction(YateEvent) ->
    YateEvent#yate_event.direction.

%% @doc: Change yate event direction
%% @spec: (Value::string(), YateEvent::yate_event()) -> 
%%        NewYateEvent::yate_event()
direction(Value, YateEvent) ->
    YateEvent#yate_event{direction=Value}.

%% @doc: Get attribute value from a yate event
%% @spec: (Key::atom(), YateEvent::yate_event()) -> Value::string()
attr(Key, YateEvent) ->
    proplists:get_value(Key, YateEvent#yate_event.attrs).

%% @doc: Change an attribute value from a yate event
%% @spec: (Key::atom(), Value::string(), YateEvent::yate_event()) -> 
%%        NewYateEvent::yate_event()
attr(Key, Value, YateEvent) ->
    change_event_attribute(Key, Value, YateEvent).

%% @doc: get all attributes from a yate event
%% @spec: (YateEvent::yate_event()) -> Attrs
%%   
%% where
%%   Attrs = [Attr]
%%   Attr = {Key::atom(), Value::string()}
attrs(YateEvent) ->
    YateEvent#yate_event.attrs.

%% @doc: Change all attributes from a yate event
%% @spec: (NewAttrs::Attrs, YateEvent::yate_event()) -> 
%%        NewYateEvent::yate_event()
%% where
%%   Attrs = [Attr]
%%   Attr = {Key::atom(), Value::string()}
attrs(NewAttrs, YateEvent) ->
    YateEvent#yate_event{attrs = NewAttrs}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc <b>[INTERNAL API]</b>: change attribute's value
change_event_attribute(Key, NewValue, YateEvent) ->
    NewAttrs = proplists:delete(Key, YateEvent#yate_event.attrs),
    YateEvent#yate_event{attrs=[ {Key, NewValue} | NewAttrs ]}.    
