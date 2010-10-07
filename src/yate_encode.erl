%% yate_encode: yate message encoding experimental erlang module.
%%
%% Copyright (C) 2009 - Alca Società Cooperativa <info@alcacoop.it>
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

%% @doc '{@module}' is a simple yate module to encode yate_event erlang records 
%%      to binary strings.
-module(yate_encode).

-export([to_binary/1]).

%% import yate_event record definition, and other utils (yate exceptions helpers)  
%% @headerfile "../include/yate.hrl"
-include("yate.hrl").

%% @doc encode a yate message from a yate_event record, or throw datainvalid yate_exception
%% @spec to_binary(YateEvt::yate_event()) -> binary()
to_binary(YateEvt) when is_record(YateEvt,yate_event) ->
    encode_yate_event(YateEvt).
%%to_binary(_Any) ->
%%    ?THROW_YATE_EXCEPTION(invalid_data, "Argument should be a yate_event record", _Any).

%% private function to convert a yate_event in a binary string by type and direction 
%% NOTE: only existent "Application to Engine" yate events have been implemented
encode_yate_event(#yate_event{type=message, direction=outgoing, attrs=Attrs, params=MsgParams}) ->
    << "%%>", "message", (encode_attributes(message_outgoing, Attrs))/binary,
     (encode_attributes(message_params, MsgParams))/binary>>;
encode_yate_event(#yate_event{type=message, direction=answer, attrs=Attrs, params=MsgParams}) ->
    << "%%<", "message", (encode_attributes(message_answer, Attrs))/binary,
     (encode_attributes(message_params, MsgParams))/binary>>;
encode_yate_event(#yate_event{type=Type, direction=outgoing, attrs=Attrs}) ->
    StringType = atom_to_list(Type),
    << "%%>", (list_to_binary(StringType))/binary, (encode_attributes(Type, Attrs))/binary>>;
encode_yate_event(YateEvt) ->
    ?THROW_YATE_EXCEPTION(invalid_application_event, "Invalid Application YATE Event", YateEvt).

%% fetch and encode attributes by type 
%%
%%% TODO: use dict:fetch and throw a YATE exception if attrs are invalid (not complete) 
%%%       or set a default value
encode_attributes(connect, Attrs) ->
    AttrsDict = dict:from_list(Attrs),
    Role = dict:fetch(role, AttrsDict),
    ChannelId = case (dict:find(channel_id, AttrsDict)) of
		  {ok, Value} -> [ Value ] ;
		  error -> [ ]
	      end,
    ChannelType = case (dict:find(channel_type, AttrsDict)) of
		  {ok, Value2} -> [ Value2 ];
		  error -> [ ]
	      end,
    join_event_chunks([Role] ++ ChannelId ++ ChannelType);
encode_attributes(install, Attrs) ->
    AttrsDict = dict:from_list(Attrs),
    Name = dict:fetch(name, AttrsDict),
    Filters = case (dict:find(filters, AttrsDict)) of
		  {ok, RawFilters} -> lists:flatmap(fun({X,Y}) -> [X, Y]  end, RawFilters);
		  error -> [ ]
	      end,
    case (dict:find(priority, AttrsDict)) of
	{ok, Priority} -> join_event_chunks([Priority, Name] ++ Filters);
	error -> join_event_chunks(["", Name] ++ Filters)
    end;
encode_attributes(uninstall, Attrs) ->
    encode_attributes(name_attr, Attrs);
encode_attributes(watch, Attrs) ->
    encode_attributes(name_attr, Attrs);
encode_attributes(unwatch, Attrs) ->
    encode_attributes(name_attr, Attrs);
encode_attributes(setlocal, Attrs) ->
    AttrsDict = dict:from_list(Attrs),
    Name = dict:fetch(name, AttrsDict),
    Value = dict:fetch(value, AttrsDict),
    join_event_chunks([Name, Value]);
encode_attributes(output, Attrs) ->
    AttrsDict = dict:from_list(Attrs),
    Value = dict:fetch(text, AttrsDict),
    join_event_chunks([Value]);
encode_attributes(message_outgoing, Attrs) ->
    AttrsDict = dict:from_list(Attrs),
    Id = dict:fetch(id, AttrsDict),
    Time = dict:fetch(time, AttrsDict),
    Name = dict:fetch(name, AttrsDict),
    RetValue = case (dict:find(retvalue, AttrsDict)) of
		   {ok, Value} -> Value;
		   error -> [ ]
	       end,
    join_event_chunks([Id, Time, Name, RetValue]);
encode_attributes(message_answer, Attrs) ->
    AttrsDict = dict:from_list(Attrs),
    Id = dict:fetch(id, AttrsDict),
    Processed = dict:fetch(processed, AttrsDict),
    Name = case (dict:find(name, AttrsDict)) of
	       {ok, NameValue} -> NameValue;
	       error -> [ ]
	   end,
    RetValue = case (dict:find(retvalue, AttrsDict)) of
		   {ok, Value} -> Value ;
		   error -> [ ]
	       end,
    join_event_chunks([Id, Processed, Name, RetValue]);
encode_attributes(message_params, MsgParams) ->
    % map and join message params with a '=' character
    FlatMsgParams = lists:flatmap(fun({X,Y}) -> 
					  [<<":">>,(encode_chunk(X,"=")),<<"=">>,(encode_chunk(Y, ":"))]  
				  end, MsgParams),
    << <<B/binary>> || B <- FlatMsgParams >>;
encode_attributes(name_attr, Attrs) ->
    % executed by all yate events with only the name attributes (uninstall, watch and unwatch)
    AttrsDict = dict:from_list(Attrs),
    Name = dict:fetch(name, AttrsDict),
    join_event_chunks([Name]).

%% join yate event chunks (with a ':' character) in a binary strean 
join_event_chunks([H]) ->
    << ":", (encode_chunk(H,""))/binary >>;
join_event_chunks([]) ->
    << >>;
join_event_chunks([H|T]) ->
    << ":", (encode_chunk(H,""))/binary, (join_event_chunks(T))/binary >>.

%% conver to binary single chunks
encode_chunk(C, Extra) when is_integer(C) ->
    list_to_binary(string_encode(integer_to_list(C),Extra));
encode_chunk(C, Extra) when is_list(C) ->
    list_to_binary(string_encode(C, Extra));
encode_chunk(C, Extra) when is_atom(C)->
    encode_chunk(atom_to_list(C), Extra);
encode_chunk(C, _Extra) when is_binary(C) ->
    C.

string_encode([H], Extra) ->
    char_encode([ H ], Extra);
string_encode([H|T], Extra) ->
    char_encode([ H ], Extra) ++ string_encode(T, Extra);
string_encode([], _Extra) ->
    [].

%%% TODO: insert YATE char encoding rules from docs
char_encode("%", _Extra) ->
    "%%";
char_encode(":", _Extra) ->
    [ $%, 64 + $: ];
char_encode([C], [C]) ->
    C1 = 64 + C,
    [ 37, C1 ];  %% NOTE: 37 = '%'
char_encode([ C ], _Extra) when C < 32 ->
    [ 37, 64 + C ];
char_encode([ C ], _Extra) when C >= 32 ->
    [ C ].
