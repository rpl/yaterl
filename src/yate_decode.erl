%% yate_decode: yate message decoding experimental erlang module.
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

%% @doc 'yate_decode' is a simple yate module to decode yate_event erlang records from binary strings.
-module(yate_decode).

%% @headerfile "../include/yate.hrl"
-include("yate.hrl").

-export([from_binary/1]).

%% @doc decode message type and call specialized attributes parsing functions
%% @spec from_binary(Raw::binary()) -> yate_event()
from_binary(Raw = <<"Error in:", Msg/binary>>) when is_binary(Raw) ->
    #yate_event{ direction=incoming, type=error, 
		 attrs=apply_string_decode_on_values([{msg, binary_to_list(Msg)}]) 
		};
from_binary(Raw = <<"%%<install:", Rest/binary>>) when is_binary(Raw) -> 
    #yate_event{ direction=answer, type=install, 
		 attrs=apply_string_decode_on_values(decode_attributes(install_answer, Rest)) 
		};
from_binary(Raw = <<"%%<uninstall:", Rest/binary>>) when is_binary(Raw) -> 
    #yate_event{ direction=answer, type=uninstall, 
		 attrs=apply_string_decode_on_values(decode_attributes(uninstall_answer, Rest)) 
		};
from_binary(Raw = <<"%%<watch:", Rest/binary>>) when is_binary(Raw) -> 
    #yate_event{ direction=answer, type=watch, 
		 attrs=apply_string_decode_on_values(decode_attributes(watch_answer, Rest)) 
		};
from_binary(Raw = <<"%%<unwatch:", Rest/binary>>) when is_binary(Raw) -> 
    #yate_event{ direction=answer, type=unwatch, 
		 attrs=apply_string_decode_on_values(decode_attributes(unwatch_answer, Rest)) 
		};
from_binary(Raw = <<"%%<setlocal:", Rest/binary>>) when is_binary(Raw) -> 
    #yate_event{ direction=answer, type=setlocal, 
		 attrs=apply_string_decode_on_values(decode_attributes(setlocal_answer, Rest)) 
		};
from_binary(Raw = <<"%%<message:", Rest/binary>>) when is_binary(Raw) -> 
    [ EventAttrs, MsgParams ] = decode_attributes(message_answer, Rest),
    #yate_event{ direction=answer, type=message, 
		 attrs=apply_string_decode_on_values(EventAttrs), 
		 params=apply_string_decode_on_values(MsgParams) 
		};
from_binary(Raw = <<"%%>message:", Rest/binary>>) when is_binary(Raw) ->
    [ EventAttrs, MsgParams ] = decode_attributes(message_incoming, Rest),
    #yate_event{ direction=incoming, type=message, 
		 attrs=apply_string_decode_on_values(EventAttrs), 
		 params=apply_string_decode_on_values(MsgParams) 
		};
from_binary(_Unknown) when is_binary(_Unknown) ->
    ?THROW_YATE_EXCEPTION(unknown_event, "Invalid Engine YATE Event", _Unknown);
from_binary(_Unknown) ->
    ?THROW_YATE_EXCEPTION(nonbinary_data, "Needs binary data", _Unknown).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TODO: 'binary:split' seems to be more efficient way to split a binary string
%%%         http://www.erlang.org/eeps/eep-0009.html
%%%         http://stackoverflow.com/questions/428124/how-can-i-split-a-binary-in-erlang
%%% NOTE: private specialized attributes parsing functions
%%%       on error throws 
%%%  { invalid_data, { data, Data }, { where, File, Line } }
%%%       unimplemented features throws
%%%  { not_implemented, {data, Rest}, { where, ?FILE, ?LINE } }
%%%
%%% IMPLEMENTATION NOTES:
% throw({ not_implemented, {data, Rest}, { where, ?FILE, ?LINE } }).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode_attributes(install_answer, Rest) when is_binary(Rest) ->
    case binary_split(Rest, ":") of
	[ Priority, Name, Success ] -> [ { priority, Priority }, { name, Name }, { success, Success } ];
        _Any -> ?THROW_YATE_EXCEPTION(invalid_data, "Error parsing install answer attributes", _Any)
%% throw({ invalid_data, {data, _Any}, { where, ?FILE, ?LINE } })
    end;
decode_attributes(uninstall_answer, Rest) when is_binary(Rest) ->
    case binary_split(Rest, ":") of
        [ Priority, Name, Success ] -> [ {priority, Priority}, { name, Name }, { success, Success } ];
        _Any -> ?THROW_YATE_EXCEPTION(invalid_data, "Error parsing uninstall answer attributes", _Any)
    end;
decode_attributes(watch_answer, Rest) when is_binary(Rest) ->
    case binary_split(Rest, ":") of
	[ Name, Success ] -> [ { name, Name }, { success, Success } ];
        _Any -> ?THROW_YATE_EXCEPTION(invalid_data, "Error parsing watch answer attributes", _Any)
    end;
decode_attributes(unwatch_answer, Rest) when is_binary(Rest) ->
    case binary_split(Rest, ":") of
	[ Name, Success ] -> [ { name, Name }, { success, Success } ];
        _Any -> ?THROW_YATE_EXCEPTION(invalid_data, "Error parsing unwatch answer attributes", _Any)
    end;
decode_attributes(setlocal_answer, Rest) when is_binary(Rest) ->
    case binary_split(Rest, ":") of
	[ Name, Value, Success ] -> [ { name, Name }, { value, Value }, { success, Success } ];
        _Any -> ?THROW_YATE_EXCEPTION(invalid_data, "Error parsing setlocal answer attributes", _Any)
    end;

%%% %%<message:<id>:<processed>:[<name>]:<retvalue>[:<key>=<value>...]
%%% TODO: name is optional verify with a test
decode_attributes(message_answer, Rest) when is_binary(Rest) ->
    case binary_split(Rest, ":") of
	[ Id, Processed, Name, RetVal | RawMsgParams ] -> 
	    Attrs = [ { id, Id }, { processed, Processed }, { name, Name }, { retvalue, RetVal }],
	    MsgParams = decode_attributes(message_parameters, RawMsgParams),
	    [Attrs, MsgParams];
        _Any -> ?THROW_YATE_EXCEPTION(invalid_data, "Error parsing answer message attributes", _Any)
    end;

%%% %%>message:<id>:<time>:<name>:<retvalue>[:<key>=<value>...]
decode_attributes(message_incoming, Rest) when is_binary(Rest) ->
    case binary_split(Rest, ":") of
	[ Id, Time, Name, RetVal | RawMsgParams ] -> 
	    Attrs = [ { id, Id }, { time, Time }, { name, Name }, { retvalue, RetVal }],
	    MsgParams = decode_attributes(message_parameters, RawMsgParams),
	    [Attrs, MsgParams];
        _Any -> ?THROW_YATE_EXCEPTION(invalid_data, "Error parsing incoming message attributes", _Any)
    end;

%%% NOTE: list_to_atom can be a problem if the system will be flooded by a lot of different keys
decode_attributes(message_parameters, [H|T] = RawMsgParams) when is_list(RawMsgParams) ->
    MsgParam = case binary_split(H, "=") of
		   [Key | Rest] -> { list_to_atom(string_decode(Key)), binary_join(Rest, <<"=">>) };
		   _Any -> ?THROW_YATE_EXCEPTION(invalid_data, "Error parsing message parameters", _Any)
	       end,
    [MsgParam | decode_attributes(message_parameters, T)];
decode_attributes(message_parameters, []) ->
    [].


%% FROM yate-extmodule-doc
%% Any value that contains special characters (ASCII code lower than 32) MUST have them converted
%% to %<upcode> where <upcode> is the character with a numeric value equal with 64 + original ASCII code. 
%% The % character itself MUST be converted to a special %% representation. Characters with codes higher 
%% than 32 (except %) SHOULD not be escaped but may be so. A %-escaped code may be received instead of 
%% an unescaped character anywhere except in the initial keyword or the delimiting colon (:) characters. 
%% Anywhere in the line except the initial keyword a % character not followed by a character with a numeric 
%% value higher than 64 (40H, 0x40, '@') or another % is an error.

apply_string_decode_on_values(KVList) ->
    lists:keymap(fun string_decode/1, 2, KVList).

string_decode(B) when is_binary(B) ->
    string_decode(binary_to_list(B));
string_decode([]) ->
    "";
string_decode([H]) ->
    [ H ];
string_decode([H1,H2| T]) ->
    { NewH, NewT } = case ([H1,H2]) of
			 [ $%, $% ] -> {[ $% ], T};
			     
			 [ $%, C ] when C > 64 -> {[ C - 64 ], T}; 
    %%%                  [ $%, C ] when C < 64 -> ERROR;
	                 [ C1, C2 ] -> {[ C1 ], [ C2 | T ]}
		     end,
    NewH ++ string_decode(NewT).

%%% Binary split from: http://stackoverflow.com/questions/428124/how-can-i-split-a-binary-in-erlang
binary_split(Binary, Chars) ->
    binary_split(Binary, Chars, 0, 0, []).

binary_split(Bin, Chars, Idx, LastSplit, Acc)
  when is_integer(Idx), is_integer(LastSplit) ->
    Len = (Idx - LastSplit),
    case Bin of
        <<_:LastSplit/binary,
         This:Len/binary,
         Char,
         _/binary>> ->
            case lists:member(Char, Chars) of
                false ->
                    binary_split(Bin, Chars, Idx+1, LastSplit, Acc);
                true ->
                    binary_split(Bin, Chars, Idx+1, Idx+1, [This | Acc])
            end;
        <<_:LastSplit/binary,
         This:Len/binary>> ->
            lists:reverse([This | Acc]);
        _ ->
            lists:reverse(Acc)
    end.

binary_join([H], _Char) ->
    << H/binary >>;
binary_join([], _Char) ->
    << >>;
binary_join([H|T], Char) ->
    << (binary_join([H],Char))/binary, Char/binary, (binary_join(T,Char))/binary >>.
