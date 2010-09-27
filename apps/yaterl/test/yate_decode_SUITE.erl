-module(yate_decode_SUITE).

-compile(export_all).

-include_lib("ct.hrl").
-include("../include/yate.hrl").
-include("local_test_helpers.hrl").

all() ->
    [
     decode_nonbinary_data,
     decode_unknown_events,

     string_decode,

     decode_yate_errorin_incoming_event,

     decode_yate_install_answer_event,
     decode_invalid_yate_install_answer_event,

     decode_yate_uninstall_answer_event,
     decode_invalid_yate_uninstall_answer_event,

     decode_yate_watch_answer_event,
     decode_invalid_watch_answer_event,

     decode_yate_unwatch_answer_event,
     decode_invalid_unwatch_answer_event,

     decode_yate_setlocal_answer_event,
     decode_invalid_setlocal_answer_event,

     decode_yate_message_answer_event,
     decode_invalid_message_answer_event,

     decode_yate_message_incoming_event,
     decode_invalid_message_incoming_event,

     message_params_could_contains_equal_symbols,
     message_params_could_have_empty_value
    ].


decode_nonbinary_data(_Config) ->
    YateNonBinaryDataException = (catch yate_decode:from_binary(non_binary_data)),
    nonbinary_data = YateNonBinaryDataException#yate_exception.type.

decode_unknown_events(_Config) ->
    %%% NOTE: invalid direction of install message (incoming instead of answer or outgoing)
    YateUnknownEventException = (catch yate_decode:from_binary(<<"%%>install:100:call.route:true">>)),
    unknown_event = YateUnknownEventException#yate_exception.type.

string_decode(_Config) ->
    YateEventEncodedRetValue = #yate_event{
      type=message,
      direction=incoming,
      attrs=[{id, "generated_id_123"},{time, "12312312312"},{name, "call.route"},{retvalue, "test:encoding"}],
      params=[{chan_id, "sip/1"}]
     },
    YateEventEncodedRetValue = 
	yate_decode:from_binary(
	  <<"%%>message:generated_id_123:12312312312:call.route:test%zencoding:chan_id=sip/1">>
	 ),
    io:format("encodedmsgparam1"),
    YateEventEncodedMsgParam1 = #yate_event{
      type=message,
      direction=incoming,
      attrs=[{id, "generated_id_123"},{time, "12312312312"},{name, "call.route"},{retvalue, ""}],
      params=[{chan_id, "sip/1"},{comment, "test:encoding1"}]
     },
    YateEventEncodedMsgParam1 =	
	yate_decode:from_binary(
	  <<"%%>message:generated_id_123:12312312312:call.route::chan_id=sip/1:comment=test%zencoding1">>
	 ),
    YateEventEncodedMsgParam2 = YateEventEncodedMsgParam1#yate_event{
      params=[{chan_id, "sip/1"},{comment, "test=encoding2"}]
     },
    YateEventEncodedMsgParam2 =
	yate_decode:from_binary(
	  <<"%%>message:generated_id_123:12312312312:call.route::chan_id=sip/1:comment=test%}encoding2">>
	 ),
    YateEventEncodedMsgParam3 = YateEventEncodedMsgParam1#yate_event{
      params=[{chan_id, "sip/1"},{comment, "test%3"}]
     },
    YateEventEncodedMsgParam3 =
	yate_decode:from_binary(
	  <<"%%>message:generated_id_123:12312312312:call.route::chan_id=sip/1:comment=test%%3">>
	 ),
    YateEventEncodedMsgParam4 = YateEventEncodedMsgParam1#yate_event{
      params=[{chan_id, "sip/1"},{comment, [65,65,10,20,30]}]
     },
    YateEventEncodedMsgParam4 =
	yate_decode:from_binary(
	  <<"%%>message:generated_id_123:12312312312:call.route::chan_id=sip/1:comment=AA%J%T%^">>
	 ).

decode_yate_errorin_incoming_event(_Config) ->
    ExpectedValue = #yate_event{
      direction=incoming,
      type=error,
      attrs=[{msg,"Test YATE error event decoding"}]
     },
    ExpectedValue = yate_decode:from_binary(<<"Error in:Test YATE error event decoding">>).

decode_yate_install_answer_event(_Config) ->
    ExpectedValue = #yate_event{
      direction=answer,
      type=install,
      attrs=[{priority, "100"},{name, "call.route"},{success, "true"}]
     },
    ExpectedValue = yate_decode:from_binary(<<"%%<install:100:call.route:true">>).

decode_invalid_yate_install_answer_event(_Config) ->
    %%% NOTE: missing priority 
    YateInvalidDataException = (catch yate_decode:from_binary(<<"%%<install:call.route:true">>)),
    invalid_data = YateInvalidDataException#yate_exception.type.

decode_yate_uninstall_answer_event(_Config) ->
    ExpectedValue = #yate_event{
      direction=answer,
      type=uninstall,
      attrs=[{priority, "100"}, {name, "call.route"},{success, "true"}]
     },
    ExpectedValue = yate_decode:from_binary(<<"%%<uninstall:100:call.route:true">>).

decode_invalid_yate_uninstall_answer_event(_Config) ->
    %%% NOTE: missing success value
    YateInvalidDataException = (catch yate_decode:from_binary(<<"%%<uninstall:call.route">>)),
    invalid_data = YateInvalidDataException#yate_exception.type.

decode_yate_watch_answer_event(_Config) ->
    ExpectedValue = #yate_event{
      direction=answer,
      type=watch,
      attrs=[{name, "call.route"},{success, "true"}]
     },
    ExpectedValue = yate_decode:from_binary(<<"%%<watch:call.route:true">>).

decode_invalid_watch_answer_event(_Config) ->
    %%% NOTE: missing success value 
    YateInvalidDataException = (catch yate_decode:from_binary(<<"%%<watch:call.route">>)),
    invalid_data = YateInvalidDataException#yate_exception.type.

decode_yate_unwatch_answer_event(_Config) ->
    ExpectedValue = #yate_event{
      direction=answer,
      type=unwatch,
      attrs=[{name, "call.route"},{success, "true"}]
     },
    ExpectedValue = yate_decode:from_binary(<<"%%<unwatch:call.route:true">>).

decode_invalid_unwatch_answer_event(_Config) ->
    %%% NOTE: missing success value 
    YateInvalidDataException = (catch yate_decode:from_binary(<<"%%<unwatch:call.route">>)),
    invalid_data = YateInvalidDataException#yate_exception.type.

decode_yate_setlocal_answer_event(_Config) ->
    ExpectedValue = #yate_event{
      direction=answer,
      type=setlocal,
      attrs=[{name, "restart"},{value, "true"},{success, "true"}]
     },
    ExpectedValue = yate_decode:from_binary(<<"%%<setlocal:restart:true:true">>).

decode_invalid_setlocal_answer_event(_Config) ->
    %%% NOTE: missing setlocal and success value 
    YateInvalidDataException = (catch yate_decode:from_binary(<<"%%<setlocal:restart">>)),
    invalid_data = YateInvalidDataException#yate_exception.type.

decode_yate_message_answer_event(_Config) ->
    ExpectedValue = #yate_event{
      direction=answer,
      type=message,
      attrs=[{id, "messageid001"},{processed, "true"},{name, "call.route"},{retvalue, "retvalue"}],
      params=[{chan_id, "sip/1"},{target_id, "sip/2"}]
     },
    ExpectedValue = yate_decode:from_binary(<<"%%<message:messageid001:true:call.route:retvalue:chan_id=sip/1:target_id=sip/2">>).

decode_invalid_message_answer_event(_Config) ->
    %%% NOTE: answer message missing a lot of data 
    YateInvalidDataException = (catch yate_decode:from_binary(<<"%%<message:call.route">>)),
    invalid_data = YateInvalidDataException#yate_exception.type,
%%% NOTE: Invalid message parameter (need '=' key/value separator)
    YateInvalidDataException2 = (catch yate_decode:from_binary(<<"%%<message:messageid001:true:call.route">>)),
    invalid_data = YateInvalidDataException2#yate_exception.type.

decode_yate_message_incoming_event(_Config) ->
    ExpectedValue = #yate_event{
      direction=incoming,
      type=message,
      attrs=[{id, "messageid001"},{time, "123456789"},{name, "call.route"},{retvalue, "retvalue"}],
      params=[{chan_id, "sip/1"},{target_id, "sip/2"}]
     },
    ExpectedValue = yate_decode:from_binary(<<"%%>message:messageid001:123456789:call.route:retvalue:chan_id=sip/1:target_id=sip/2">>).

decode_invalid_message_incoming_event(_Config) ->
    %%% NOTE: incoming message missing a lot of data 
    YateInvalidDataException = (catch yate_decode:from_binary(<<"%%>message:call.route">>)),
    invalid_data = YateInvalidDataException#yate_exception.type,
    %%% NOTE: Invalid message parameter (need '=' key/value separator)
    YateInvalidDataException2 = (catch yate_decode:from_binary(<<"%%>message:messageid001:123456789:call.route">>)),
    invalid_data = YateInvalidDataException2#yate_exception.type.

message_params_could_contains_equal_symbols(_Config) ->
    ExpectedValue = #yate_event{
      direction=incoming,
      type=message,
      attrs=[{id, "messageid001"},{time, "123456789"},{name, "call.route"},{retvalue, "retvalue"}],
      params=[{chan_id, "sip/1"},{target_id, "sip/2"},{fake_param, "key=value,key2=value2"}]
     },
    ExpectedValue = yate_decode:from_binary(<<"%%>message:messageid001:123456789:call.route:retvalue:chan_id=sip/1:target_id=sip/2:fake_param=key=value,key2=value2">>).

message_params_could_have_empty_value(_Config) ->
    ExpectedValue = #yate_event{
      direction=incoming,
      type=message,
      attrs=[{id,"1231243124"},
             {time,"123456789"},
             {name,"call.route"},
             {retvalue,"retvalue"}],
      params=[{fake_param,[]}]},

    ExpectedValue = (catch yate_decode:from_binary(<<"%%>message:1231243124:123456789:call.route:retvalue:fake_param">>)).
    
