%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% YATE RECORD DEFINITIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @type yate_event() = tuple().
%%       ```#yate_event{
%%             type,
%%             direction,
%%             attrs,
%%             msgparams
%%          }'''
-record(yate_event, {
	  %%% direction: answer, incoming, outgoing
	  direction,
	  %%% direction: install, watch, message, etc.
	  type,
    %%% attrs: event attributes (name, retval, id etc.)
	  attrs,
	  %%% params: message key/value parameters
	  params
	 }).


-record(yate_channel, {direct, caller, callto}).

%% @type yate_exception() = tuple().
%%       ```#yate_exception{
%%             type,
%%             errmsg,
%%             data,
%%             where
%%          }'''
-record(yate_exception, {
	  type,
	  errmsg,
	  data,
	  where
	 }).

-define(THROW_YATE_EXCEPTION(Type, ErrMsg, Data), 
	throw(#yate_exception{type=Type, errmsg=ErrMsg, data={data, Data}, where={where, ?FILE, ?LINE} })
       ).

%%%%%%%%%%%%%%%%%%%
%%% YATE CONSTS %%%
%%%%%%%%%%%%%%%%%%%

%%% NOTE: 'Yate Engine -> External Application' Event Headers
-define(ERROR_IN_HDR, "Error in: ").
-define(INSTALL_ANSWER_HDR, "%%<install:").
-define(UNINSTALL_ANSWER_HDR, "%%<uninstall:").
-define(WATCH_ANSWER_HDR, "%%<watch:").
-define(UNWATCH_ANSWER_HDR, "%%<unwatch:").
-define(SETLOCAL_ANSWER_HDR, "%%<setlocal:").
-define(MESSAGE_ANSWER_HDR, "%%<message:").
-define(MESSAGE_INCOMING_HDR, "%%>message:").

%%% NOTE: 'External Application -> Yate Engine' Event Headers
-define(OUTPUT_HDR, "%%>output:").
-define(CONNECT_HDR, "%%>connect:").
-define(SETLOCAL_OUTGOING_HDR, "%%>setlocal:").
-define(INSTALL_OUTGOING_HDR, "%%>install:").
-define(UNINSTALL_OUTGOING_HDR, "%%>uninstall:").
-define(WATCH_OUTGOING_HDR, "%%>watch:").
-define(UNWATCH_OUTGOING_HDR, "%%>unwatch:").
-define(MESSAGE_OUTGOING_HDR, "%%>message:").
%%% NOTE: duplicated defines
%%%       -define(MESSAGE_ANSWER_HDR, "%%<message:").
%%%       -define(MESSAGE_OUTGOING_HDR, ?MESSAGE_INCOMING_HDR).
