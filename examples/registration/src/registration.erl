-module(registration).

-behaviour(yaterl_gen_mod).

-export([
         connection_available/0,
         subscribe_completed/0,
         subscribe_error/2,
         handle_install_message/1,
         handle_watch_message/1,
         main/1
        ]).

connection_available() ->
    %% NOTE:
    %% [{setlocal, "param", "value"},
    %%  {"first.message", watch},
    %%  {"second.message", install, 80},
    %%  {"third.message", install, 80, {filters, [{"param", "value"}]}}]
    ConfigList = [{"user.auth", install, "10"}],
    yaterl_gen_mod:start_subscribe_sequence(ConfigList).

subscribe_completed() ->
    error_logger:info_msg("SUBSCRIBING COMPLETED").

subscribe_error(_LastResponse, _LastRequest) ->
    error_logger:error_msg("SUBSCRIBE ERROR... EXITING"),
    init:stop(1).

handle_install_message(YateMessage) ->
    case {yate_message:name(YateMessage), yate_event:direction(YateMessage)} of
        {"user.auth", incoming} ->
            %% NOTE: you have to return a reply/ack
            %%   yaterl_gen_mod:reply(YateMessage)
            %%   yaterl_gen_mod:ack(YateMessage)
            Username = yate_message:param(username, YateMessage),
            Password = get_user_password(Username),
            YateMessage2 = yate_message:retvalue(Password, YateMessage),
            yaterl_gen_mod:reply(YateMessage2);
        _AnyOther -> log(YateMessage)
    end.

handle_watch_message(_YateMessage) ->
    %% NOTE: return value ignored
    ok.

log(YateMessage) ->
    error_logger:info_msg("IGNORED Msg: ~p~n", [YateMessage]),
    yaterl_gen_mod:ack(YateMessage).

%%% NOTE: escript entry point    
main(_) ->
    config(),
    ok = application:start(sasl),
    ok = application:start(yaterl),
    timer:sleep(infinity).

%%% NOTE: logging and yaterl configurations
config() ->
    LogDir = "/tmp/",
    BaseLogFileName = [LogDir,atom_to_list(?MODULE),"_",os:getpid()],
    yaterl_config:log_level(error),
    yaterl_config:log_files(BaseLogFileName++".log", 
                            BaseLogFileName++"_sasl.log"),

    yaterl_config:yaterl_custom_module_name(?MODULE),
    ok.


%%%%%%%%%%%%%%
%% INTERNALS
%%%%%%%%%%%%%%

get_user_password(Password) ->
    "test123".
