-module(yaterl_devtool).

-export([
         help/0,
         main/1,
         generate/2,
         pack/3,
         connect/1
        ]).

main(Args) ->
    io:format("INPUT: ~p~n", [Args]),
    getopt_parse_run(Args).

help() ->
    getopt_help().
    
generate(ProjectName, ProjectType) ->
    generate_dirtree(ProjectName, ProjectType),
    generate_files(ProjectName, ProjectType).

generate_dirtree(ProjectName, _ProjectType) ->
    filelib:ensure_dir(ProjectName++"/ebin/"),
    filelib:ensure_dir(ProjectName++"/src/"),
    filelib:ensure_dir(ProjectName++"/include/").

generate_files(ProjectName, ProjectType) ->
    TplVars = [{project_name, ProjectName},
            {project_type, ProjectType}],
    {ok, AppSrc} = yaterl_gen_mod_appsrc_dtl:render(TplVars),
    {ok, ModSrc} = yaterl_gen_mod_dtl:render(TplVars),
    BaseFileName = ProjectName++"/src/"++ProjectName,
    io:format("SOURCE: ~p~n", [AppSrc]),
    io:format("WRITING TO: ~s~n", [BaseFileName]),
    ok = file:write_file(BaseFileName ++ ".app.src", AppSrc),
    ok = file:write_file(BaseFileName ++ ".erl", ModSrc).

pack(ProjectName, ProjectType, MainModuleName) ->
    Files = load_files_to_pack(),
    case zip:create("mem", Files, [memory]) of
        {ok, {"mem", ZipBin}} ->
            write_zip_to_file(ProjectName, ProjectType, MainModuleName, ZipBin);
        {error, ZipError} ->
            io:format("ERROR: failed to pack yaterl_gen_mod project~n~p~n", [ZipError]),
            erlang:halt(2)
    end.

load_files_to_pack() ->
    load_files_to_pack("*", "ebin").

load_files_to_pack(Wildcard, Dir) ->
    [read_file_to_pack(Filename, Dir) || Filename <- filelib:wildcard(Wildcard, Dir)].

read_file_to_pack(Filename, Dir) ->
    {ok, Bin} = file:read_file(filename:join(Dir, Filename)),
    {Filename, Bin}.

write_zip_to_file(ProjectName, ProjectType, MainModuleName, ZipBin) ->
    MainModuleNameBin = list_to_binary(MainModuleName),
    ScriptData = case ProjectType of
                     "global" -> ProjectNameBin = list_to_binary(ProjectName),
                                 <<"#!/usr/bin/env escript\n",
                                   "%%!-sname ", MainModuleNameBin/binary, 
                                   " -noinput -escript main ", MainModuleNameBin/binary, "\n",
                                   ZipBin/binary>>;
                     "channel" -> <<"#!/usr/bin/env escript\n",
                                    "%%!-noinput -escript main ", MainModuleNameBin/binary, "\n",
                                    ZipBin/binary>>
                 end,
    case file:write_file(ProjectName, ScriptData) of
        ok ->
            io:format("SUCCESS: packed script to '~s'~n", [ProjectName]),
            [] = os:cmd("chmod a+x "++ProjectName);
        {error, WriteError} ->
            io:format("ERROR: failed to write yaterl_gen_mod packed script to file~n~p~n",
                      [WriteError]),
            erlang:halt(3)
    end.

connect(_Options) ->
    unimplemented.


%%%%%%%%%%%%%%%%%%%%%%%
%% PRIVATE FUNCTIONS %%
%%%%%%%%%%%%%%%%%%%%%%%

getopt_speclist() ->
    [
     {help,    $h, "help",    undefined, "Print this help"},
     {verbose, $v, "verbose", boolean,   "List all the actions executed"},
     {project_type, $t, "type", string, "Project type (global, channel)"}
    ].
    
getopt_help() ->
    getopt:usage(getopt_speclist(), "yaterl-devtool", "command parameters...",
             [{"command",   "Commands to be executed (e.g. generate, pack)"},
              {"parameters", "Command parameters"}]),
    io:format("Command examples:~n"),
    io:format("\tgenerate <ProjectName>~n"),
    io:format("\tpack <ExecutableName> [<MainModuleName>]~n").

getopt_parse_run(RawArgs) ->
    Result = getopt:parse(getopt_speclist(), RawArgs),
    {Opts, Args} = case Result of
        {ok, Value} ->
            Value;
        {error, {invalid_option, InvalidOption}} ->
            help(),
            io:format("ERROR: unknown option '~s'~n", [InvalidOption]),
            erlang:halt(1);
        {error, Reason} ->
            io:format("ERROR: ~p~n", [Reason]),
            erlang:halt(1)
    end,
    devtool_execute(Args, Opts).

devtool_execute([], _Opts) ->
    help();
devtool_execute(["help"|_Rest], _Opts) ->
    help();
devtool_execute(["generate", ProjectName], Opts) ->
    ProjectType = case proplists:lookup(project_type, Opts) of
                      none -> "global";
                      Value -> Value
                  end,
    generate(ProjectName, ProjectType);
devtool_execute(["generate", _ProjectName | _ ], Opts) ->
    help(),
    io:format("ERROR: too many arguments.~n");
devtool_execute(["pack", ProjectName], Opts) ->
    ProjectType = case proplists:lookup(project_type, Opts) of
                      none -> "global";
                      Value -> Value
                  end,
    MainModuleName = ProjectName,
    pack(ProjectName, ProjectType, MainModuleName);
devtool_execute(["pack", ProjectName, MainModuleName], Opts) ->
    ProjectType = case proplists:lookup(project_type, Opts) of
                      none -> "global";
                      Value -> Value
                  end,
    pack(ProjectName, ProjectType, MainModuleName);
devtool_execute(["pack", _ProjectName, _MainModuleName | _ ], Opts) ->
    help(),
    io:format("ERROR: too many arguments.~n");
devtool_execute(_, _) ->
    help(),
    io:format("ERROR: unknown command or invalid arguments.~n").

