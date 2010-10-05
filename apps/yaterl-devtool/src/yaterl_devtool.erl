-module(yaterl_devtool).

-export([
         help/0,
         main/1,
         generate/2,
         pack/4,
         connect/1
        ]).

main(Args) ->
    getopt_parse_run(Args).

help() ->
    getopt_help().
    
generate(ProjectName, ProjectType) ->
    generate_dirtree(ProjectName, ProjectType),
    generate_files(ProjectName, ProjectType).

generate_dirtree(ProjectName, _ProjectType) ->
    filelib:ensure_dir(ProjectName++"/ebin/"),
    filelib:ensure_dir(ProjectName++"/src/"),
    filelib:ensure_dir(ProjectName++"/include/"),
    filelib:ensure_dir(ProjectName++"/build-tools/").

generate_files(ProjectName, ProjectType) ->
    TplVars = [{project_name, ProjectName},
            {project_type, ProjectType}],
    {ok, AppSrc} = yaterl_gen_mod_appsrc_dtl:render(TplVars),
    {ok, ModSrc} = yaterl_gen_mod_dtl:render(TplVars),
    {ok, MakefileSrc} = makefile_dtl:render(TplVars),
    BaseFileName = ProjectName++"/src/"++ProjectName,
    io:format("Generating file: ~s.app.src~n", [BaseFileName]),
    ok = file:write_file(BaseFileName ++ ".app.src", AppSrc),
    io:format("Generating file: ~s.erl~n", [BaseFileName]),
    ok = file:write_file(BaseFileName ++ ".erl", ModSrc),
    io:format("Generating file: Makefile~n"),
    ok = file:write_file(ProjectName++"/Makefile", MakefileSrc),
    io:format("Generating file: rebar~n"),
    RebarData = extract_rebar_from_escript_archive(),
    ok = file:write_file(ProjectName++"/build-tools/rebar", RebarData),
    [] = os:cmd("chmod a+x "++ProjectName++"/build-tools/rebar"),
    {ok, _BytesCopied} = file:copy(escript:script_name(), 
                                   ProjectName++"/build-tools/yaterl-devtool"),
    [] = os:cmd("chmod a+x "++ProjectName++"/build-tools/yaterl-devtool").

pack(ProjectName, ProjectType, MainModuleName, Opts) ->
    {pack_yaterl, PackYaterl} = case proplists:lookup(pack_yaterl, Opts) of
                      none ->  {pack_yaterl, false};
                      Value -> Value
                    end,
    {yaterl_lib_dir, YaterlLibDir} = case proplists:lookup(yaterl_lib_dir, Opts) of
                                         none -> {yaterl_lib_dir, unknown};
                                         Value2 -> Value2
                                     end,
    Files = load_files_to_pack(PackYaterl, YaterlLibDir),
    case zip:create("mem", Files, [memory]) of
        {ok, {"mem", ZipBin}} ->
            write_zip_to_file(ProjectName, ProjectType, MainModuleName, ZipBin);
        {error, ZipError} ->
            io:format("ERROR: failed to pack yaterl_gen_mod project~n~p~n", [ZipError]),
            erlang:halt(2)
    end.

load_files_to_pack(true, none) ->
    code:del_path(yaterl), %% FIX: remove yaterl-devtool from code paths
    Files1 = collect_files_to_pack("*", "ebin"),
    Files2 = collect_files_to_pack("*", code:lib_dir(yaterl) ++ "/ebin"),
    Files1 ++ Files2;
load_files_to_pack(true, YaterlLibDir) when is_list(YaterlLibDir) ->
    Files1 = collect_files_to_pack("*", "ebin"),
    Files2 = collect_files_to_pack("*", YaterlLibDir ++ "/ebin"),
    Files1 ++ Files2;
load_files_to_pack(false, _) ->
    Files1 = collect_files_to_pack("*", "ebin"),
    Files1.

collect_files_to_pack(Wildcard, Dir) ->
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
     {project_type, $t, "type", string, "Project type (global, channel)"},
     {yaterl_lib_dir, $y, "yaterl", string, "Path to the yaterl lib dir (e.g. ./builds/yaterl-0.0.1)"},
     {pack_yaterl, undefined, "pack-yaterl", boolean, "Include yaterl into the packaged escript"}
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
    pack(ProjectName, ProjectType, MainModuleName, Opts);
devtool_execute(["pack", ProjectName, MainModuleName], Opts) ->
    ProjectType = case proplists:lookup(project_type, Opts) of
                      none -> "global";
                      Value -> Value
                  end,
    pack(ProjectName, ProjectType, MainModuleName, Opts);
devtool_execute(["pack", _ProjectName, _MainModuleName | _ ], Opts) ->
    help(),
    io:format("ERROR: too many arguments.~n");
devtool_execute(_Args, _Opts) ->
    help(),
    %%DEBUG: io:format("DEBUG: Args (~p) and Opts (~p). ~n", [_Args, _Opts]),
    io:format("ERROR: unknown command or invalid arguments.~n").


extract_rebar_from_escript_archive() ->
    Filename = escript:script_name(),
    {ok, File} = file:open(Filename,[read,binary]),
    {ok, _Line1} = file:read_line(File),
    {ok, _Line2} = file:read_line(File),
    {ok, Data} = file:read(File, 65535555), %%% TODO: this is awful :-P
    {ok, ExtractedList} = zip:extract(Data,[memory, {file_list, ["rebar"]}]),
    [{_Name, RebarData}|_] = ExtractedList,
    RebarData.
    
