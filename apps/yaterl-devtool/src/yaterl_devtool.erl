-module(yaterl_devtool).

-export([
         help/0,
         main/1,
         generate/2,
         pack/2,
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

pack(ProjectName, ProjectType) ->
    Files = load_files_to_pack(),
    case zip:create("mem", Files, [memory]) of
        {ok, {"mem", ZipBin}} ->
            write_zip_to_file(ProjectName, ProjectType, ZipBin);
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

write_zip_to_file(ProjectName, ProjectType, ZipBin) ->
    ScriptData = case ProjectType of
                     "global" -> ProjectNameBin = list_to_binary(ProjectName),
                                 <<"#!/usr/bin/env escript\n",
                                   "%%!-sname test", ProjectNameBin/binary, 
                                   " -noinput\n",
                                   ZipBin/binary>>;
                     "channel" -> <<"#!/usr/bin/env escript\n",
                                    "%%!-noinput\n",
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

connect(Options) ->
    ok.


%%%%%%%%%%%%%%%%%%%%%%%
%% PRIVATE FUNCTIONS %%
%%%%%%%%%%%%%%%%%%%%%%%

getopt_speclist() ->
    [
     {help,    $h, "help",    undefined, "Print this help"},
     {verbose, $v, "verbose", boolean,   "List all the actions executed"},
     {generate_project_name, $g, "generate", string,  "Generate a new project skeleton"},
     {project_type, $t, "type", string, "Project skeleton type"},
     {pack_project_name, $p, "pack", string,  "Generate a new project skeleton"}
    ].
    
getopt_help() ->
    getopt:usage(getopt_speclist(), "yaterl-devtool").

getopt_parse_run(Args) ->
    Result = getopt:parse(getopt_speclist(), Args),
    Opts = case Result of
        {ok, {ParsedOpts, RestArgs}} ->
            io:format("PARSED: ~p~nREST: ~p~n", [ParsedOpts,RestArgs]),
            ParsedOpts;
        {error, {invalid_option, InvalidOption}} ->
            help(),
            io:format("ERROR: unknown option '~s'~n", [InvalidOption]),
            erlang:halt(1);
        {error, Reason} ->
            io:format("ERROR: ~p~n", [Reason]),
            erlang:halt(1)
    end,
    Help = proplists:lookup(help, Opts),
    GenerateProjectName = proplists:lookup(generate_project_name, Opts),
    PackProjectName = proplists:lookup(pack_project_name, Opts),
    ProjectType = proplists:lookup(project_type, Opts),
    execute(Help, GenerateProjectName, PackProjectName, ProjectType).

execute(none, none, none, none) ->
    help();
execute({help, true}, _, _, _) ->
    help();
execute(none, {generate_project_name, ProjectName}, none, none) ->
    execute(none, {generate_project_name, ProjectName}, none, {project_type, "global"});
execute(none, {generate_project_name, ProjectName}, none, {project_type, ProjectType}) ->
    generate(ProjectName, ProjectType);
execute(none, none, {pack_project_name, ProjectName}, none) ->
    execute(none, none, {pack_project_name, ProjectName}, {project_type, "global"});
execute(none, none, {pack_project_name, ProjectName}, {project_type, ProjectType}) ->
    pack(ProjectName, ProjectType).
