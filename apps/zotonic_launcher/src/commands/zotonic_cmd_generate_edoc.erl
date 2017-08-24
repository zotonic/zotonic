%%%-------------------------------------------------------------------
%%% @author Melki
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Aug 2017 9:29 AM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_generate_edoc).
-author("Melki").

%% API
-export([run/1]).

files(Path) ->
    filelib:fold_files(Path, ".*erl$", true, fun(F, Acc) -> [F|Acc] end, []).

run(_) ->
    generate("src", "core"),
    io:format("Generated core edoc files in ~p~n", [outpath("core")]),
    Zotonic = os:getenv("ZOTONIC"),
    Modules = [string:substr(L, length(Zotonic++"/modules/")+1) || L <- filelib:wildcard(filename:join([os:getenv("ZOTONIC"), "modules/mod_*"]))],

    [
        generate("modules/" ++ M, "modules/" ++ M) || M <- Modules
    ],
    io:format("Generated modules edoc files in ~p~n", [outpath("modules")]).

generate(InputDir, OutputDir) ->
    SrcPath = filename:join([os:getenv("ZOTONIC"), InputDir]),
    Files = files(SrcPath),
    edoc:files(Files, [{dir, outpath(OutputDir)}]).

outpath(Component) ->
    Path = filename:join([os:getenv("ZOTONIC"), "doc", "_build", "edoc", Component]),
    filelib:ensure_dir(filename:join([Path, ".empty"])),
    Path.
