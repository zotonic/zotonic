#!/usr/bin/env escript
%%
%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011 Arjan Scherpenisse
%% @doc Generates edoc documentation for the zotonic core system.

%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%%
%% usage zotonic generate-edoc
%%

main(_) ->
    Apps = [ filename:basename(F) || F <- filelib:wildcard( filename:join([ os:getenv("ZOTONIC"), "apps", "zotonic_*" ])) ],
    Core = lists:filter(fun(A) -> is_zotonic_core(A) end, Apps),
    Modules = lists:filter(fun(A) -> is_zotonic_mod(A) end, Apps),
    io:format("Generating edoc for core apps... "),
    lists:map(
        fun(M) ->
            generate("apps/" ++ M, "core/" ++ M)
        end,
        Core),
    io:format("~nGenerated core edoc files in ~p~n", [ outpath("core") ]),
    io:format("Generating edoc for core modules... "),
    lists:map(
        fun(M) ->
            generate("apps/" ++ M, "modules/" ++ M)
        end,
        Modules),
    io:format("Generated modules edoc files in ~p~n", [ outpath("modules") ]).

is_zotonic_core("zotonic_mod_" ++ _) -> false;
is_zotonic_core("zotonic_" ++ _) -> true;
is_zotonic_core(_) -> false.

is_zotonic_mod("zotonic_mod_" ++ _) -> true;
is_zotonic_mod(_) -> false.


generate(InputDir, OutputDir) ->
    SrcPath = filename:join([ os:getenv("ZOTONIC"), InputDir ]),
    Files = files(SrcPath),
    Options = [
        {preprocess, true},
        {includes, [
            filename:join([ SrcPath, "include" ]),
            filename:join([ os:getenv("ZOTONIC"), "apps" ]),
            filename:join([ os:getenv("ZOTONIC"), "apps", "zotonic_core" ]),
            filename:join([ os:getenv("ZOTONIC"), "apps", "zotonic_core", "include" ]),
            filename:join([ os:getenv("ZOTONIC"), "apps", "zotonic_mod_admin" ]),
            filename:join([ os:getenv("ZOTONIC"), "apps", "zotonic_mod_oembed" ]),
            filename:join([ os:getenv("ZOTONIC"), "apps", "zotonic_mod_import_csv" ]),
            filename:join([ os:getenv("ZOTONIC"), "_build", "default", "lib" ])
        ]},
        {dir, outpath(OutputDir)}
    ],
    edoc:files(Files, Options).

files(Path) ->
    filelib:fold_files(filename:join(Path, "src"), ".*erl$", true, fun(F, Acc) -> [ F | Acc ] end, []).

outpath(Component) ->
    Path = filename:join([os:getenv("ZOTONIC"), "doc", "_build", "edoc", Component]),
    filelib:ensure_dir(filename:join([Path, ".empty"])),
    Path.
