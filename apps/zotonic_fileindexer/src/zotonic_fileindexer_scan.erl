%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2018 Marc Worrell
%% @doc Recursively scan a directory for files.

%% Copyright 2018 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(zotonic_fileindexer_scan).

-author('Marc Worrell <marc@worrell.nl>').

-export([
    scan/2
    ]).

-include_lib("zotonic_fileindexer/include/zotonic_fileindexer.hrl").

% Regexp for files to be ignored.
-define(IGNORE, "^_flymake|\\.#|\\.~|^\\.").

scan(Dir, undefined) -> scan(Dir, ".");
scan(Dir, "") -> scan(Dir, ".");
scan(Dir, <<>>) -> scan(Dir, ".");
scan(Dir, FilenameRE) ->
    {ok, IgnoreRE} = re:compile(?IGNORE),
    {ok, FileRE} = re:compile(FilenameRE),
    scan_recursive("", Dir, FileRE, IgnoreRE, []).

scan_recursive(RelPath, Dir, FileRE, IgnoreRE, Acc) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
              lists:foldl(
                fun(F, AccF) ->
                    scan_filename(F, RelPath, Dir, FileRE, IgnoreRE, AccF)
                end,
                Acc,
                Files);
        {error, _} ->
            Acc
    end.

scan_filename(F, RelPath, Dir, FileRE, IgnoreRE, Acc) ->
    case re:run(F, IgnoreRE) of
        nomatch ->
            Path = filename:join(Dir, F),
            RelPath1 = join(RelPath, F),
            case filelib:is_dir(Path) of
                true ->
                    scan_recursive(RelPath1, Path, FileRE, IgnoreRE, Acc);
                false ->
                    case re:run(F, FileRE) of
                        {match, _} ->
                            Found = #fileindex{
                                basename = to_binary(F),
                                rootname = to_binary(filename:rootname(F)),
                                extension = to_binary(filename:extension(F)),
                                relpath = to_binary(RelPath1),
                                path = to_binary(Path)
                            },
                            [ Found | Acc ];
                        nomatch ->
                            Acc
                    end
            end;
        {match, _} ->
            Acc
    end.

join("", F) -> F;
join(Dir, F) -> filename:join([ Dir, F ]).

to_binary(B) when is_binary(B) -> B;
to_binary(L) -> unicode:characters_to_binary(L).
