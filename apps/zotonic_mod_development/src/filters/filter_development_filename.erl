%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2026 Maas-Maarten Zeeman
%% @doc Display filenames relative to the Zotonic root.
%% @end

%% Copyright 2026 Maas-Maarten Zeeman
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

-module(filter_development_filename).
-moduledoc(<<"Resolve file and directory symlinks, then display paths within the Zotonic root as relative filenames. Paths outside the root remain absolute.">>).

-export([development_filename/2]).

%% @doc Resolve symlinks and shorten filenames inside the Zotonic root for display only.
-spec development_filename(undefined | file:filename_all(), z:context()) -> undefined | binary().
development_filename(undefined, _Context) ->
    undefined;
development_filename(Filename, _Context) ->
    Path = resolve(z_convert:to_binary(Filename)),
    Root = filename:split(resolve(z_convert:to_binary(filename:absname(z_path:get_path())))),
    z_convert:to_binary(relative(filename:split(Path), Root, Path)).

%% @doc Resolve an absolute path, preserving the input if resolution fails.
%% Walk each component: a regular file can still be reached through a linked
%% priv directory. Bound link traversal so cycles fall back to the input path.
resolve(Path) ->
    case filename:pathtype(Path) of
        absolute ->
            case resolve_parts(filename:split(Path), <<>>, 40) of
                {ok, Resolved} -> Resolved;
                {error, _} -> Path
            end;
        _ -> Path
    end.

%% @doc Walk path components and symlink targets with a bounded link traversal count.
resolve_parts([], Path, _Remaining) ->
    {ok, Path};
resolve_parts([<<".">> | Rest], Path, Remaining) ->
    resolve_parts(Rest, Path, Remaining);
resolve_parts([<<"..">> | Rest], Path, Remaining) ->
    resolve_parts(Rest, filename:dirname(Path), Remaining);
resolve_parts([Part | Rest], Parent, Remaining) ->
    Path = filename:join(Parent, Part),
    case file:read_link(Path) of
        {ok, _} when Remaining =:= 0 ->
            {error, eloop};
        {ok, Target} ->
            TargetPath = filename:absname(Target, filename:dirname(Path)),
            resolve_parts(filename:split(TargetPath) ++ Rest, <<>>, Remaining - 1);
        {error, einval} ->
            resolve_parts(Rest, Path, Remaining);
        {error, _} = Error ->
            Error
    end.

%% @doc Strip matching root components without treating similarly prefixed directories as children.
relative([Part | Path], [Part | Root], Original) ->
    relative(Path, Root, Original);
relative([], [], _Original) ->
    <<".">>;
relative(Path, [], Original) ->
    case lists:member(<<"..">>, Path) of
        true -> Original;
        false -> filename:join(Path)
    end;
relative(_, _, Original) ->
    Original.
