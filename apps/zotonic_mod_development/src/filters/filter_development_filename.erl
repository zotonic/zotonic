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
-moduledoc(#{
    zotonic_keywords => ["reference", "frontend_developer", "template_filter"]
}).
-moduledoc(<<"
Format a source filename for display in development tools. Resolves symbolic links
in both files and parent directories, then removes the Zotonic root directory from
paths inside that root. This makes template viewer, cross-reference and language
server paths easier to read.

For example:

```django
{{ template_file|development_filename|escape }}
```

With Zotonic installed in `/home/developer/zotonic`, the filename
`/home/developer/zotonic/apps/mysite/priv/templates/page.tpl` is displayed as
`apps/mysite/priv/templates/page.tpl`. A development build path whose `priv`
directory links to the source directory is first resolved to that source path.
The root itself is displayed as `.`.

Absolute paths outside the Zotonic root retain their full resolved path. Relative
paths are left unchanged. An undefined input returns `undefined`; other supported
filename inputs return a binary. If symlink resolution fails, the original path
is used for root-relative formatting. Link traversal is limited to prevent cycles.

The root is obtained from `z_path:get_path()` and is resolved through symlinks too.
It refers to the Zotonic installation, not the current site's directory.

This filter formats labels only: retain the original filename for opening files
or generating editor links. It does not check access permissions or HTML-escape
the result; use `escape` when displaying it in HTML, as in the example above.
">>).

-export([development_filename/1, development_filename/2, resolve/1]).

%% @doc Resolve symlinks and shorten filenames inside the Zotonic root for display only.
-spec development_filename(undefined | file:filename_all(), z:context()) -> undefined | binary().
development_filename(Filename, _Context) ->
    development_filename(Filename).

%% @doc Format a source path for development tools without requiring a site context.
-spec development_filename(undefined | file:filename_all()) -> undefined | binary().
development_filename(undefined) ->
    undefined;
development_filename(Filename) ->
    Path = resolve(z_convert:to_binary(Filename)),
    Root = filename:split(resolve(z_convert:to_binary(filename:absname(z_path:get_path())))),
    z_convert:to_binary(relative(filename:split(Path), Root, Path)).

%% @doc Resolve an absolute path, preserving the input if resolution fails.
%% Walk each component: a regular file can still be reached through a linked
%% priv directory. Bound link traversal so cycles fall back to the input path.
-spec resolve(binary()) -> binary().
resolve(Path) when is_binary(Path) ->
    case filename:pathtype(Path) of
        absolute ->
            case resolve_parts(filename:split(Path), <<>>, 40) of
                {ok, Resolved} -> z_convert:to_binary(Resolved);
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
