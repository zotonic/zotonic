%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2014 Marc Worrell
%% @doc Defines all paths for files and directories of a site.

%% Copyright 2009-2014 Marc Worrell
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

-module(z_path).
-author("Marc Worrell <marc@worrell.nl").

-export([
         site_dir/1,
         module_dir/2,
         media_preview/1,
         media_archive/1,
         abspath/2,
         files_subdir/2,
         files_subdir_ensure/2,
         user_sites_dir/0,
         user_modules_dir/0
        ]).

-include("zotonic.hrl").

%% @doc Return the path to the site folder of the given context.
%% @spec site_dir(#context{}) -> filename()
site_dir(Context=#context{host=Host}) ->
    F = fun() -> site_dir(Host) end,
    z_depcache:memo(F, {site_dir, Host}, ?HOUR, Context);
site_dir(Host) when is_atom(Host) ->
    find_first_path(
      [
       filename:join([z_path:user_sites_dir(), Host]),
       filename:join([z_utils:lib_dir(priv), "sites", Host])
      ]).

%% @doc Return the path to the given module in the given context.
%% @spec module_dir(atom(), #context{}) -> string()
module_dir(Module, Context=#context{host=Host}) ->
    F = fun() -> module_dir(Module, Host) end,
    z_depcache:memo(F, {module_dir, Module}, ?HOUR, Context);
module_dir(Module, Host) when is_atom(Host) ->
    find_first_path(
      [
       filename:join([z_path:user_sites_dir(), Host, "modules", Module]),
       filename:join([z_path:user_modules_dir(), Module]),
       filename:join([z_utils:lib_dir(modules)])
      ]).


find_first_path(Paths) ->
    lists:foldl(fun(Dir, undefined) ->
                        case filelib:is_dir(Dir) of
                            true -> Dir;
                            false -> undefined
                        end;
                   (_, Acc) -> Acc
                end,
                undefined,
                Paths).

%% @doc Return the path to the media preview directory
%% @spec media_preview(#context{}) -> filename()
media_preview(Context) ->
    files_subdir("preview", Context).

%% @doc Return the path to the media archive directory
%% @spec media_archive(#context{}) -> filename()
media_archive(Context) ->
    files_subdir("archive", Context).

%% @doc Return the absolute path to a file in the 'file' directory
abspath(Path, Context) ->
    files_subdir(Path, Context).

%% @doc Return the path to a files subdirectory
%% @spec files_subdir(SubDir::filename(), #context{}) -> filename()
files_subdir(SubDir, #context{host=Host}) ->
    filename:join([z_path:site_dir(Host), "files", SubDir]).

%% @doc Return the path to a files subdirectory and ensure that the directory is present
%% @spec files_subdir_ensure(SubDir::filename(), #context{}) -> filename()
files_subdir_ensure(SubDir, Context) ->
    Dir = files_subdir(SubDir, Context),
    ok = filelib:ensure_dir(filename:join([Dir, ".empty"])),
    Dir.

%% @doc The directory of the user-defined sites
user_sites_dir() ->
    z_config:get(user_sites_dir).

%% @doc The directory of the user-defined modules
user_modules_dir() ->
    z_config:get(user_modules_dir).

