%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Discover independently deployed module repositories from their source paths.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(z_site_update_repositories).
-export([user_modules/0, module/1]).

%% Include inactive modules too. Git resolves source symlinks, nested apps and
%% worktrees; comparing repository roots excludes Zotonic's bundled modules.
-spec user_modules() -> map().
user_modules() ->
    ZotonicRoot = git_root(z_path:get_path()),
    lists:foldl(fun({Module, App, _}, Acc) ->
        case repository(App, ZotonicRoot) of
            {git, Root} -> Acc#{Module => #{repository => filename:basename(Root)}};
            false -> Acc
        end
    end, #{}, z_module_manager:scan()).

%% Resolve again on preview and confirmation. Do not accept a browser-supplied
%% path or trust the cached list used to display the module buttons.
-spec module(Module) -> {git, file:filename_all()} | false when Module :: atom() | binary().
module(Module) ->
    case [App || {M, App, _} <- z_module_manager:scan(), matches(Module, M)] of
        [App] -> repository(App, git_root(z_path:get_path()));
        _ -> false
    end.

matches(Module, Module) -> true;
matches(Module, Known) when is_binary(Module) -> Module =:= atom_to_binary(Known, utf8);
matches(_, _) -> false.

repository(App, ZotonicRoot) ->
    case z_path:site_source_dir(App) of
        {error, _} -> false;
        Source ->
            case git_root(Source) of
                false -> false;
                ZotonicRoot -> false;
                Root -> {git, Root}
            end
    end.

git_root(Path) ->
    case exec:run("git rev-parse --show-toplevel", [sync, stdout, stderr, {cd, Path}]) of
        {ok, Output} ->
            case z_string:trim(iolist_to_binary(proplists:get_value(stdout, Output, []))) of
                <<>> -> false;
                Root -> unicode:characters_to_list(Root)
            end;
        {error, _} -> false
    end.
