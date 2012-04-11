%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-06-03
%% @doc Add a module management screen to the admin.

%% Copyright 2009 Marc Worrell
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

-module(mod_admin_modules).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Admin module support").
-mod_description("Manages modules. Adds an admin interface to activate and deactivate modules.").
-mod_prio(700).
-mod_depends([admin]).
-mod_provides([]).

%% interface functions
-export([
    all/1,
    observe_admin_menu/3
]).

-include("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").


%% @spec all(context()) -> ModuleDescriptions
%% @doc Fetch a list of all modules available, including their description as a propertylist. The module list is sorted
%% on the name of the module.
all(Context) ->
    Active  = z_module_manager:active(Context),
    Modules = z_module_manager:scan(Context),
    Descrs  = [ add_sort_key({z_module_manager:prio(M), M, [{is_active, lists:member(M, Active)}, {path, Path} | descr(M)]}) || {M, Path} <- Modules ],
    lists:sort(Descrs).


    add_sort_key({Prio, M, Props}) ->
        SortKey = case atom_to_list(M) of
                    "mod_" ++ _ -> {not proplists:get_value(is_active, Props), 2, z_string:to_name(proplists:get_value(mod_title, Props))};
                    _ -> {not proplists:get_value(is_active, Props), 1, proplists:get_value(mod_title, Props)}
                  end,
        {SortKey, Prio, M, Props}.
        

%% @spec descr(ModuleName) -> proplist()
%% @doc Return a property list with the title and other attributes of the module.
descr(Module) ->
    Descr = case z_module_manager:module_exists(Module) of
        true ->
            try
                erlang:get_module_info(Module, attributes)
            catch 
                _M:E -> [{error, E}]
            end;
        false ->
            [{error, enoent}]
    end,
    case proplists:get_value(title, Descr) of
        undefined ->
            Title = case atom_to_list(Module) of
                        "mod_" ++ T -> 
                            string:join(string:tokens(T, "_"), " ");
                        T ->
                            string:join(string:tokens(T, "_"), " ")
                    end,
            [{title, Title} | Descr];
        _Title ->
            Descr
    end.


observe_admin_menu(admin_menu, Acc, Context) ->
    [
     #menu_item{id=admin_modules_,
                parent=admin_system,
                label=?__("Modules", Context),
                url={admin_modules},
                visiblecheck={acl, use, mod_admin_modules}}
     
     |Acc].

