%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2023 Marc Worrell
%% @doc Overview of modules, allows activating/deactivating the modules.
%% @end

%% Copyright 2009-2023 Marc Worrell
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

-module(controller_admin_module_manager).
-moduledoc("
Shows the list of Zotonic modules currently known to the system.

The list is sorted based on the module’s status: active modules are listed first, non-active modules next.

Each module has a button which let you toggle the active status of the module.

Todo

Extend documentation
").
-author("Marc Worrell <marc@worrell.nl>").

-export([
    service_available/1,
    is_authorized/1,
    process/4,

    event/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

service_available(Context) ->
    Context1 = z_context:set_noindex_header(Context),
    Context2 = z_context:set_nocache_headers(Context1),
    {true, Context2}.

is_authorized(Context) ->
    z_controller_helper:is_authorized([ {use, z_context:get(acl_module, Context, mod_admin_modules)} ], Context).

process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
    Status = z_module_manager:get_modules_status(Context),
    Selected = z_context:get(selected, Context, "modules"),
    Configurable = [{M#module_index.module, M} || M <- z_module_indexer:find_all(template, "_admin_configure_module.tpl", Context)],
    Vars =
        [
         {configurable, Configurable},
         {selected, Selected},
         {modules, mod_admin_modules:all(Context)},
         {status, Status}
        ],
    Template = z_context:get(template, Context, "admin_modules.tpl"),
	Html = z_template:render(Template, Vars, Context),
	z_context:output(Html, Context).

event(#postback{ message = {reinstall, [ {module, Module} ]} }, Context) ->
    case z_acl:is_allowed(user, mod_admin_modules, Context) of
        true ->
            case z_module_manager:reinstall(Module, Context) of
                ok ->
                    z_render:growl(?__("Reinstalled the data model.", Context), Context);
                nop ->
                    z_render:growl(?__("This module does not have a <tt>manage_schema/2</tt> function.", Context), Context)
            end;
        false ->
            z_render:growl_error(?__("Sorry, you are not allowed to do this.", Context), Context)
    end.
