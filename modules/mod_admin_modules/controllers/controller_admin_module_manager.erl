%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Overview of modules, allows activating/deactivating the modules.

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

-module(controller_admin_module_manager).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    is_authorized/2
]).

-include_lib("controller_html_helper.hrl").
-include_lib("modules/mod_admin/include/controller_admin_helper.hrl").

is_authorized(ReqData, Context) ->
    admin_controller_is_authorized(mod_admin_modules, ReqData, Context).

html(Context) ->
    Status = z_module_manager:get_modules_status(Context),
    Selected = z_context:get(selected, Context, "modules"),
    Status1 = lists:flatten(
                    [ 
                        [ {Module, atom_to_list(State)} || {Module, _, _Pid, _Date} <- Specs ] 
                        || {State, Specs} <- Status 
                    ]),
    Configurable = [{M#module_index.module, M} || M <- z_module_indexer:find_all(template, "_admin_configure_module.tpl", Context)],
    Vars =
        [
         {configurable, Configurable},
         {selected, Selected},
         {modules, mod_admin_modules:all(Context)},
         {status, Status1}
        ],
    Template = z_context:get(template, Context, "admin_modules.tpl"),
	Html = z_template:render(Template, Vars, Context),
	z_context:output(Html, Context).
