%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2015 Maas-Maarten Zeeman
%% @doc Allow viewing system statistics.

%% Copyright 2015 Maas-Maarten Zeeman
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

-module(mod_admin_statistics).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-mod_title("Admin Statistics").
-mod_description("Allow admins to view system statistics.").
-mod_prio(800).
-mod_depends([admin, mod_mqtt]).
-mod_provides([]).


%% interface functions
-export([
    observe_admin_menu/3,
    observe_acl_is_allowed/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").


%%
%% Observes
%%

observe_acl_is_allowed(#acl_is_allowed{ action = subscribe, object = #acl_mqtt{ topic = [ <<"erlang">>, <<"stats">>| _ ]}}, Context) ->
    z_acl:is_allowed(use, mod_admin_statistics, Context);
observe_acl_is_allowed(#acl_is_allowed{}, _Context) ->
    undefined.


observe_admin_menu(#admin_menu{}, Acc, Context) ->
    [
     #menu_item{id=admin_statistics,
                parent=admin_system,
                label=?__("Statistics", Context),
                url={admin_statistics},
                visiblecheck={acl, use, mod_admin_statistics}}
     |Acc].


%%
%% Helpers
%%


