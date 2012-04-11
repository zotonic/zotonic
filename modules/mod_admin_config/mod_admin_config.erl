%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-08-07
%% @doc Allow editing and inserting config keys with string values.

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

-module(mod_admin_config).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Admin config support").
-mod_description("Allow admins to edit the system configuration.").
-mod_prio(800).
-mod_depends([admin]).
-mod_provides([]).


%% interface functions
-export([
    observe_admin_menu/3
]).

-include("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").


observe_admin_menu(admin_menu, Acc, Context) ->
    [
     #menu_item{id=admin_config,
                parent=admin_system,
                label=?__("Config", Context),
                url={admin_config},
                visiblecheck={acl, use, mod_admin_config}}
     
     |Acc].

