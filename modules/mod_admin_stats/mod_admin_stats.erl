%% @author Andreas Stenius <git@astekk.se>
%% @copyright 2013 Andreas Stenius
%% Date: 2013-02-23
%% @doc Statistics for the admin interface.

%% Copyright 2013 Andreas Stenius
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

-module(mod_admin_stats).
-author("Andreas Stenius <git@astekk.se>").

-mod_title("Admin Statistics").
-mod_description("Provides statistic information.").
-mod_prio(500).

%% interface functions
-export([
         observe_admin_menu/3,
         observe_postback_notify/2
        ]).

-include_lib("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").

observe_admin_menu(admin_menu, Acc, Context) ->
    [#menu_item{ id=admin_stats,
                 parent=admin_system,
                 label=?__("Statistics", Context),
                 url={admin_stats},
                 visiblecheck={acl, use, mod_admin_stats}
               }
     |Acc].

observe_postback_notify(#postback_notify{ message="update_metrics" }, Context) ->
    {Output, Context1} = z_template:render_to_iolist("_metrics_jsdata.tpl", [], Context),
    z_context:add_script_page(
      io_lib:format("z_event('new_metrics', ~s);", [z_convert:to_flatlist(Output)]),
      Context1),
    Context1;
observe_postback_notify(Args, _Context) ->
    ?DEBUG(Args),
    undefined.
