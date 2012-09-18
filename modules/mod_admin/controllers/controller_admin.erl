%% @author Tim Benniks <tim@timbenniks.com>
%% @copyright 2009 Tim Benniks.
%% @doc Admin webmachine_controller.

%% Copyright 2009 Tim Benniks
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

-module(controller_admin).
-author("Tim Benniks <tim@timbenniks.com>").

-export([is_authorized/2
        ]).

-include_lib("html_controller.hrl").

is_authorized(ReqData, Context) ->
    z_acl:wm_is_authorized(use, z_context:get(acl_module, Context, mod_admin), admin_logon, ReqData, Context).

html(Context) ->
    Template = z_context:get(template, Context, "admin.tpl"),
    Selected = z_context:get(selected, Context, "dashboard"),
    Html = z_template:render(Template, [{selected, Selected}], Context),
    z_context:output(Html, Context).
