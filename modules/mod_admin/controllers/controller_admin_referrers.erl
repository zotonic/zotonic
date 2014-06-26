%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-05-08
%% @doc List all referrers to a rsc, should also offer to filter on predicate

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

-module(controller_admin_referrers).
-author("Marc Worrell <marc@worrell.nl").

-export([resource_exists/2,
         is_authorized/2
        ]).

-include_lib("controller_html_helper.hrl").

is_authorized(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = z_context:ensure_all(Context1),
    z_context:lager_md(Context2), 
    z_acl:wm_is_authorized([{use, mod_admin}], admin_logon, Context2).

resource_exists(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Id = m_rsc:rid(z_context:get_q("id", Context1), Context1),
    Context2 = z_context:set(id, Id, Context1),
    ?WM_REPLY(m_rsc:exists(Id, Context2), Context2).

html(Context) ->
    Vars = [
            {id, z_context:get(id, Context)}
           ],
    Html = z_template:render("admin_referrers.tpl", Vars, Context),
    z_context:output(Html, Context).
