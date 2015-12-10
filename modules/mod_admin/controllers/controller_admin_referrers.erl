%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2015 Marc Worrell
%% @doc List all referrers to a rsc

%% Copyright 2009-2015 Marc Worrell
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
         previously_existed/2,
         moved_temporarily/2,
         is_authorized/2
        ]).

-include_lib("controller_html_helper.hrl").

is_authorized(ReqData, Context) ->
    z_admin_controller_helper:is_authorized(mod_admin, ReqData, Context).

resource_exists(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Id = m_rsc:rid(z_context:get_q("id", Context1), Context1),
    Context2 = z_context:set(id, Id, Context1),
    ?WM_REPLY(m_rsc:exists(Id, Context2), Context2).

previously_existed(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Id = z_context:get(id, Context1),
    IsGone = m_rsc_gone:is_gone(Id, Context1),
    ?WM_REPLY(IsGone, Context1).

moved_temporarily(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Id = z_context:get(id, Context1),
    redirect(m_rsc_gone:get_new_location(Id, Context1), Context1).

redirect(undefined, Context) ->
    ?WM_REPLY(false, Context);
redirect(Location, Context) ->
    ?WM_REPLY({true, Location}, Context).

html(Context) ->
    Vars = [
            {id, z_context:get(id, Context)}
           ],
    Html = z_template:render("admin_referrers.tpl", Vars, Context),
    z_context:output(Html, Context).
