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

-export([resource_exists/1,
         previously_existed/1,
         moved_temporarily/1,
         is_authorized/1,
         process/4
        ]).

is_authorized(Context) ->
    z_controller_helper:is_authorized([ {use, z_context:get(acl_module, Context, mod_admin)} ], Context).

resource_exists(Context) ->
    Id = m_rsc:rid(z_context:get_q(<<"id">>, Context), Context),
    Context2 = z_context:set(id, Id, Context),
    {m_rsc:exists(Id, Context2), Context2}.

previously_existed(Context) ->
    Id = z_context:get(id, Context),
    {m_rsc_gone:is_gone(Id, Context), Context}.

moved_temporarily(Context) ->
    Id = z_context:get(id, Context),
    redirect(m_rsc_gone:get_new_location(Id, Context), Context).

redirect(undefined, Context) ->
    {false, Context};
redirect(Location, Context) ->
    {{true, Location}, Context}.

process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
    Vars = [
            {id, z_context:get(id, Context)}
           ],
    Html = z_template:render("admin_referrers.tpl", Vars, Context),
    z_context:output(Html, Context).
