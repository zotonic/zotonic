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

-module(resource_admin_referrers).
-author("Marc Worrell <marc@worrell.nl").

-export([
    resource_exists/2,
    is_authorized/2
]).

-include_lib("resource_html.hrl").

is_authorized(ReqData, Context) ->
    z_acl:wm_is_authorized(use, mod_admin, ReqData, Context).

resource_exists(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = z_context:ensure_all(Context1),
    Id = z_context:get_q("id", Context2),
    try
        IdN = list_to_integer(Id),
        Context3 = z_context:set(id, IdN, Context2),
        ?WM_REPLY(m_rsc:exists(IdN, Context3), Context3)
    catch
        _:_ -> ?WM_REPLY(false, Context2)
    end.

html(Context) ->
    Vars = [
        {id, z_context:get(id, Context)}
    ],
    Html = z_template:render("admin_referrers.tpl", Vars, Context),
	z_context:output(Html, Context).
