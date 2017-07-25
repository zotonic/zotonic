%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011 Arjan Scherpenisse
%% @doc Mailing status/control page

%% Copyright 2011 Arjan Scherpenisse
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

-module(controller_admin_mailing_preview).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([
    resource_exists/1,
    is_authorized/1
]).

-include_lib("zotonic_core/include/controller_html_helper.hrl").

%% @todo Change this into "visible" and add a view instead of edit template.
is_authorized(Context) ->
    Context1 = z_admin_controller_helper:init_session(Context),
    {Context2, Id} = controller_admin_edit:ensure_id(Context1),
    z_acl:wm_is_authorized([{use, mod_mailinglist}, {view, Id}], Context2).


resource_exists(Context) ->
    {Context2, Id} = controller_admin_edit:ensure_id(Context),
    case Id of
        undefined -> {false, Context2};
        _N -> {m_rsc:exists(Id, Context2), Context2}
    end.


html(Context) ->
    PageId = z_context:get(id, Context),
    ListId = m_rsc:rid(mailinglist_test, Context),
    Vars = [
        {id,PageId},
        {list_id, ListId},
        {email_from, m_mailinglist:get_email_from(ListId, Context)}
    ],
    Html = z_template:render({cat, <<"mailing_page.tpl">>}, Vars, Context),
	z_context:output(Html, Context).
