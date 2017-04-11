%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2009-10-01
%% @doc Authorizing an OAuth access key

%% Copyright 2009 Arjan Scherpenisse
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

-module(controller_oauth_apps).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([
         is_authorized/1,
         event/2
]).

-include_lib("controller_html_helper.hrl").


is_authorized(Context) ->
    z_admin_controller_helper:is_authorized(mod_oauth, Context).


html(Context) ->
    case z_convert:to_integer(z_context:get_q("app_id", Context)) of
        undefined ->
            Html = z_template:render("oauth_apps.tpl", [{page_admin_oauth, true}], Context),
        	z_context:output(Html, Context);
        AppId ->
            App = m_oauth_app:get_consumer(AppId, Context),
            Vars = [
                {page_admin_oauth, true},
                {app_id, AppId},
                {app, App},
                {tokens, m_oauth_app:consumer_access_tokens(AppId, Context)}
            ],
            Html = z_template:render("oauth_consumer_tokens.tpl", Vars, Context),
            z_context:output(Html, Context)
    end.


%%
%% Start add oauth application (consumer)
%%
event(Event, Context) ->
    case z_acl:is_allowed(use, mod_oauth, Context) of
        true ->
            do_event(Event, Context);
        false ->
            lager:error("Refused access to oauth events."),
            Context
    end.

do_event(#postback{message=start_add_app}, Context) ->
    z_render:dialog("Add application", "_oauth_consumer_edit.tpl", [], Context);

%%
%% Start edit oauth application (consumer)
%%
do_event(#postback{message={start_edit_app, Arg}}, Context) ->
    Id = proplists:get_value(id, Arg),
    Consumer = m_oauth_app:get_consumer(Id, Context),
    Vars = [{consumer, Consumer}],
    z_render:dialog(?__("Edit application", Context), <<"_oauth_consumer_edit.tpl">>, Vars, Context);

%%
%% Consumer save handler
%%
do_event(#submit{message={consumer_save, Arg}}, Context) ->
    Title = z_context:get_q("zp-title", Context),
    Descr = z_context:get_q("zp-text", Context),
    URL = z_context:get_q("zp-url", Context),
    Callback = z_context:get_q("zp-callback", Context),
    Perms = z_context:get_q_all("zp-perm", Context),
    Context1 = case proplists:get_value(id, Arg) of
        undefined ->
            Consumer = m_oauth_app:create_consumer(Title, URL, Descr, Callback, Context),
            m_oauth_perms:set(proplists:get_value(id, Consumer), Perms, Context),
            z_render:wire({growl, [{text, ?__("Created new application.", Context)}]}, Context);
        Id ->
            m_oauth_app:update_consumer(
                Id,
                [
                    {application_title, Title},
                    {application_descr, Descr},
                    {application_uri, URL},
                    {callback_uri, Callback}
                ],
                Context),
            m_oauth_perms:set(Id, Perms, Context),
            z_render:wire({growl, [{text, ?__("Application details saved.", Context)}]}, Context)
    end,
    Html = z_template:render(<<"_oauth_apps_list.tpl">>, [], Context1),
    Context2 = z_render:update(<<"oauth-apps">>, Html, Context1),
    z_render:wire({dialog_close, []}, Context2);

do_event(#postback{message={start_del_app, Arg}}, Context) ->
    % Ask confirmarion to delete oauth application (consumer)
    AppId = proplists:get_value(id, Arg),
    Vars = [
        {id, AppId},
        {tokens, m_oauth_app:consumer_access_tokens(AppId, Context)}
    ],
    z_render:dialog(?__("Delete application", Context), "_dialog_oauth_consumer_delete.tpl", Vars, Context);

do_event(#postback{message={confirm_del_app, Arg}}, Context) ->
    AppId = proplists:get_value(id, Arg),
    m_oauth_app:delete_consumer(AppId, Context),
    Context1 = z_render:update("oauth-apps", #render{ template="_oauth_apps_list.tpl", vars=[] }, Context),
    z_render:wire([
            {dialog_close, []},
            {growl, [{text, "Application removed."}]}
        ],
        Context1);

do_event(#postback{message={ensure_anonymous_token, Arg}}, Context) ->
    AppId = proplists:get_value(app_id, Arg),
    _ = m_oauth_app:ensure_anonymous_token(AppId, Context),
    z_render:wire({reload, []}, Context);

do_event(#postback{message={delete_token, Arg}}, Context) ->
    TokenId = proplists:get_value(token_id, Arg),
    _ = m_oauth_app:delete_consumer_token(TokenId, Context),
    z_render:wire({reload, []}, Context);

do_event(#postback{message={reset_consumer_secret, Arg}}, Context) ->
    AppId = proplists:get_value(app_id, Arg),
    _ = m_oauth_app:reset_consumer_secret(AppId, Context),
    z_render:wire({reload, []}, Context).

