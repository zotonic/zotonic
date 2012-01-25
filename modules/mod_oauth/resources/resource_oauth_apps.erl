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

-module(resource_oauth_apps).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([
         is_authorized/2,
         event/2
]).

-include_lib("resource_html.hrl").


is_authorized(ReqData, Context) ->
    z_acl:wm_is_authorized(use, mod_admin, ReqData, Context).


html(Context) ->
    Html = z_template:render("oauth_apps.tpl", [{page_admin_oauth, true}], Context),
	z_context:output(Html, Context).



del_consumer(Id, Context) ->
    m_oauth_app:delete_consumer(Id, Context),
    Html = z_template:render("_oauth_apps_list.tpl", [], Context),
    Context1 = z_render:update("oauth-apps", Html, Context),
    z_render:wire({growl, [{text, "Application removed."}]}, Context1).



%%
%% Start add oauth application (consumer)
%%
event(#postback{message=start_add_app}, Context) ->
    z_render:dialog("Add application", "_oauth_consumer_edit.tpl", [], Context);

%%
%% Start edit oauth application (consumer)
%%
event(#postback{message={start_edit_app, Arg}}, Context) ->
    Id = proplists:get_value(id, Arg),
    Consumer = m_oauth_app:get_consumer(Id, Context),
    Vars = [{consumer, Consumer}],
    z_render:dialog("Edit application", "_oauth_consumer_edit.tpl", Vars, Context);

%%
%% Consumer save handler
%%
event(#submit{message={consumer_save, Arg}}, Context) ->
    Title = z_context:get_q("zp-title", Context),
    Descr = z_context:get_q("zp-text", Context),
    URL = z_context:get_q("zp-url", Context),
    Callback = z_context:get_q("zp-callback", Context),
    Perms = z_context:get_q_all("zp-perm", Context),

    Context1 = case proplists:get_value(id, Arg) of
                   undefined ->
                       Consumer = m_oauth_app:create_consumer(Title, URL, Descr, Callback, Context),
                       m_oauth_perms:set(z_db:get(id, Consumer), Perms, Context),
                       z_render:wire({growl, [{text, "Created new application."}]}, Context);
                   
                   Id ->
                       m_oauth_app:update_consumer(Id, [{application_title, Title},
                                                        {application_descr, Descr},
                                                        {application_uri, URL},
                                                        {callback_uri, Callback}], Context),
                       m_oauth_perms:set(Id, Perms, Context),
                       z_render:wire({growl, [{text, "Application details saved."}]}, Context)
    end,
    Html = z_template:render("_oauth_apps_list.tpl", [], Context1),
    Context2 = z_render:update("oauth-apps", Html, Context1),
    z_render:wire({dialog_close, []}, Context2);


%%
%% Delete oauth application (consumer)
%%
event(#postback{message={start_del_app, Arg}}, Context) ->
    Id = proplists:get_value(id, Arg),
    Vars = [{id, Id}, {delete, true}],
    z_render:dialog("Delete application", "_oauth_consumer_tokens.tpl", Vars, Context);


event(#postback{message={start_tokens, Arg}}, Context) ->
    Id = proplists:get_value(id, Arg),
    Vars = [{id, Id}],
    z_render:dialog("Tokens", "_oauth_consumer_tokens.tpl", Vars, Context);
    

event(#postback{message={confirm_del_app, Arg}}, Context) ->
    Id = proplists:get_value(id, Arg),
    Context1 = z_render:wire({dialog_close, []}, Context),
    del_consumer(Id, Context1).
