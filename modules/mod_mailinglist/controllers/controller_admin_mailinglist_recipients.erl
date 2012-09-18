%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc List all mailing lists, enable adding and deleting mailing lists.

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

-module(controller_admin_mailinglist_recipients).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    is_authorized/2,
	event/2
]).

-include_lib("html_controller.hrl").

is_authorized(ReqData, Context) ->
	z_acl:wm_is_authorized(use, mod_mailinglist, ReqData, Context).

html(Context) ->
    Vars = [
        {page_admin_mailinglist, true},
		{id, z_convert:to_integer(z_context:get_q("id", Context))}
    ],
	Html = z_template:render("admin_mailinglist_recipients.tpl", Vars, Context),
	z_context:output(Html, Context).

event(#postback{message={dialog_recipient_add, [{id,Id}]}}, Context) ->
	Vars = [
		{id, Id}
	],
	z_render:dialog(?__("Add recipient", Context), "_dialog_mailinglist_recipient.tpl", Vars, Context);

event(#postback{message={dialog_recipient_edit, [{id,Id}, {recipient_id, RcptId}]}}, Context) ->
	Vars = [
            {id, Id},
            {recipient_id, RcptId}
	],
	z_render:dialog(?__("Edit recipient", Context), "_dialog_mailinglist_recipient.tpl", Vars, Context);

event(#postback{message={recipient_is_enabled_toggle, [{recipient_id, RcptId}]}, target=TargetId}, Context) ->
	m_mailinglist:recipient_is_enabled_toggle(RcptId, Context),
	z_script:add_script(
		["$(\"#", TargetId, "\").parents(\"li:first\").toggleClass(\"unpublished\"); "], 
		Context);

event(#postback{message={recipient_change_email, [{recipient_id, RcptId}]}}, Context) ->
    Email = z_context:get_q("triggervalue", Context),
    m_mailinglist:update_recipient(RcptId, [{email, Email}], Context),
    z_render:growl(?__("E-mail address updated", Context), Context);

event(#postback{message={recipient_delete, [{recipient_id, RcptId}, {target, Target}]}}, Context) ->
	m_mailinglist:recipient_delete_quiet(RcptId, Context),
	z_render:wire([ {growl, [{text, ?__("Recipient deleted.", Context)}]},
					{slide_fade_out, [{target, Target}]}
				], Context);

event(#postback{message={recipients_clear, [{id, Id}]}}, Context) ->
	m_mailinglist:recipients_clear(Id, Context),
	z_render:wire([{reload, []}], Context);

event(#postback{message={dialog_recipient_upload, [{id,_Id}]}}, Context) ->
	Context.
