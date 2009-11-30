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

-module(resource_admin_mailinglist_recipients).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    is_authorized/2,
	event/2
]).

-include_lib("resource_html.hrl").

is_authorized(ReqData, Context) ->
	z_auth:wm_is_authorized(ReqData, Context).

html(Context) ->
    Vars = [
        {page_admin_mailinglist, true},
		{id, z_convert:to_integer(z_context:get_q("id", Context))}
    ],
	Html = z_template:render("admin_mailinglist_recipients.tpl", Vars, Context),
	z_context:output(Html, Context).

event({postback, {dialog_recipient_add, [{id,Id}]}, _TriggerId, _TargetId}, Context) ->
	Vars = [
		{id, Id}
	],
	z_render:dialog("Add recipient.", "_dialog_mailinglist_recipient_add.tpl", Vars, Context);

event({submit, {recipient_add, [{id,Id}]}, _TriggerId, _TargetId}, Context) ->
	case z_acl:rsc_is_editable(Id, Context) of
		true ->
			Email = z_context:get_q_validated(email, Context),
			Notification = case z_convert:to_bool(z_context:get_q(send_welcome, Context, false)) of
				true -> send_welcome;
				false -> silent
			end,
			case m_mailinglist:insert_recipient(Id, Email, Notification, Context) of
				ok -> 
					z_render:wire([	{growl, [{text, "Added the recipient."}]}, 
									{dialog_close, []},
									{reload, []}], Context);
				{error, _Reason} ->
					z_render:growl_error("Could not add the recipient.", Context)
			end;
		false ->
			z_render:growl_error("You are not allowed to add and enable recipients.", Context)
	end;

event({postback, {recipient_is_enabled_toggle, [{recipient_id, RcptId}]}, _TriggerId, _TargetId}, Context) ->
	m_mailinglist:recipient_is_enabled_toggle(RcptId, Context),
	Context;

event({postback, {recipient_delete, [{recipient_id, RcptId}]}, _TriggerId, _TargetId}, Context) ->
	m_mailinglist:recipient_delete(RcptId, Context),
	z_render:wire([ {growl, [{text, "Recipient deleted."}]},
					{slide_fade_out, [{target, "recipient-"++integer_to_list(RcptId)}]}
				], Context);

event({postback, {dialog_recipient_upload, [{id,_Id}]}, _TriggerId, _TargetId}, Context) ->
	Context.
