%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012 Marc Worrell
%% @doc Send a page by e-mail to an user supplied e-mail address

%% Copyright 2012 Marc Worrell, Arjan Scherpenisse
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

-module(action_mailinglist_dialog_mail_page).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
	event/2
]).

-include("zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    Id = z_convert:to_integer(proplists:get_value(id, Args)),
    OnSuccess = proplists:get_all_values(on_success, Args),
    Postback = {dialog_mail_page, Id, OnSuccess},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.

event(#postback{message={dialog_mail_page, Id, OnSuccess}}, Context) ->
	Vars = [
            {id, Id},
            {on_success, OnSuccess}
	],
	z_render:dialog(?__("E-mail this page", Context), "_dialog_mail_page.tpl", Vars, Context);

%% When the page is not yet visible and the user did not choose the
%% "now" button, the mailing is queued.
event(#submit{message={mail_page, Args}}, Context) ->
	Id = proplists:get_value(id, Args),
	OnSuccess = proplists:get_all_values(on_success, Args),
	Email = z_context:get_q_validated("email", Context),
	Vars = [
		{id, Id},
		{recipient, Email}
	],
	case m_rsc:is_a(Id, document, Context) of
		false -> z_email:send_render(Email, {cat, "mailing_page.tpl"}, Vars, Context);
		true ->
			E = #email{
				to=Email,
				html_tpl={cat, "mailing_page.tpl"},
				vars=Vars,
				attachments=[Id]
			},
			z_email:send(E, Context)
	end,
	Context1 = z_render:growl(?__("Sending the e-mail...", Context), Context),
	z_render:wire(OnSuccess, Context1).
