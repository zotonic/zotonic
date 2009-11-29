%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-11-27
%% @doc Open a dialog for sending an e-mail to a mailing list.

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

-module(action_mailinglist_dialog_mailing_page).
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
    Postback = {dialog_mailing_page, Id, OnSuccess},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.

event({postback, {dialog_mailing_page, Id, OnSuccess}, _TriggerId, _TargetId}, Context) ->
	Vars = [
		{id, Id},
		{on_success, OnSuccess}
	],
	z_render:dialog("Give e-mail address of recipient.", "_dialog_mailing_page.tpl", Vars, Context);

event({submit, {mailing_page, Args}, _TriggerId, _TargetId}, Context) ->
	PageId = proplists:get_value(id, Args),
	OnSuccess = proplists:get_all_values(on_success, Args),
	ListId = z_convert:to_integer(z_context:get_q("mailinglist_id", Context)),
	Name = m_rsc:p(ListId, name, Context),
	Context1 = case Name == <<"mailinglist_test">> orelse z_acl:rsc_visible(PageId, z_acl:anondo(Context)) of
		true -> 
			z_notifier:notify({mailinglist_mailing, ListId, PageId}, Context),
			z_render:growl("The e-mails are being send...", Context);
		false -> 
			m_mailinglist:insert_scheduled(ListId, PageId, Context),
			z_render:growl("The mailing will be send when the page becomes visible.", Context)
	end,
	Context2 = mod_mailinglist:update_scheduled_list(PageId, Context1),
	z_render:wire(OnSuccess, Context2).


