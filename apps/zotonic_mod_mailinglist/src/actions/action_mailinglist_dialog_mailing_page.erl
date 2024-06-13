%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2024 Marc Worrell
%% @doc Open a dialog for sending an e-mail to a mailing list.
%% @end

%% Copyright 2009-2024 Marc Worrell, Arjan Scherpenisse
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

-include_lib("zotonic_core/include/zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    Id = z_convert:to_integer(proplists:get_value(id, Args)),
    ListId = z_convert:to_integer(proplists:get_value(list_id, Args)),
    OnSuccess = proplists:get_all_values(on_success, Args),
    Postback = {dialog_mailing_page, Id, ListId, OnSuccess},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.

event(#postback{message={dialog_mailing_page, Id, ListId, OnSuccess}}, Context) ->
	Vars = [
            {id, Id},
            {list_id, ListId},
            {on_success, OnSuccess}
	],
	z_render:dialog(?__("Confirm sending to mailinglist", Context), "_dialog_mailing_page.tpl", Vars, Context);

%% When the page is not yet visible and the user did not choose the
%% "now" button, the mailing is queued.
event(#submit{message={mailing_page, Args}}, Context) ->
	PageId = proplists:get_value(id, Args),
	OnSuccess = proplists:get_all_values(on_success, Args),
	ListId = m_rsc:rid(z_context:get_q(<<"list_id">>, Context), Context),
	IsMatchLanguage = z_convert:to_bool(z_context:get_q(<<"is_match_language">>, Context)),
	IsSendAll = z_convert:to_bool(z_context:get_q(<<"is_send_all">>, Context)),
    When = z_context:get_q(<<"mail_when">>, Context),
    Options = [
    	{is_match_language, IsMatchLanguage},
    	{is_send_all, IsSendAll}
    ],
	Context1 = case z_acl:rsc_visible(PageId, z_acl:anondo(Context)) orelse When =:= <<"now">> of
		true ->
			Notification = #mailinglist_mailing{
				list_id = ListId,
				page_id = PageId,
				options = Options
			},
			z_notifier:notify(Notification, Context),
			z_render:growl(?__("The e-mails are being sent...", Context), Context);
		false ->
			m_mailinglist:insert_scheduled(ListId, PageId, Options, Context),
			z_render:growl(?__("The mailing will be send when the page becomes visible.", Context), Context)
	end,
	z_render:wire(OnSuccess, Context1).
