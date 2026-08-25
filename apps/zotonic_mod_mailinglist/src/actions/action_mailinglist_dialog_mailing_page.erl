%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2026 Marc Worrell
%% @doc Open a dialog for sending an e-mail to a mailing list.
%% @end

%% Copyright 2009-2026 Marc Worrell, Arjan Scherpenisse
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
-moduledoc("
Shows the dialog to mail the current page ([resource](/id/doc_glossary#term-resource)) to a mailing list. This is used
in the admin “mailing status” interface. A mailing can be sent immediately, when the page becomes publicly visible, or
at an explicitly selected date and time.
").
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
    case is_allowed(Id, ListId, Context) of
        true ->
            IsTest = is_test_mailinglist(ListId, Context),
            Vars = [
                {id, Id},
                {list_id, ListId},
                {is_test, IsTest},
                {on_success, OnSuccess}
            ],
            z_render:dialog(
                dialog_title(IsTest, Context),
                "_dialog_mailing_page.tpl",
                Vars,
                Context);
        false ->
            z_render:growl_error(?__("You are not allowed to send this page.", Context), Context)
    end;

event(#submit{message={mailing_page, Args}}, Context) ->
    PageId = m_rsc:rid(proplists:get_value(id, Args), Context),
    OnSuccess = proplists:get_all_values(on_success, Args),
    ListId = m_rsc:rid(z_context:get_q(<<"list_id">>, Context), Context),
    IsMatchLanguage = z_convert:to_bool(z_context:get_q(<<"is_match_language">>, Context)),
    IsSendAll = z_convert:to_bool(z_context:get_q(<<"is_send_all">>, Context)),
    When = z_context:get_q(<<"mail_when">>, Context),
    Options = [
        {is_match_language, IsMatchLanguage},
        {is_send_all, IsSendAll}
    ],
    case is_allowed(PageId, ListId, Context) of
        true ->
            When1 = case is_test_mailinglist(ListId, Context) of
                true -> <<"now">>;
                false -> When
            end,
            handle_mailing(When1, ListId, PageId, Options, OnSuccess, Context);
        false ->
            z_render:growl_error(?__("You are not allowed to send this page.", Context), Context)
    end.

dialog_title(true, Context) ->
    ?__("Confirm sending test mailing", Context);
dialog_title(false, Context) ->
    ?__("Confirm sending to mailinglist", Context).


handle_mailing(undefined, ListId, PageId, Options, OnSuccess, Context) ->
    handle_mailing(<<"now">>, ListId, PageId, Options, OnSuccess, Context);
handle_mailing(<<"now">>, ListId, PageId, Options, OnSuccess, Context) ->
    ok = mod_mailinglist:queue_mailing(ListId, PageId, Options, Context),
    finish(?__("The mailing has been queued for immediate sending...", Context), OnSuccess, Context);
handle_mailing(<<"scheduled">>, ListId, PageId, Options, OnSuccess, Context) ->
    ok = m_mailinglist:insert_scheduled(ListId, PageId, Options, Context),
    ok = mod_mailinglist:ensure_scheduled_task(Context),
    finish(
        ?__("The mailing will be sent when the page becomes visible.", Context),
        OnSuccess,
        Context);
handle_mailing(<<"date">>, ListId, PageId, Options, OnSuccess, Context) ->
    case mailing_date(Context) of
        {ok, Due} ->
            ok = m_mailinglist:insert_scheduled(ListId, PageId, Options, Due, Context),
            ok = mod_mailinglist:ensure_scheduled_task(Context),
            finish(?__("The mailing has been scheduled.", Context), OnSuccess, Context);
        {error, past} ->
            z_render:growl_error(?__("The mailing date must be in the future.", Context), Context);
        {error, invalid} ->
            z_render:growl_error(?__("Enter a valid mailing date and time.", Context), Context)
    end;
handle_mailing(_When, _ListId, _PageId, _Options, _OnSuccess, Context) ->
    z_render:growl_error(?__("Select when the mailing should be sent.", Context), Context).

finish(Message, OnSuccess, Context) ->
    Context1 = z_render:growl(Message, Context),
    z_render:wire([{dialog_close, []} | OnSuccess], Context1).

mailing_date(Context) ->
    Date = z_context:get_q(<<"dt:ymd:0:mailing_date">>, Context),
    Time = z_context:get_q(<<"dt:hi:0:mailing_date">>, Context),
    case z_utils:is_empty(Date) orelse z_utils:is_empty(Time) of
        true ->
            {error, invalid};
        false ->
            try
                Props = z_context:get_q_map_noz(Context),
                LocalDate = maps:get(<<"mailing_date">>, Props),
                Now = calendar:universal_time(),
                case z_datetime:to_utc(LocalDate, Context) of
                    undefined ->
                        {error, invalid};
                    Due when Due > Now ->
                        {ok, Due};
                    _Due ->
                        {error, past}
                end
            catch
                _:_ -> {error, invalid}
            end
    end.

is_allowed(PageId, ListId, Context) ->
    mod_mailinglist:is_allowed_to_send(ListId, PageId, Context).

is_test_mailinglist(ListId, Context) ->
    ListId =:= m_rsc:rid(mailinglist_test, Context).
