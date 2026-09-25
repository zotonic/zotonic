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
-moduledoc(#{
    zotonic_keywords => [
        "reference", "frontend_developer", "wire_action", "mailing_lists", "send_and_receive"
    ]
}).
-moduledoc("
Shows the dialog to mail the current page ([resource](/id/doc_glossary#term-resource)) to a mailing list. This is used
in the admin “mailing status” interface. A mailing can be sent immediately, when the page becomes publicly visible, or
at an explicitly selected date and time.\n").
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
    {PostbackMsgJS, _PickledPostback} = z_render:make_postback(
        Postback, click, TriggerId, TargetId, ?MODULE, Context
    ),
    {PostbackMsgJS, Context}.

event(#postback{message = {dialog_mailing_page, Id, ListId, OnSuccess}}, Context) ->
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
                Context
            );
        false ->
            z_render:growl_error(?__("You are not allowed to send this page.", Context), Context)
    end;
event(#postback{message = {mailing_resend_review, Args}}, Context) ->
    case m_mailinglist_run:get(proplists:get_value(run_id, Args), Context) of
        {ok, Run} ->
            case
                m_mailinglist_run:allowed(Run, Context) andalso
                    maps:get(<<"details_expired">>, Run, undefined) =:= undefined
            of
                true ->
                    Options = [
                        {parent_id, maps:get(<<"id">>, Run)},
                        {single_test_address,
                            proplists:get_value(
                                single_test_address, maps:get(<<"options">>, Run, [])
                            )},
                        {send_mode, proplists:get_value(mode, Args)},
                        {language, maps:get(<<"language">>, Run)},
                        {fallback_language, maps:get(<<"fallback_language">>, Run)},
                        {audience, maps:get(<<"audience">>, Run)}
                    ],
                    handle_mailing(
                        <<"now">>,
                        maps:get(<<"mailinglist_id">>, Run),
                        maps:get(<<"page_id">>, Run),
                        Options,
                        [],
                        Context
                    );
                false ->
                    z_render:growl_error(
                        ?__("You are not allowed to send this page.", Context), Context
                    )
            end;
        _ ->
            z_render:growl_error(?__("This mailing is unavailable.", Context), Context)
    end;
event(#postback{message = {mailing_back, Args}}, Context) ->
    Page = proplists:get_value(id, Args),
    List = proplists:get_value(list_id, Args),
    case is_allowed(Page, List, Context) of
        true ->
            z_render:dialog(
                dialog_title(is_test_mailinglist(List, Context), Context),
                "_dialog_mailing_page.tpl",
                [{is_test, is_test_mailinglist(List, Context)} | Args],
                Context
            );
        false ->
            z_render:growl_error(?__("You are not allowed to send this page.", Context), Context)
    end;
event(#postback{message = {mailing_confirm, Args}}, Context) ->
    List = proplists:get_value(list_id, Args),
    Page = proplists:get_value(page_id, Args),
    case
        m_mailinglist_run:create(
            List,
            Page,
            proplists:get_value(type, Args),
            proplists:get_value(due, Args),
            proplists:get_value(options, Args),
            Context
        )
    of
        {ok, RunId} ->
            mod_mailinglist:ensure_scheduled_task(Context),
            z_render:wire(
                [{dialog_close, []}, {redirect, [{dispatch, admin_mailing_run}, {run_id, RunId}]}],
                Context
            );
        {error, history_expired} ->
            z_render:growl_error(
                ?__(
                    "Recipient history has expired. Start a new mailing and explicitly select all recipients; some people may receive this page again.",
                    Context
                ),
                Context
            );
        {error, _} ->
            z_render:growl_error(
                ?__(
                    "Could not queue this mailing. Check your permissions and language selection.",
                    Context
                ),
                Context
            )
    end;
event(#submit{message = {mailing_page, Args}}, Context) ->
    PageId = m_rsc:rid(proplists:get_value(id, Args), Context),
    OnSuccess = proplists:get_all_values(on_success, Args),
    ListId = m_rsc:rid(z_context:get_q(<<"list_id">>, Context), Context),
    IsMatchLanguage = z_convert:to_bool(z_context:get_q(<<"is_match_language">>, Context)),
    IsSendAll = z_convert:to_bool(z_context:get_q(<<"is_send_all">>, Context)),
    When = z_context:get_q(<<"mail_when">>, Context),
    Draft =
        case proplists:get_value(options, Args) of
            D when is_list(D) -> D;
            _ -> []
        end,
    Options = [
        {single_test_address, proplists:get_value(single_test_address, Draft)},
        {parent_id, proplists:get_value(parent_id, Draft)},
        {is_match_language, IsMatchLanguage},
        {is_send_all, IsSendAll},
        {language, z_context:get_q(<<"mailing_language">>, Context, <<>>)},
        {fallback_language,
            z_context:get_q(
                <<"fallback_language">>, Context, m_mailinglist_run:fallback(PageId, Context)
            )},
        {audience, z_context:get_q(<<"audience">>, Context, <<"matching">>)},
        {send_mode, z_context:get_q(<<"send_mode">>, Context, <<"new">>)}
    ],
    case is_allowed(PageId, ListId, Context) of
        true ->
            When1 =
                case is_test_mailinglist(ListId, Context) of
                    true -> <<"now">>;
                    false -> When
                end,
            case z_context:get_q(<<"mailing_step">>, Context) of
                <<"test">> -> send_preview_test(ListId, PageId, Options, Context);
                _ -> handle_mailing(When1, ListId, PageId, Options, OnSuccess, Context)
            end;
        false ->
            z_render:growl_error(?__("You are not allowed to send this page.", Context), Context)
    end.

dialog_title(true, Context) ->
    ?__("Prepare test mailing", Context);
dialog_title(false, Context) ->
    ?__("Choose recipients and language", Context).

handle_mailing(undefined, ListId, PageId, Options, OnSuccess, Context) ->
    handle_mailing(<<"now">>, ListId, PageId, Options, OnSuccess, Context);
handle_mailing(When, ListId, PageId, Options, OnSuccess, Context) ->
    Schedule =
        case When of
            <<"now">> ->
                {ok, <<"date">>, calendar:universal_time()};
            <<"scheduled">> ->
                {ok, <<"publication">>,
                    case m_rsc:p(PageId, publication_start, Context) of
                        undefined -> calendar:universal_time();
                        D -> D
                    end};
            <<"date">> ->
                case mailing_date(Context) of
                    {ok, D} -> {ok, <<"date">>, D};
                    Error -> Error
                end;
            _ ->
                {error, invalid}
        end,
    case Schedule of
        {ok, Type, Due} ->
            try z_mailinglist_run:review(ListId, PageId, Options, Context) of
                #{counts := Counts, reasons := Reasons} ->
                    Rows = [
                        #{language => L, status => S, total => N}
                     || {{L, S}, N} <- lists:sort(maps:to_list(Counts))
                    ],
                    Vars = [
                        {id, PageId},
                        {list_id, ListId},
                        {type, Type},
                        {due, Due},
                        {counts, Rows},
                        {reasons, lists:sort(maps:to_list(Reasons))},
                        {eligible,
                            lists:sum([N || {{_, <<"pending">>}, N} <- maps:to_list(Counts)])},
                        {mail_when, When},
                        {mailing_date, z_context:get_q(<<"dt:ymd:0:mailing_date">>, Context)},
                        {mailing_time, z_context:get_q(<<"dt:hi:0:mailing_date">>, Context)},
                        {is_test, is_test_mailinglist(ListId, Context)},
                        {options, [{request_key, z_ids:id(32)} | Options]},
                        {on_success, OnSuccess}
                    ],
                    z_render:dialog(
                        ?__("Review mailing", Context), "_dialog_mailing_review.tpl", Vars, Context
                    )
            catch
                error:{badmatch, {error, history_expired}} ->
                    z_render:growl_error(
                        ?__(
                            "Recipient history has expired. Go back and select all recipients to send again.",
                            Context
                        ),
                        Context
                    );
                _:_ ->
                    z_render:growl_error(
                        ?__(
                            "Could not prepare the recipient estimate. Check the list query and language selection.",
                            Context
                        ),
                        Context
                    )
            end;
        {error, _} ->
            z_render:growl_error(
                ?__("Enter a valid future mailing date and time.", Context), Context
            )
    end.

%% Keep the mailing draft visible while a separate, single-address test is queued.
send_preview_test(ListId, PageId, Options, Context) ->
    Email = m_mailinglist:normalize_email(z_context:get_q(<<"test_email">>, Context)),
    case Email =/= undefined andalso z_email_utils:is_email(Email) of
        false ->
            z_render:growl_error(?__("Enter a valid test email address.", Context), Context);
        true ->
            Language = z_context:get_q(<<"test_language">>, Context),
            TestOptions = [
                {single_test_address, Email},
                {language, Language},
                {audience, <<"all">>},
                {send_mode, <<"all">>}
            ],
            case
                m_mailinglist_run:create(
                    m_rsc:rid(mailinglist_test, Context),
                    PageId,
                    <<"date">>,
                    calendar:universal_time(),
                    TestOptions,
                    Context
                )
            of
                {ok, RunId} ->
                    mod_mailinglist:ensure_scheduled_task(Context),
                    Vars = [
                        {id, PageId},
                        {list_id, ListId},
                        {options, Options},
                        {mail_when, z_context:get_q(<<"mail_when">>, Context)},
                        {mailing_date, z_context:get_q(<<"dt:ymd:0:mailing_date">>, Context)},
                        {mailing_time, z_context:get_q(<<"dt:hi:0:mailing_date">>, Context)},
                        {test_run_id, RunId},
                        {test_email, Email},
                        {test_language, Language}
                    ],
                    z_render:dialog(
                        ?__("Choose recipients and language", Context),
                        "_dialog_mailing_page.tpl",
                        Vars,
                        Context
                    );
                {error, _} ->
                    z_render:growl_error(
                        ?__(
                            "Could not send the test. Check the test mailing list and selected language.",
                            Context
                        ),
                        Context
                    )
            end
    end.

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
