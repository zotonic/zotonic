%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2026 Marc Worrell
%% @doc Survey module. Define surveys and generic forms and let people fill them in.
%% @end

%% Copyright 2010-2026 Marc Worrell
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

-module(mod_survey).
-moduledoc("
Adds the concept of survey [resources](/id/doc_glossary#term-resource): user-definable forms which can be created in the
admin interface and filled out by the website’s visitors.



Survey question types
---------------------

The following question types are defined in the survey:

| Type | Description |
| ==== | =========== |
| likert | Answer a question on a scale of 5 points, from “completely disagree” (1) to “completely agree” (5). |
| short answer | An open question with a single-lined text field. You have the option of specifying a validation like email, date, numeric. |
| long answer | An open question with a big text field. |
| matching | Question type which allows you to match given answers to each other. |
| thurstone | A multiple choice field. Like multiple choice, but more powerful. The choices are translatable, and you have the possibility to select either a single answer, multiple answers or submit the form directly when choosing an answer. |
| multiple choice | A simple multiple choice field that has the added option that the multiple choice can be a numeric value, in which case an overview of the total value will be shown in the printable list and beneath the survey pie chart. This is useful for creating forms which require you to enter an amount or quantity, e.g. for a reservation system. Multiple choice fields cannot currently be translated, use the “thurstone” question type in that case. |
| true or false | Answers a true or false question. You have the option to specify custom texts for both the options. |
| yes or no | Like true or false, answers a true or false question. You have the option to specify custom texts for both the options. |
| narrative | Question type for specifying inline questions in a narrative fashion. |
| category | Choose a single resource from a given category as the answer to this question. |
| country | Select a country from a drop-down list. |
| hidden | A hidden input value. Can be used to register that a specific page of questions has been submitted. |
| upload | Upload a file. Can be used on the last survey page, you have to add your own survey handler to handle the uploaded file. |
| header | Renders a sub-heading between questions. |
| prompt | Renders an extra prompt block. |
| text block | Renders a text block between questions. |


Intercepting survey submissions
-------------------------------

When a survey is submitted, the survey module sends out a `#survey_submit{}` notification.

This notification has the following fields:

*   id - The id of survey being submitted
*   handler - A handler name (see below)
*   answers - The answers that were filled in
*   missing - answers that were missing
*   answers\\_raw - Unprocessed answers, e.g. the raw submission

To intercept a survey submission you would observe this survey\\_submit notification, and return `ok`:


```erlang
observe_survey_submit(#survey_submit{ id = SurveyId }, Context) ->
    ?DEBUG(SurveyId),
    ok.
```



Creating a custom survey handler
--------------------------------

The survey edit page has a dropdown for so-called “survey handlers”. A survey handler is a property that is set on
the resource that indicates the handler that needs to be taken. Handlers are collected using the
`#survey_get_handlers{}` fold notification.

For instance, the following defines a handler called “email\\_me”:


```erlang
observe_survey_get_handlers(#survey_get_handlers{}, All, Context) ->
  [
   {<<\"email_me\">>, ?__(<<\"E-mail me when survey is submitted\">>, Context)}
   | All
  ].
```

Each handler will show up in the dropdown list and the editor can pick which handler he wants. The value chosen is
passed along in the `handler` property of the survey submission, and as such can be used to intercept the survey submission:


```erlang
observe_survey_submit(#survey_submit{ handler = <<\"email_me\">>, id = SurveyId }, Context) ->
    %% Do something here for surveys which have 'email_me' selected as handler
    ok;
observe_survey_submit(#survey_submit{}, _Context) ->
    %% Let other surveys use the default submision mechanism
    undefined.
```



Configurations keys
-------------------

In the survey result editor it is possible to link an answer to a newly created person.

The category and content group for this person can be configured via the following two keys:

*   `mod_survey.person_category`, default to `person`
*   `mod_survey.person_content_group`, defaults to `default_content_group`

Todo

Add more documentation
").
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Survey").
-mod_description("Create and publish questionnaires.").
-mod_prio(400).
-mod_schema(7).
-mod_depends([ admin, mod_wires ]).
-mod_provides([ survey, poll ]).
-mod_config([
        #{
            key => person_category,
            type => string,
            default => "person",
            description => "The category used for the person resource when creating a user from a survey result."
        },
        #{
            key => person_content_group,
            type => string,
            default => "default_content_group",
            description => "The content group used for the person resource when creating a user from a survey result. "
                           "If empty the default group for the current ACL module is used."
        }
    ]).

%% interface functions
-export([
    manage_schema/2,
    event/2,
    observe_admin_edit_blocks/3,
    observe_admin_rscform/3,
    observe_survey_is_submit/2,

    observe_rsc_merge/2,

    observe_export_resource_filename/2,
    observe_export_resource_header/2,
    observe_export_resource_data/2,

    observe_acl_is_allowed/2,

    observe_tick_24h/2,

    get_page/3,

    register_nonce/1,
    unregister_nonce/1,
    do_submit/4,
    save_submit/2,
    survey_start/2,

    is_page_back_allowed/3,

    collect_answers/4,
    render_next_page/8,
    go_button_target/4,
    module_name/1
]).

%% Testing
-export([
    drop_till_page/2,
    page_has_feedback_view/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_survey/include/survey.hrl").

% Used for the timeout of the nonce set when starting to fill in
% a survey. After this the nonce is not valid anymore. As this nonce
% is only used to catch quick successions of the submitting the same
% form we can set it to a very long validity.
-define(SURVEY_FILL_NONCE_TIMEOUT, ?WEEK).

%% @doc Schema for mod_survey lives in separate module
manage_schema(What, Context) ->
    survey_schema:manage_schema(What, Context).

event(#postback{message={survey_start, Args}}, Context) ->
    Update = survey_start(Args, Context),
    render_update(Update, Args, Context);

event(#submit{message={survey_next, Args}}, Context) ->
    {id, SurveyId} = proplists:lookup(id, Args),
    {answers, Answers} = proplists:lookup(answers, Args),
    {history, History} = proplists:lookup(history, Args),
    Editing = proplists:get_value(editing, Args),
    case z_convert:to_bool(z_context:get_q(<<"z_formnovalidate">>, Context)) of
        true ->
            case z_context:get_q(<<"z_submitter">>, Context) of
                <<"z_survey_save">> ->
                    if
                        Editing =:= undefined -> save_page(SurveyId, Answers, Args, Context);
                        true -> ok
                    end,
                    z_render:wire({script, [ {script, <<"z_event('survey-stop-confirm');">>} ]}, Context);
                _ ->
                    % Back button pressed, no validation of query args.
                    case History of
                        [_,PageNr|History1] ->
                            case is_page_back_allowed(SurveyId, PageNr, Context) of
                                true ->
                                    render_update(
                                        render_next_page(SurveyId, PageNr, exact, Answers, History1, Editing, Args, Context),
                                        Args, Context);
                                false ->
                                    Context
                            end;
                        _History ->
                            render_update(
                                render_next_page(SurveyId, 0, exact, Answers, [], Editing, Args, Context),
                                Args, Context)
                    end
            end;
        false ->
            % Submit button pressed, query args are validated.
            {page_nr, PageNr} = proplists:lookup(page_nr, Args),
            render_update(
                render_next_page(SurveyId, PageNr+1, forward, Answers, History, Editing, Args, Context),
                Args, Context)
    end;

event(#postback{message={survey_back, Args}}, Context) ->
    {id, SurveyId} = proplists:lookup(id, Args),
    {answers, Answers} = proplists:lookup(answers, Args),
    {history, History} = proplists:lookup(history, Args),
    Editing = proplists:get_value(editing, Args),
    case History of
        [_,PageNr|History1] ->
            render_update(
                render_next_page(SurveyId, PageNr, exact, Answers, History1, Editing, Args, Context),
                Args, Context);
        _History ->
            render_update(
                render_next_page(SurveyId, 0, exact, Answers, [], Editing, Args, Context),
                Args, Context)
    end;

event(#postback{message={survey_remove_result_confirm, Args}}, Context) ->
    {id, SurveyId} = proplists:lookup(id, Args),
    {answer_id, AnswerId} = proplists:lookup(answer_id, Args),

    case z_acl:is_allowed(delete_result, #acl_survey{id=SurveyId, answer_id=AnswerId}, Context) of
        true ->
            z_render:wire({confirm, [
                    {is_dangerous_action, true},
                    {text, ?__("Are you sure you want to delete this result?", Context)},
                    {ok, ?__("Delete", Context)},
                    {postback, {survey_remove_result, Args}},
                    {delegate, ?MODULE}
                ]},
                Context);
        false ->
            z_render:growl(?__("You are not allowed to change these results.", Context), Context)
    end;

event(#postback{message={survey_remove_result, Args}}, Context) ->
    {id, SurveyId} = proplists:lookup(id, Args),
    {answer_id, AnswerId} = proplists:lookup(answer_id, Args),
    case z_acl:is_allowed(delete_result, #acl_survey{id=SurveyId, answer_id=AnswerId}, Context) of
        true ->
            m_survey:delete_result(SurveyId, AnswerId, Context),
            Target = "survey-result-"++z_convert:to_list(AnswerId),
            z_render:wire([
                    {growl, [{text, ?__("Result deleted.", Context)}]},
                    {slide_fade_out, [{target, Target}]}
                ], Context);
        false ->
            z_render:growl(?__("You are not allowed to change these results.", Context), Context)
    end.

%% @doc Append the possible blocks for a survey's edit page.
observe_admin_edit_blocks(#admin_edit_blocks{id=Id}, Menu, Context) ->
    case m_rsc:is_a(Id, survey, Context) of
        true ->
            [
                {100, ?__("Questions", Context), [
                    {survey_truefalse, ?__("True/False", Context)},
                    {survey_yesno, ?__("Yes/No", Context)},
                    {survey_likert, ?__("Likert", Context)},
                    {survey_thurstone, ?__("Thurstone", Context)},
                    {survey_category, ?__("Category", Context)},
                    {survey_matching, ?__("Matching", Context)},
                    {survey_narrative, ?__("Narrative", Context)},
                    {survey_short_answer, ?__("Short answer", Context)},
                    {survey_long_answer, ?__("Long answer", Context)},
                    {survey_country, ?__("Country select", Context)},
                    {survey_button, ?__("Button", Context)},
                    {survey_page_break, ?__("Page break", Context)},
                    {survey_page_options, ?__("Page options", Context)},
                    {survey_stop, ?__("Stop", Context)},
                    {survey_upload, ?__("File upload", Context)},
                    {survey_multiple_choice, ?__("Multiple choice", Context)}
                ]}
                | Menu
            ];
        false ->
            Menu
    end.

%% @doc Redo the page jumps into correct page break blocks
observe_admin_rscform(#admin_rscform{is_a=IsA}, Post, _Context) ->
    case lists:member(survey, IsA) of
        true -> survey_admin:admin_rscform(Post);
        false -> Post
    end.


%% @doc Check if the given block is a survey question with submit button
observe_survey_is_submit(#survey_is_submit{block=Q}, _Context) ->
    case maps:get(<<"type">>, Q, undefined) of
        <<"survey_button">> -> true;
        <<"survey_", _/binary>> -> maps:get(<<"input_type">>, Q, undefined) =:= <<"submit">>;
        _ -> undefined
    end.

%% @doc Rename the answers of the loser to the winner
observe_rsc_merge(#rsc_merge{winner_id=WinnerId, loser_id=LoserId}, Context) ->
    m_survey:rsc_merge(WinnerId, LoserId, Context).

%% @doc Fetch the filename for the export
observe_export_resource_filename(#export_resource_filename{dispatch=survey_results_download, id=Id}, Context) ->
    case m_survey:is_allowed_results_download(Id, Context) of
        true ->
            Filename = lists:flatten([
                            "survey-",
                            integer_to_list(Id),
                            case m_rsc:p(Id, slug, Context) of
                                undefined -> "";
                                <<>> -> "";
                                Slug -> [$-|z_convert:to_list(Slug)]
                            end]),
            {ok, Filename};
        false ->
            throw({stop_request, 403})
    end;
observe_export_resource_filename(#export_resource_filename{}, _Context) ->
    undefined.

%% @doc Fetch the header for the survey download
observe_export_resource_header(#export_resource_header{dispatch=survey_results_download, id=Id}, Context) ->
    case m_survey:is_allowed_results_download(Id, Context) of
        true ->
            {Hs, Prompts, Data} = m_survey:survey_results_prompts(Id, false, Context),
            Data1 = [ Row || {_Id, Row} <- Data ],
            {ok, Hs, [ Prompts | Data1 ]};
        false ->
            throw({stop_request, 403})
    end;
observe_export_resource_header(#export_resource_header{}, _Context) ->
    undefined.

%% @doc Fetch all ids making up the export, handles collections and search queries.
observe_export_resource_data(#export_resource_data{dispatch=survey_results_download, state=Data}, _Context) ->
    {ok, Data, undefined};
observe_export_resource_data(#export_resource_data{}, _Context) ->
    undefined.

%% @doc Check access to the survey answers.
observe_acl_is_allowed(#acl_is_allowed{
        action = view_result,
        object = #acl_survey{
            id = SurveyId,
            answer_id = AnswerId
        }}, Context) ->
    z_acl:rsc_editable(SurveyId, Context) orelse m_survey:is_answer_user(AnswerId, Context);
observe_acl_is_allowed(#acl_is_allowed{
        action = update_result,
        object = #acl_survey{
            id = SurveyId,
            answer_id = AnswerId
        }}, Context) ->
    z_acl:rsc_editable(SurveyId, Context)
    orelse (        z_convert:to_integer(m_rsc:p_no_acl(SurveyId, <<"survey_multiple">>, Context)) =:= 2
            andalso m_survey:is_answer_user(AnswerId, Context));
observe_acl_is_allowed(#acl_is_allowed{
        action = delete_result,
        object = #acl_survey{ id = SurveyId }}, Context) ->
    z_acl:rsc_editable(SurveyId, Context);
observe_acl_is_allowed(#acl_is_allowed{
    action = Action,
    object = #acl_mqtt{
        topic = [ <<"user">>, UserId, <<"survey-submission">>, SurveyId | _ ]
    }}, Context) when Action =:= subscribe; Action =:= publish ->
    CurrentUser = z_acl:user(Context),
    UserId1 = m_rsc:rid(UserId, Context),
    SurveyId1 = m_rsc:rid(SurveyId, Context),
    if
        UserId1 =:= CurrentUser ->
            z_acl:rsc_visible(SurveyId1, Context);
        true ->
            z_acl:rsc_editable(SurveyId1, Context)
    end;
observe_acl_is_allowed(#acl_is_allowed{}, _Context) ->
    undefined.

%% @doc Every day prune old saved intermediate survey results.
observe_tick_24h(tick_24h, Context) ->
    m_survey_saved:prune_saved(Context).


-spec survey_start(Args, Context) -> Update when
    Args :: proplists:proplist(),
    Context :: z:context(),
    Update :: z:context() | #render{}.
survey_start(Args, Context) ->
    {id, SurveyId} = proplists:lookup(id, Args),
    AnswerId = z_convert:to_integer(proplists:get_value(answer_id, Args)),
    case is_integer(AnswerId) andalso z_acl:rsc_editable(SurveyId, Context) of
        true ->
            {Answers, ResultUserId} = case m_survey:single_result(SurveyId, AnswerId, Context) of
                [] ->
                    {[], undefined};
                Result ->
                    As = proplists:get_value(answers, Result, []),
                    As1 = lists:map(
                        fun({QName, Ans}) ->
                            Answer = proplists:get_value(answer, Ans),
                            {QName, Answer}
                        end,
                        As),
                    {As1, proplists:get_value(user_id, Result)}
            end,
            Editing = {editing, AnswerId, undefined},
            Args1 = [
                {answer_user_id, ResultUserId},
                {survey_session_nonce, z_nonce:nonce(?SURVEY_FILL_NONCE_TIMEOUT)},
                {viewer, proplists:get_value(viewer, Args)},
                {element_id, proplists:get_value(element_id, Args)}
                | proplists:delete(answer_user_id,
                    proplists:delete(survey_session_nonce,
                        proplists:delete(viewer,
                            proplists:delete(element_id, Args))))
            ],
            render_next_page(SurveyId, 1, exact, Answers, [], Editing, Args1, Context);
        false ->
            Editing = proplists:get_value(editing, Args),
            MaybePrevArgs = case m_survey:is_save_intermediate(SurveyId, Context) of
                true when Editing =:= undefined ->
                    m_survey_saved:get_saved(SurveyId, Context);
                false ->
                    {error, enoent}
            end,
            case MaybePrevArgs of
                {ok, #{
                    <<"page_nr">> := PageNr,
                    <<"saved_args">> := PrevArgs
                }} ->
                    {answers, Answers} = proplists:lookup(answers, PrevArgs),
                    History = proplists:get_value(history, PrevArgs, []),
                    PrevArgs1 = [
                        {survey_session_nonce, z_nonce:nonce(?SURVEY_FILL_NONCE_TIMEOUT)},
                        {viewer, proplists:get_value(viewer, Args)},
                        {element_id, proplists:get_value(element_id, Args)}
                        | proplists:delete(survey_session_nonce,
                            proplists:delete(viewer,
                                proplists:delete(element_id, PrevArgs)))
                    ],
                    render_next_page(SurveyId, PageNr, exact, Answers, History, undefined, PrevArgs1, Context);
                {error, _} ->
                    Answers = normalize_answers(proplists:get_value(answers, Args)),
                    Args1 = [
                        {answer_user_id, z_acl:user(Context)},
                        {survey_session_nonce, z_nonce:nonce(?SURVEY_FILL_NONCE_TIMEOUT)}
                        | proplists:delete(answer_user_id,
                            proplists:delete(survey_session_nonce, Args))
                    ],
                    render_next_page(SurveyId, 1, exact, Answers, [], Editing, Args1, Context)
            end
    end.

%% @doc Fetch all the blocks for the specific survey page.
get_page(Id, Nr, #context{} = Context) when is_integer(Nr) ->
    case m_rsc:p(Id, <<"blocks">>, Context) of
        Qs when is_list(Qs) ->
            go_page(Nr, Qs, [], exact, Context);
        _ ->
            []
    end.

%% @doc Register a nonce to prevent duplicate form submits by people
%% clicking multiple times on the submit button.
-spec register_nonce(SessionNonce) -> ok | {error, Reason} when
    SessionNonce :: binary() | undefined,
    Reason :: duplicate | overload | key | expired.
register_nonce(undefined) ->
    ok;
register_nonce(<<>>) ->
    ok;
register_nonce(SessionNonce) when is_binary(SessionNonce) ->
    z_nonce:register(SessionNonce).

-spec unregister_nonce(SessionNonce) -> ok | {error, key} when
    SessionNonce :: binary() | undefined.
unregister_nonce(undefined) ->
    ok;
unregister_nonce(<<>>) ->
    ok;
unregister_nonce(SessionNonce) when is_binary(SessionNonce) ->
    z_nonce:unregister(SessionNonce).


%%====================================================================
%% support functions
%%====================================================================


%% @doc Check if the given page is configured to allow to go back to that page
%% from a later page.
-spec is_page_back_allowed(SurveyId, PageNr, Context) -> boolean() when
    SurveyId :: m_rsc:resource_id(),
    PageNr :: integer(),
    Context :: z:context().
is_page_back_allowed(SurveyId, PageNr, Context) ->
    case m_rsc:p(SurveyId, <<"blocks">>, Context) of
        Questions when is_list(Questions) ->
            case drop_till_page(PageNr, Questions) of
                {[], _} ->
                    false;
                {L, _} ->
                    is_page_back_allowed(takepage(L))
            end;
        _ ->
            false
    end.

is_page_back_allowed(L) ->
    not lists:any(fun has_no_page_back_option/1, L).

has_no_page_back_option(#{ <<"type">> := <<"survey_page_options">>, <<"is_no_back">> := IsHideBack }) ->
    IsHideBack;
has_no_page_back_option(_) ->
    false.


%% @doc If the survey is set to save intermediate results then those are saved to the
%% survey_saved model.
-spec maybe_save_intermediate_results(Editing, SurveyId, PageNr, SubmitArgs, Context) -> ok | {error, Reason} when
    Editing :: term() | undefined,
    SurveyId :: m_rsc:resource_id(),
    PageNr :: non_neg_integer(),
    SubmitArgs :: proplists:proplist(),
    Context :: z:context(),
    Reason :: term().
maybe_save_intermediate_results(undefined, SurveyId, PageNr, SubmitArgs, Context) ->
    case m_survey:is_save_intermediate(SurveyId, Context) of
        true -> m_survey_saved:put_saved(SurveyId, PageNr, SubmitArgs, Context);
        _ -> ok
    end;
maybe_save_intermediate_results(_Editing, _SurveyId, _PageNr, _SubmitArgs, _Context) ->
    ok.

normalize_answers(undefined) -> [];
normalize_answers(L) -> lists:map(fun normalize_answer/1, L).

normalize_answer(A) when is_binary(A) -> {A, <<"1">>};
normalize_answer(A) when is_atom(A) -> {z_convert:to_binary(A), <<"1">>};
normalize_answer({A, undefined}) -> {z_convert:to_binary(A), <<>>};
normalize_answer({A, true}) -> {z_convert:to_binary(A), <<"1">>};
normalize_answer({A, false}) -> {z_convert:to_binary(A), <<"0">>};
normalize_answer({A, [B|_] = V}) when is_binary(B) -> {z_convert:to_binary(A), V};
normalize_answer({A, [L|_] = V}) when is_list(L) ->
    V1 = [ z_convert:to_binary(X) || X <- V ],
    {z_convert:to_binary(A), V1};
normalize_answer({A, V}) -> {z_convert:to_binary(A), z_convert:to_binary(V)};
normalize_answer([A, V]) -> normalize_answer({A,V}).


render_update(#context{} = RenderContext, _Args, _Context) ->
    RenderContext;
render_update(#render{} = Render, Args, Context) ->
    TargetId = proplists:get_value(element_id, Args, <<"survey-question">>),
    z_render:update(TargetId, Render, Context).

%% @doc Merge the current submit with the known answers and save the intermediate results.
save_page(SurveyId, Answers, Args, Context) when is_integer(SurveyId) ->
    case m_survey:is_save_intermediate(SurveyId, Context) of
        true ->
            AnswersNoValidate = z_convert:to_list(proplists:get_value(answers_novalidate, Args, [])),
            {SubmittedAnswers, _Submitter0} = get_args(Context),
            SubmittedAnswers1 = group_multiselect(SubmittedAnswers),
            % Remove the newly submitted question answers from both the validated and the unvalidated lists.
            % The user could be backing through multiple pages, effectively removing the answers from the validated answers.
            Answers1 = lists:foldl(fun({Arg,_Val}, Acc) -> proplists:delete(Arg, Acc) end, Answers, SubmittedAnswers1),
            AnswersNoValidate1 = lists:foldl(fun({Arg,_Val}, Acc) -> proplists:delete(Arg, Acc) end, AnswersNoValidate, SubmittedAnswers1),
            AnswersNoValidate2 = AnswersNoValidate1 ++ SubmittedAnswers1,
            SavedArgs = [
                {answers, Answers1},
                {answers_novalidate, AnswersNoValidate2}
                | proplists:delete(answers,
                    proplists:delete(answers_novalidate, Args))
            ],
            PageNr = proplists:get_value(page_nr, Args, 1),
            m_survey_saved:put_saved(SurveyId, PageNr, SavedArgs, Context);
        false ->
            ok
    end.

%% @doc Fetch the next page from the survey, update the page view
-spec render_next_page(SurveyId, PageNr, Direction, Answers, History, Editing, Args, Context) -> Result
    when SurveyId :: m_rsc:resource_id(),
         PageNr :: non_neg_integer(),
         Direction :: exact|forward,
         Answers :: list(),
         History :: list(),
         Editing :: term()|undefined,
         Args :: proplists:proplist(),
         Context :: z:context(),
         Result :: #render{} | z:context().
render_next_page(SurveyId, 0, _Direction, _Answers, _History, _Editing, Args, Context) when is_integer(SurveyId) ->
    case z_convert:to_binary(proplists:get_value(viewer, Args)) of
        <<"overlay">> ->
            z_render:overlay_close(Context);
        <<"dialog">> ->
            z_render:dialog_close(Context);
        _ ->
            z_render:wire({redirect, [{id, SurveyId}]}, Context)
    end;
render_next_page(SurveyId, PageNr, Direction, Answers, History, Editing, Args, Context) when is_integer(SurveyId) ->
    Viewer = z_convert:to_binary(proplists:get_value(viewer, Args)),
    AnswersNoValidate = z_convert:to_list(proplists:get_value(answers_novalidate, Args, [])),
    {Answers2, AnswersNoValidate2, Submitter} = case proplists:get_value(is_feedback_view, Args) of
        true ->
            {Answers, AnswersNoValidate, proplists:get_value(feedback_submitter, Args)};
        _ ->
            {SubmittedAnswers, Submitter0} = get_args(Context),
            SubmittedAnswers1 = group_multiselect(SubmittedAnswers),
            % Remove the newly submitted question answers from both the validated and the unvalidated lists.
            % The user could be backing through multiple pages, effectively removing the answers from the validated answers.
            Answers1 = lists:foldl(fun({Arg,_Val}, Acc) -> proplists:delete(Arg, Acc) end, Answers, SubmittedAnswers1),
            AnswersNoValidate1 = lists:foldl(fun({Arg,_Val}, Acc) -> proplists:delete(Arg, Acc) end, AnswersNoValidate, SubmittedAnswers1),
            case z_convert:to_bool(z_context:get_q(<<"z_formnovalidate">>, Context)) of
                true when Direction =:= exact ->
                    % Back - form was not validated
                    {Answers1, AnswersNoValidate1 ++ SubmittedAnswers1, Submitter0};
                false ->
                    % Forward - form was validated
                    {Answers1 ++ SubmittedAnswers1, AnswersNoValidate1, Submitter0}
            end
    end,
    case m_rsc:p(SurveyId, <<"blocks">>, Context) of
        Questions when is_list(Questions) ->

            IsFeedbackNeeded = Direction =:= forward
                        andalso proplists:get_value(is_feedback_view, Args) =/= true
                        andalso page_has_feedback_view(PageNr - 1, Questions),

            {Next, IsFeedbackView} = if
                IsFeedbackNeeded ->
                    {go_page(PageNr-1, Questions, Answers2, exact, Context), true};
                Submitter =:= undefined ->
                    {go_page(PageNr, Questions, Answers2, Direction, Context), false};
                true ->
                    {go_button_target(Submitter, Questions, Answers2, Context), false}
            end,

            case Next of
                {L, NewPageNr} when is_list(L) ->

                    % Maybe save the intermediate results.
                    SavedArgs = [
                        {answers, Answers2},
                        {answers_novalidate, AnswersNoValidate2},
                        {page_nr, NewPageNr},
                        {is_feedback_view, false}
                        | proplists:delete(answers,
                            proplists:delete(answers_novalidate,
                                proplists:delete(page_nr,
                                    proplists:delete(is_feedback_view, Args))))
                    ],
                    maybe_save_intermediate_results(Editing, SurveyId, PageNr, SavedArgs, Context),

                    AnswersFeedback = if
                        IsFeedbackView ->
                            {CollectFoundAnswers, _CollectMissing} = collect_answers(SurveyId, Questions, Answers2, Context),
                            survey_answers_to_storage(CollectFoundAnswers);
                        true ->
                            undefined
                    end,

                    % A new list of questions, PageNr might be another than expected
                    TargetId = proplists:get_value(element_id, Args, <<"survey-question">>),
                    SurveySessionNonce = proplists:get_value(survey_session_nonce, Args),
                    Vars = [
                        {id, SurveyId},
                        {element_id, TargetId},
                        {page_nr, NewPageNr},
                        {questions, L},
                        {pages, count_pages(Questions)},
                        {answers, Answers2},
                        {answers_novalidate, AnswersNoValidate2},
                        {answer_user_id, proplists:get_value(answer_user_id, Args)},
                        {history, push_history(NewPageNr, History)},
                        {editing, Editing},
                        {viewer, Viewer},
                        {survey_session_nonce, SurveySessionNonce},
                        {is_feedback_view, IsFeedbackView},
                        {feedback_result, #{ <<"answers">> => AnswersFeedback}},
                        {feedback_submitter, Submitter}
                    ],
                    #render{template="_survey_question_page.tpl", vars=Vars};

                {error, {not_found, Name}} ->
                    ?LOG_ERROR(#{
                        text => <<"Survey error, page not found">>,
                        in => zotonic_mod_survey,
                        result => error,
                        reason => page_not_found,
                        survey_id => SurveyId,
                        page_name => Name
                    }),
                    NameSafe = z_html:escape(Name),
                    #context{} = z_render:growl_error(<<"Error in survey, could not find page ", NameSafe/binary>>, Context);

                {error, Reason} ->
                    ?LOG_ERROR(#{
                        text => <<"Survey error evaluating submit">>,
                        in => zotonic_mod_survey,
                        result => error,
                        reason => Reason,
                        page_nr => PageNr,
                        survey_id => SurveyId
                    }),
                    z_render:growl_error("Error evaluating submit.", Context);

                stop ->
                    render_next_page(SurveyId, 0, Direction, Answers, History, Editing, Args, Context);

                submit ->
                    %% That was the last page. Show a thank you and save the result.
                    case do_submit(SurveyId, Questions, Answers2, Editing, Args, Context) of
                        ok ->
                            IsShowResults = z_convert:to_bool(m_rsc:p(SurveyId, <<"survey_show_results">>, Context)),
                            render_result_page(SurveyId, Editing, IsShowResults, History, Viewer, Args, Context);
                        {ok, #context{} = SubmitContext} ->
                            SubmitContext;
                        {ok, #render{} = SubmitRender} ->
                            SubmitRender;
                        {error, Reason} ->
                            ?LOG_WARNING(#{
                                in => zotonic_mod_survey,
                                text => <<"Survey submission error">>,
                                result => error,
                                reason => Reason,
                                survey_id => SurveyId
                            }),
                            #render{
                                template="_survey_error.tpl",
                                vars=[
                                    {id, SurveyId},
                                    {history, History},
                                    {viewer, Viewer}
                                ]}
                    end
            end;
        _NoBlocks ->
            % No survey defined, show an error page.
            #render{
                template="_survey_error.tpl",
                vars=[
                    {id, SurveyId},
                    {viewer, Viewer}
                ]}
    end.

push_history(PageNr, []) -> [ PageNr ];             % first page
push_history(PageNr, [PageNr|_] = H) -> H;          % stay
push_history(PageNr, [_,PageNr|H]) -> [PageNr|H];   % back
push_history(PageNr, H) -> [ PageNr | H ].          % forward

page_has_feedback_view(0, _Qs) ->
    false;
page_has_feedback_view(Nr, Qs) ->
    case drop_till_page(Nr, Qs) of
        {[], _Nr} -> false;
        {L, _Nr1} ->
            case has_feedback(L) of
                true -> not is_page_back_allowed(takepage(L));
                false -> false
            end
    end.

has_feedback([]) -> false;
has_feedback([B|Bs]) ->
    case is_page_end(B) of
        true -> false;
        false ->
            case has_feedback_1(B) of
                true -> true;
                false -> has_feedback(Bs)
            end
    end.

has_feedback_1(#{ <<"is_test_direct">> := true }) ->
    false;
has_feedback_1(B) ->
    not z_utils:is_empty(maps:get(<<"test_correct">>, B, undefined))
    orelse not z_utils:is_empty(maps:get(<<"test_wrong">>, B, undefined)).


-spec render_result_page(SurveyId, Editing, IsShowResults, History, Viewer, Args, Context) -> Result when
    SurveyId :: m_rsc:resource_id(),
    Editing :: undefined | {editing, AnswerId, list()},
    AnswerId :: integer(),
    IsShowResults :: boolean(),
    History :: list(),
    Viewer :: binary(),
    Args :: list(),
    Context :: z:context(),
    Result :: z:context() | #render{}.
render_result_page(SurveyId, undefined, true, History, Viewer, _Args, _Context) ->
    #render{
        template="_survey_results.tpl",
        vars=[
            {id, SurveyId},
            {inline, true},
            {history, History},
            {viewer, Viewer}
        ]
    };
render_result_page(SurveyId, undefined, false, History, Viewer, _Args, _Context) ->
    #render{
        template="_survey_end.tpl",
        vars=[
            {id, SurveyId},
            {history, History},
            {viewer, Viewer}
        ]
    };
render_result_page(SurveyId, {editing, _AnswerId, []}, _IsShowResults, _History, Viewer, _Args, Context) ->
    Context1 = z_render:update(
            "survey-results",
            #render{
                template="_admin_survey_editor_results.tpl",
                vars=[
                    {id, SurveyId},
                    {viewer, Viewer}
                ]
            },
            Context),
    viewer_close(Viewer, Context1);
render_result_page(SurveyId, {editing, _AnswerId, undefined}, true, History, Viewer, _Args, _Context) ->
    #render{
        template="_survey_results.tpl",
        vars=[
            {id, SurveyId},
            {inline, true},
            {history, History},
            {viewer, Viewer}
        ]
    };
render_result_page(SurveyId, {editing, _AnswerId, undefined}, false, _History, Viewer, _Args, _Context) ->
    #render{
        template="_survey_end_edit.tpl",
        vars=[
            {id, SurveyId},
            {inline, true},
            {is_editing, true},
            {viewer, Viewer}
        ]
    };
render_result_page(SurveyId, {editing, _AnswerId, Actions}, true, History, Viewer, Args, Context) ->
    ContextActions = z_render:wire(Actions, Context),
    TargetId = proplists:get_value(element_id, Args, <<"survey-question">>),
    z_render:update(
        TargetId,
        #render{
            template="_survey_results.tpl",
            vars=[
                {id, SurveyId},
                {inline, true},
                {history, History},
                {viewer, Viewer}
            ]
        },
        ContextActions);
render_result_page(_SurveyId, {editing, _AnswerId, Actions}, false, _History, Viewer, _Args, Context) ->
    ContextActions = z_render:wire(Actions, Context),
    viewer_close(Viewer, ContextActions).

viewer_close(<<"dialog">>, Context) ->
    z_render:dialog_close(Context);
viewer_close(<<"overlay">>, Context) ->
    z_render:overlay_close(Context);
viewer_close(_, Context) ->
    Context.


get_args(Context) ->
    Args = [ {K,V} || {K,V} <- z_context:get_q_all_noz(Context), is_binary(V), K =/= <<"*">> ],
    Submitter = z_context:get_q(<<"z_submitter">>, Context),
    Buttons = proplists:get_all_values(<<"survey$button">>, Args),
    WithButtons = lists:foldl(
        fun
            (B, Acc) when B =:= Submitter ->
                [ {B,<<"yes">>} | proplists:delete(B, Acc) ];
            (B, Acc) ->
                [ {B, <<"no">>} | Acc ]
        end,
        Args,
        Buttons),
    Args1 = lists:filter(
        fun
            ({<<"survey$button">>, _}) -> false;
            ({_, _}) -> true
        end,
        WithButtons),
    Submitter1 = case lists:member(Submitter, Buttons) of
        true -> Submitter;
        false -> undefined
    end,
    {Args1, Submitter1}.

group_multiselect([]) ->
    [];
group_multiselect(As) ->
    group_multiselect(lists:sort(As), undefined, [], []).

group_multiselect([], K, [V], Acc) -> [{K,V}|Acc];
group_multiselect([], K, Vs, Acc) -> [{K,Vs}|Acc];
group_multiselect([{K,V}|KVs], undefined, [], Acc) -> group_multiselect(KVs, K, [V], Acc);
group_multiselect([{K,V}|KVs], K, Vs, Acc) -> group_multiselect(KVs, K, [V|Vs], Acc);
group_multiselect([{K,V}|KVs], K1, [V1], Acc) -> group_multiselect(KVs, K, [V], [{K1,V1}|Acc]);
group_multiselect([{K,V}|KVs], K1, V1s, Acc) -> group_multiselect(KVs, K, [V], [{K1,V1s}|Acc]).


%% @doc Count the number of pages in the survey
count_pages([]) ->
    0;
count_pages(L) ->
    count_pages(L, 1).

count_pages([], N) ->
    N;
count_pages([Q|L], N) ->
    case is_page_end(Q) of
        true ->
            case lists:dropwhile(fun is_page_end/1, L) of
                [] -> N;
                L1 -> count_pages(L1, N+1)
            end;
        false ->
            count_pages(L, N)
    end.


go_button_target(Submitter, Questions, Answers, Context) ->
    [Button|_] = lists:dropwhile(
        fun
            (#{ <<"name">> := NS }) when NS =:= Submitter -> false;
            (_) -> true
        end,
        Questions),
    TargetName = maps:get(<<"target">>, Button, <<>>),
    % The target might be a jump block, evaluate any jump conditions at
    % the block, or just skip if the jump target is a normal question block.
    case eval_page_jumps(fetch_question_name(Questions, TargetName, 1, in_q), Answers, Context) of
        stop -> stop;
        submit -> submit;
        {error, _} = Error -> Error;
        {L1, Nr1} ->
            L2 = takepage(L1),
            {L2, Nr1}
    end.

go_page(Nr, Qs, _Answers, exact, _Context) ->
    case drop_till_page(Nr, Qs) of
        {[], _Nr} -> submit;
        {PageStart, Nr1} ->
            Page = takepage(PageStart),
            {Page, Nr1}
    end;
go_page(Nr, Qs, Answers, forward, Context) ->
    {PrevPageStart, _Nr1} = drop_till_page(Nr - 1, Qs),
    PrevPageEnd = lists:dropwhile(fun(B) -> not is_page_end(B) end, PrevPageStart),
    case eval_page_jumps({PrevPageEnd, Nr}, Answers, Context) of
        stop -> stop;
        submit -> submit;
        {error, _} = Error -> Error;
        {NextPageStart, NextNr} ->
            NextPage = takepage(NextPageStart),
            {NextPage, NextNr}
    end.


eval_page_jumps(stop, _Answers, _Context) ->
    stop;
eval_page_jumps(submit, _Answers, _Context) ->
    submit;
eval_page_jumps({[], _Nr}, _Answers, _Context) ->
    submit;
eval_page_jumps({[Q|L], Nr} = QsNr, Answers, Context) ->
    case is_page_end(Q) of
        true ->
            case test(Q, Answers, Context) of
                ok ->
                    eval_page_jumps({L, Nr}, Answers, Context);
                {jump, Name} ->
                    % Go to question 'name', count pagebreaks in between for the new page nr
                    % Only allow jumping forward to prevent endless loops.
                    case fetch_question_name(L, z_convert:to_binary(Name), Nr, in_pagebreak) of
                        stop -> stop;
                        submit -> submit;
                        {[], _Nr} -> {error, {not_found, Name}};
                        NextQsNr ->
                            eval_page_jumps(NextQsNr, Answers, Context)
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        false ->
            QsNr
    end.

test(Q, Answers, Context) ->
    case maps:get(<<"type">>, Q, undefined) of
        <<"survey_stop">> ->
            ok;
        <<"survey_page_options">> ->
            ok;
        <<"survey_page_break">> ->
            survey_q_page_break:test(Q, Answers, Context);
        <<"survey_button">> ->
            % Assume button
            Name = maps:get(<<"name">>, Q, undefined),
            case proplists:get_value(Name, Answers) of
                <<"yes">> ->
                    Target = maps:get(<<"target">>, Q, undefined),
                    case z_utils:is_empty(Target) of
                        true -> ok;
                        false -> {jump, Target}
                    end;
                _ ->
                    ok
            end
    end.

fetch_question_name(_, <<"stop">>, _Nr, _State) ->
    stop;
fetch_question_name(_, <<"submit">>, _Nr, _State) ->
    submit;
fetch_question_name([], _Name, Nr, _State) ->
    % Page not found - maybe show error/warning here
    {[], Nr};
fetch_question_name([Q|Qs] = QQs, Name, Nr, State) ->
    case maps:get(<<"name">>, Q, undefined) of
        QName when QName =:= Name ->
            {QQs, Nr};
        _QName ->
            case is_page_end(Q) of
                true ->
                    % Keep counting page numbers
                    Nr1 = case State of
                        in_q -> Nr + 1;
                        in_pagebreak -> Nr
                    end,
                    fetch_question_name(Qs, Name, Nr1, in_pagebreak);
                false ->
                    fetch_question_name(Qs, Name, Nr, in_q)
            end
    end.

%% @doc Fetch the Nth page. Multiple page breaks in a row count as a single page break.
%% Returns the position after the page jumps of the previous page.
drop_till_page(Nr, []) ->
    {[], Nr};
drop_till_page(Nr, L) ->
    L1 = lists:filter(
        fun
            (#{ <<"name">> := <<"survey_feedback">> }) -> false;
            (_) -> true
         end, L),
    drop_till_page(1, Nr, L1).

drop_till_page(_, Nr, []) ->
    {[], Nr};
drop_till_page(N, Nr, L) when N >= Nr ->
    {L, N};
drop_till_page(N, Nr, L) when N =:= Nr - 1 ->
    L1 = lists:dropwhile(fun(B) -> not is_page_end(B) end, L),
    L2 = lists:dropwhile(fun(B) -> is_page_end(B) end, L1),
    {L2, Nr};
drop_till_page(N, Nr, [B|Bs]) when N < Nr ->
    case is_page_end(B) of
        true ->
            L1 = lists:dropwhile(fun is_page_end/1, Bs),
            drop_till_page(N+1, Nr, L1);
        false ->
            drop_till_page(N, Nr, Bs)
    end;
drop_till_page(N, Nr, [_|Bs]) ->
    drop_till_page(N, Nr, Bs).


takepage(L) ->
    takepage(L, []).

takepage([], Acc) ->
    lists:reverse(Acc);
takepage([Q|L], Acc) ->
    case is_page_end(Q) of
        true ->
            % Always add the page-ends to the list of questions, so that
            % the template can decide to show a Next button or not.
            Ends = lists:takewhile(fun is_page_end/1, L),
            lists:reverse([ Q | Acc ], Ends);
        _ ->
            case maps:get(<<"name">>, Q, undefined) of
                <<"survey_feedback">> -> takepage(L, Acc);
                _ -> takepage(L, [Q|Acc])
            end
    end.

is_page_end(#{ <<"type">> := <<"survey_page_break">> }) -> true;
is_page_end(#{ <<"type">> := <<"survey_stop">> }) -> true;
is_page_end(#{ <<"type">> := <<"survey_page_options">> }) -> true;
is_page_end(_) -> false.


%% @doc Collect all answers per question, save to the database. External entry point
%% in use by some websites. Keep this for backwards compatibility.
-spec do_submit(m_rsc:resource_id(), Questions, Answers, z:context()) ->
          ok
        | {ok, z:context()}
        | {error, term()}
        when Questions :: list(map()),
             Answers :: list().
do_submit(SurveyId, Questions, Answers, Context) ->
    do_submit(SurveyId, Questions, Answers, undefined, [], Context).


%% @todo Check if we are missing any answers
-spec do_submit(m_rsc:resource_id(), Questions, Answers, Editing, SubmitArgs, Context) ->
          ok
        | {ok, ContextOrRender}
        | {error, term()}
    when Questions :: list(map()),
         Answers :: list(),
         Editing :: undefined | {editing, AnswerId, Actions},
         AnswerId :: integer(),
         Actions :: list() | tuple() | undefined,
         SubmitArgs :: proplists:proplist(),
         Context :: z:context(),
         ContextOrRender :: z:context() | #render{}.
do_submit(SurveyId, Questions, Answers, Editing, SubmitArgs, Context) ->
    SurveySessionNonce = proplists:get_value(survey_session_nonce, SubmitArgs),
    case register_nonce(SurveySessionNonce) of
        ok ->
            case do_submit_1(SurveyId, Questions, Answers, Editing, SubmitArgs, Context) of
                ok -> ok;
                {ok, #context{}} = OK -> OK;
                {ok, #render{}} = OK -> OK;
                {error, full} ->
                    unregister_nonce(SurveySessionNonce),
                    Context1 = z_render:wire(
                            {alert, [
                                {title, ?__("Sorry", Context)},
                                {text, ?__(
                                    "Sorry, the maximum number of submissions has been reached. You can no longer submit this form.",
                                    Context)}
                            ]}, Context),
                    {ok, Context1};
                {error, _} = Error ->
                    unregister_nonce(SurveySessionNonce),
                    Error
            end;
        {error, duplicate} ->
            Context1 = z_render:wire(
                    {alert, [
                        {title, ?__("Already submitted", Context)},
                        {text, ?__(
                            "Sorry, you already submitted your answers. Reload the page if "
                            "you want to fill in the form again.", Context)}
                    ]}, Context),
            {ok, Context1};
        {error, overload} ->
            Context1 = z_render:wire(
                    {alert, [
                        {title, ?__("Sorry", Context)},
                        {text, ?__("We are experiencing an overload, please try again in 10 minutes.", Context)}
                    ]}, Context),
            {ok, Context1};
        {error, _} ->
            % Could be expired or an invalid nonce key
            Context1 = z_render:wire(
                    {alert, [
                        {title, ?__("Sorry", Context)},
                        {text, ?__("Your session was expired, please restart and try again.", Context)}
                    ]}, Context),
            {ok, Context1}
    end.

do_submit_1(SurveyId, Questions, Answers, undefined, SubmitArgs, Context) ->
    {FoundAnswers, Missing} = collect_answers(SurveyId, Questions, Answers, Context),
    case z_notifier:first(
        #survey_submit{
            id = SurveyId,
            handler = m_rsc:p_no_acl(SurveyId, <<"survey_handler">>, Context),
            answers = FoundAnswers,
            missing = Missing,
            answers_raw = Answers,
            submit_args = SubmitArgs
        },
        Context)
    of
        undefined ->
            case save_submit(SurveyId, FoundAnswers, Answers, Context) of
                {ok, _} -> ok;
                {error, _} = Error -> Error
            end;
        ok ->
            m_survey_saved:delete_saved(SurveyId, Context),
            maybe_mail(SurveyId, Answers, undefined, false, Context),
            ok;
        {save, #context{} = SaveContext} ->
            %% Use the passed context to save the answers.
            case save_submit(SurveyId, FoundAnswers, Answers, SaveContext) of
                {ok, _} -> {ok, SaveContext};
                {error, _} = Error -> Error
            end;
        {save, #render{} = Render} ->
            case save_submit(SurveyId, FoundAnswers, Answers, Context) of
                {ok, _} -> {ok, Render};
                {error, _} = Error -> Error
            end;
        {ok, _ContextOrRender} = Handled ->
            m_survey_saved:delete_saved(SurveyId, Context),
            Handled;
        {error, _Reason} = Error ->
            Error
    end;
do_submit_1(SurveyId, Questions, Answers, {editing, AnswerId, _Actions}, _SubmitArgs, Context) ->
    % Save the modified survey results
    case z_acl:is_allowed(update_result, #acl_survey{id=SurveyId, answer_id=AnswerId}, Context) of
        true ->
            {FoundAnswers, _Missing} = collect_answers(SurveyId, Questions, Answers, Context),
            StorageAnswers = survey_answers_to_storage(FoundAnswers),
            m_survey:replace_survey_submission(SurveyId, AnswerId, StorageAnswers, Context),
            case z_context:get_q(<<"submit-email">>, Context) of
                undefined ->
                    ok;
                _SomeValue ->
                    maybe_mail(SurveyId, Answers, AnswerId, true, Context)
            end,
            ok;
        false ->
            {ok, z_render:growl(?__("You are not allowed to change these results.", Context), Context)}
    end.

%% @doc Save the form in the submit. Can be called from survey_submit observers if they
%% need the answer id.
-spec save_submit(SurveySubmit, Context) -> {ok, AnswerId} | {error, Reason} when
    SurveySubmit :: #survey_submit{},
    Context :: z:context(),
    AnswerId :: integer(),
    Reason :: full | term().
save_submit(#survey_submit{
        id = SurveyId,
        answers = FoundAnswers,
        answers_raw = Answers
    }, Context) ->
    save_submit(SurveyId, FoundAnswers, Answers, Context).

-spec save_submit(SurveyId, FoundAnswers, Answers, Context) -> {ok, AnswerId} | {error, Reason} when
    SurveyId :: m_rsc:resource_id(),
    FoundAnswers :: list(),
    Answers :: list(),
    Context :: z:context(),
    AnswerId :: integer(),
    Reason :: full | term().
save_submit(SurveyId, FoundAnswers, Answers, Context) ->
    StorageAnswers = survey_answers_to_storage(FoundAnswers),
    case m_survey:insert_survey_submission(SurveyId, StorageAnswers, Context) of
        {ok, ResultId} ->
            m_survey_saved:delete_saved(SurveyId, Context),
            maybe_mail(SurveyId, Answers, ResultId, false, Context),
            {ok, ResultId};
        {error, _} = Error ->
            Error
    end.

maybe_mail(SurveyId, Answers, ResultId, IsEditing, Context) ->
    case IsEditing orelse probably_email(SurveyId, Context) of
        true ->
            PrepAnswers = survey_answer_prep:readable(SurveyId, Answers, Context),
            Attachments = uploads(Context),
            SurveyResult = case ResultId of
                undefined -> undefined;
                _ -> m_survey:single_result(SurveyId, ResultId, Context)
            end,
            mail_respondent(SurveyId, Answers, ResultId, PrepAnswers, SurveyResult, IsEditing, Context),
            mail_result(SurveyId, PrepAnswers, SurveyResult, Attachments, Context);
        false ->
            nop
    end.

uploads(Context) ->
    Qs = z_context:get_q_all_noz(Context),
    [ Upload || {_, #upload{} = Upload} <- Qs ].

probably_email(SurveyId, Context) ->
    not z_utils:is_empty(m_rsc:p_no_acl(SurveyId, survey_email, Context))
    orelse z_convert:to_bool(m_rsc:p_no_acl(SurveyId, survey_email_respondent, Context)).

%% @doc mail the survey result to an e-mail address
mail_result(SurveyId, PrepAnswers, SurveyResult, Attachments, Context) ->
    case m_survey:survey_emails(SurveyId, Context) of
        [] -> skip;
        Es ->
            lists:foreach(
                fun(E) ->
                    Vars = [
                        {is_result_email, true},
                        {id, SurveyId},
                        {answers, PrepAnswers},
                        {result, SurveyResult}
                    ],
                    EmailRec = #email{
                        to=E,
                        html_tpl="email_survey_result.tpl",
                        vars=Vars,
                        attachments=Attachments
                    },
                    z_email:send(EmailRec, Context)
                end,
                Es)
    end.

mail_respondent(SurveyId, Answers, ResultId, PrepAnswers, SurveyResult, IsEditing, Context) ->
    case IsEditing orelse z_convert:to_bool(m_rsc:p_no_acl(SurveyId, survey_email_respondent, Context)) of
        true ->
            EmailUser = case IsEditing of
                false ->
                    m_rsc:p_no_acl(z_acl:user(Context), email_raw, Context);
                true ->
                    AnsUserId = m_survey:answer_user(ResultId, Context),
                    m_rsc:p_no_acl(AnsUserId, email_raw, Context)
            end,
            case find_email_respondent(Answers, EmailUser) of
                <<>> ->
                    skip;
                undefined ->
                    skip;
                Email ->
                    Vars = [
                        {id, SurveyId},
                        {answers, PrepAnswers},
                        {result, SurveyResult}
                    ],
                    z_email:send_render(Email, "email_survey_result.tpl", Vars, Context),
                    ok
            end;
        false ->
            skip
    end.

find_email_respondent([], Default) ->
    Default;
find_email_respondent([{<<"email">>, Ans}|As], Default) ->
    Ans1 = z_string:trim(Ans),
    case z_utils:is_empty(Ans1) of
        true -> find_email_respondent(As, Default);
        false -> Ans1
    end;
find_email_respondent([_Ans|As], Default) ->
    find_email_respondent(As, Default).


%% @doc Collect all answers, report any missing answers.
-spec collect_answers(SurveyId, Qs, Answers, Context) -> {AnswerList, MissingNames} when
    SurveyId :: m_rsc:resource_id(),
    Qs :: [ map() ],
    Answers :: list(),
    Context :: z:context(),
    AnswerList :: list(),
    MissingNames :: [ binary() ].
collect_answers(SurveyId, Qs, Answers, Context) ->
    collect_answers(SurveyId, Qs, Answers, [], [], Context).


collect_answers(_SurveyId, [], _Answers, FoundAnswers, Missing, _Context) ->
    {FoundAnswers, Missing};
collect_answers(SurveyId, [Q|Qs], Answers, FoundAnswers, Missing, Context) ->
    case maps:get(<<"type">>, Q, undefined) of
        <<"survey_", _/binary>> = Type ->
            Module = module_name(Type),
            QName = maps:get(<<"name">>, Q, undefined),
            case Module:answer(SurveyId, Q, Answers, Context) of
                {ok, none} ->
                    collect_answers(SurveyId, Qs, Answers, FoundAnswers, Missing, Context);
                {ok, AnswerList} ->
                    collect_answers(SurveyId, Qs, Answers, [{QName, AnswerList}|FoundAnswers], Missing, Context);
                {error, missing} ->
                    case z_convert:to_bool(maps:get(<<"is_required">>, Q, false)) of
                        true ->
                            collect_answers(SurveyId, Qs, Answers, FoundAnswers, [QName|Missing], Context);
                        false ->
                            collect_answers(SurveyId, Qs, Answers, FoundAnswers, Missing, Context)
                    end
            end;
        _ ->
            collect_answers(SurveyId, Qs, Answers, FoundAnswers, Missing, Context)
    end.

survey_answers_to_storage(AnsPerBlock) ->
    lists:flatten(
        lists:map(
            fun({BlockName, Ans}) ->
                [
                    {Name, [
                        {block, BlockName},
                        {answer, Vs}
                    ]}
                    ||
                    {Name, Vs} <- Ans
                ]
            end,
            AnsPerBlock)).


module_name(A) when is_atom(A) ->
    module_name(atom_to_binary(A, utf8));
module_name(<<"survey_", Type/binary>>) ->
    %% @todo first check if module exists before making atom
    list_to_atom("survey_q_"++z_convert:to_list(Type));
module_name(_) ->
    undefined.

