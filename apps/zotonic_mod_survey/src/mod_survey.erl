%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2012 Marc Worrell
%% @doc Survey module.  Define surveys and let people fill them in.

%% Copyright 2010-2012 Marc Worrell
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
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Survey").
-mod_description("Create and publish questionnaires.").
-mod_prio(400).
-mod_schema(4).
-mod_depends([admin]).
-mod_provides([survey, poll]).

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

    get_page/3,

    do_submit/4,
    collect_answers/3,
    render_next_page/7,
    go_button_target/4,
    module_name/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include("survey.hrl").


%% @doc Schema for mod_survey lives in separate module
manage_schema(What, Context) ->
    survey_schema:manage_schema(What, Context).

event(#postback{message={survey_start, Args}}, Context) ->
    {id, SurveyId} = proplists:lookup(id, Args),
    AnswerId = z_convert:to_integer(proplists:get_value(answer_id, Args)),
    case is_integer(AnswerId) andalso z_acl:rsc_editable(SurveyId, Context) of
        true ->
            Answers = scomp_survey_poll:single_result(SurveyId, AnswerId, Context),
            Editing = {editing, AnswerId, undefined},
            render_update(render_next_page(SurveyId, 1, exact, Answers, [], Editing, Context), Args, Context);
        false ->
            Answers = normalize_answers(proplists:get_value(answers, Args)),
            Editing = proplists:get_value(editing, Args),
            render_update(render_next_page(SurveyId, 1, exact, Answers, [], Editing, Context), Args, Context)
    end;

event(#submit{message={survey_next, Args}}, Context) ->
    {id, SurveyId} = proplists:lookup(id, Args),
    {page_nr, PageNr} = proplists:lookup(page_nr, Args),
    {answers, Answers} = proplists:lookup(answers, Args),
    {history, History} = proplists:lookup(history, Args),
    Editing = proplists:get_value(editing, Args),
    render_update(render_next_page(SurveyId, PageNr+1, forward, Answers, History, Editing, Context), Args, Context);

event(#postback{message={survey_back, Args}}, Context) ->
    {id, SurveyId} = proplists:lookup(id, Args),
    {answers, Answers} = proplists:lookup(answers, Args),
    {history, History} = proplists:lookup(history, Args),
    Editing = proplists:get_value(editing, Args),
    case History of
        [_,PageNr|History1] ->
            render_update(render_next_page(SurveyId, PageNr, exact, Answers, History1, Editing, Context), Args, Context);
        _History ->
            render_update(render_next_page(SurveyId, 0, exact, Answers, [], Editing, Context), Args, Context)
    end;

event(#postback{message={survey_remove_result_confirm, Args}}, Context) ->
    {id, SurveyId} = proplists:lookup(id, Args),
    {answer_id, _AnswerId} = proplists:lookup(answer_id, Args),
    case z_acl:rsc_editable(SurveyId, Context) of
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
    case z_acl:rsc_editable(SurveyId, Context) of
        true ->
            m_survey:delete_result(SurveyId, AnswerId, Context),
            Target = "survey-result-"++z_convert:to_list(AnswerId),
            z_render:wire([
                    {growl, [{text, ?__("Survey result deleted.", Context)}]},
                    {slide_fade_out, [{target, Target}]}
                ], Context);
        false ->
            z_render:growl(?__("You are not allowed to change these results.", Context), Context)
    end;

event(#postback{message={admin_show_emails, Args}}, Context) ->
    {id, SurveyId} = proplists:lookup(id, Args),
    case m_survey:is_allowed_results_download(SurveyId, Context) of
        true ->
            case m_survey:survey_results(SurveyId, true, Context) of
                [Headers|Data] ->
                    All = [lists:zip(Headers, Row) || {_Id,Row} <- Data],
                    z_render:dialog(?__("E-mail addresses", Context),
                                    "_dialog_survey_email_addresses.tpl",
                                    [{id, SurveyId}, {all, All}],
                                    Context);
                [] ->
                    z_render:dialog(?__("E-mail addresses", Context),
                                    "_dialog_survey_email_addresses.tpl",
                                    [{id, SurveyId}, {all, []}],
                                    Context)
            end;
        false ->
            Context
    end.


%% @doc Append the possible blocks for a survey's edit page.
observe_admin_edit_blocks(#admin_edit_blocks{id=Id}, Menu, Context) ->
    case m_rsc:is_a(Id, survey, Context) of
        true ->
            [
                {100, ?__("Survey Questions", Context), [
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
    case proplists:get_value(type, Q) of
        <<"survey_button">> -> true;
        <<"survey_", _/binary>> -> proplists:get_value(input_type, Q) =:= <<"submit">>;
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
            {Hs, Promps, Data} = m_survey:survey_results_prompts(Id, false, Context),
            Data1 = [ Row || {_Id, Row} <- Data ],
            {ok, Hs, [ Promps | Data1 ]};
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


get_page(Id, Nr, #context{} = Context) when is_integer(Nr) ->
    case m_rsc:p(Id, blocks, Context) of
        Qs when is_list(Qs) ->
            go_page(Nr, Qs, [], exact, Context);
        _ ->
            []
    end.


%%====================================================================
%% support functions
%%====================================================================


normalize_answers(undefined) -> [];
normalize_answers(L) -> lists:map(fun normalize_answer/1, L).

normalize_answer(A) when is_atom(A), is_binary(A) -> {z_convert:to_binary(A), <<"1">>};
normalize_answer({A, undefined}) -> {z_convert:to_binary(A), <<>>};
normalize_answer({A, true}) -> {z_convert:to_binary(A), <<"1">>};
normalize_answer({A, false}) -> {z_convert:to_binary(A), <<"0">>};
normalize_answer({A,V}) -> {z_convert:to_binary(A), z_convert:to_binary(V)};
normalize_answer([A,V]) -> normalize_answer({A,V}).


render_update(#context{} = RenderContext, _Args, _Context) ->
    RenderContext;
render_update(#render{} = Render, Args, Context) ->
    TargetId = proplists:get_value(element_id, Args, "survey-question"),
    z_render:update(TargetId, Render, Context).


%% @doc Fetch the next page from the survey, update the page view
-spec render_next_page(integer(), integer(), exact|forward, list(), list(), term()|undefined, #context{}) -> #render{} | #context{}.
render_next_page(Id, 0, _Direction, _Answers, _History, _Editing, Context) when is_integer(Id) ->
    z_render:wire({redirect, [{id, Id}]}, Context);
render_next_page(Id, PageNr, Direction, Answers, History, Editing, Context) when is_integer(Id) ->
    {As, Submitter} = get_args(Context),
    Answers1 = lists:foldl(fun({Arg,_Val}, Acc) -> proplists:delete(Arg, Acc) end, Answers, As),
    Answers2 = Answers1 ++ group_multiselect(As),
    case m_rsc:p(Id, blocks, Context) of
        Questions when is_list(Questions) ->

            Next = case Submitter of
                       undefined ->
                            go_page(PageNr, Questions, Answers2, Direction, Context);
                       _ButtonName ->
                            go_button_target(Submitter, Questions, Answers2, Context)
                   end,

            case Next of
                {L,NewPageNr} when is_list(L) ->
                    % A new list of questions, PageNr might be another than expected
                    Vars = [
                        {id, Id},
                        {q, As},
                        {page_nr, NewPageNr},
                        {questions, L},
                        {pages, count_pages(Questions)},
                        {answers, Answers2},
                        {history, [NewPageNr|History]},
                        {editing, Editing}
                    ],
                    #render{template="_survey_question_page.tpl", vars=Vars};

                {error, {not_found, Name} = Reason} ->
                    lager:error("Survey ~p, error ~p on page ~p", [Id, Reason, PageNr]),
                    z_render:growl_error("Error in survey, could not find page "++z_convert:to_list(Name), Context);

                {error, Reason} ->
                    lager:error("Survey ~p, error ~p on page ~p", [Id, Reason, PageNr]),
                    z_render:growl_error("Error evaluating submit.", Context);

                stop ->
                    render_next_page(Id, 0, Direction, Answers, History, Editing, Context);

                submit when Editing =:= undefined ->
                    %% That was the last page. Show a thank you and save the result.
                    case do_submit(Id, Questions, Answers2, Context) of
                        ok ->
                            case z_convert:to_bool(m_rsc:p(Id, survey_show_results, Context)) of
                                true ->
                                    #render{
                                        template="_survey_results.tpl",
                                        vars=[
                                            {id,Id}, {inline, true}, {history, History}, {q, As}
                                        ]
                                    };
                                false ->
                                    #render{
                                        template="_survey_end.tpl",
                                        vars=[
                                            {id,Id}, {history, History}, {q, As}
                                        ]
                                    }
                            end;
                        {ok, ContextOrRender} ->
                            ContextOrRender;
                        {error, _Reason} ->
                            #render{template="_survey_error.tpl", vars=[{id,Id}, {history,History}, {q, As}]}
                    end;

                submit ->
                    admin_edit_survey_result(Id, Questions, Answers2, Editing, Context)
            end;
        _NoBlocks ->
            % No survey defined, show an error page.
            #render{template="_survey_error.tpl", vars=[{id,Id}, {q, As}]}
    end.

    get_args(Context) ->
        Args = [ {z_convert:to_binary(K), z_convert:to_binary(V)}
                || {K,V} <- z_context:get_q_all_noz(Context), not is_tuple(V)
               ],
        Submitter = proplists:get_value(<<"z_submitter">>, Args),
        Buttons = proplists:get_all_values(<<"survey$button">>, Args),
        WithButtons = lists:foldl(fun(B, Acc) ->
                                      case B of
                                          Submitter -> [{B,<<"yes">>} | proplists:delete(B, Acc) ];
                                          _ -> [{B, <<"no">>} | Acc]
                                      end
                                  end,
                                  Args,
                                  Buttons),
        {proplists:delete(<<"z_submitter">>, proplists:delete(<<"survey$button">>, WithButtons)),
         case lists:member(Submitter,Buttons) of
             true -> Submitter;
             false -> undefined
         end}.


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
        [Button|_] = lists:dropwhile(fun(B) -> proplists:get_value(name, B) =/= Submitter end, Questions),
        TargetName = proplists:get_value(target, Button),
        case eval_page_jumps(fetch_question_name(Questions, TargetName, 1, in_q), Answers, Context) of
            {error, _} = Error -> Error;
            stop -> stop;
            submit -> submit;
            {L1, Nr1} ->
                L2 = takepage(L1),
                {L2,Nr1}
        end.

    go_page(Nr, Qs, _Answers, exact, _Context) ->
        case fetch_page(Nr, Qs) of
            stop -> stop;
            submit -> submit;
            {[], _Nr} -> submit;
            {L,Nr1} ->
                L1 = lists:dropwhile(fun is_page_end/1, L),
                L2 = takepage(L1),
                {L2,Nr1}
        end;
    go_page(Nr, Qs, Answers, forward, Context) ->
        case eval_page_jumps(fetch_page(Nr, Qs), Answers, Context) of
            {error, _} = Error -> Error;
            stop -> stop;
            submit -> submit;
            {L1, Nr1} ->
                L2 = takepage(L1),
                {L2,Nr1}
        end.



    eval_page_jumps(submit, _Answers, _Context) ->
        submit;
    eval_page_jumps(stop, _Answers, _Context) ->
        stop;
    eval_page_jumps({[], _Nr}, _Answers, _Context) ->
        submit;
    eval_page_jumps({[Q|L],Nr} = QsNr, Answers, Context) ->
        case is_page_end(Q) of
            true ->
                case test(Q, Answers, Context) of
                    ok ->
                        eval_page_jumps({L,Nr}, Answers, Context);
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
        case proplists:get_value(type, Q) of
            <<"survey_stop">> ->
                ok;
            <<"survey_page_break">> ->
                survey_q_page_break:test(Q, Answers, Context);
            <<"survey_button">> ->
                % Assume button
                Name = proplists:get_value(name, Q),
                case proplists:get_value(Name, Answers) of
                    <<"yes">> ->
                        Target = proplists:get_value(target, Q),
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
        % Page not found - should show error/warning here
        {[], Nr};
    fetch_question_name([Q|Qs] = QQs, Name, Nr, State) ->
        case proplists:get_value(name, Q) of
            Name ->
                {QQs, Nr};
            _Other ->
                case is_page_end(Q) of
                    true ->
                        case State of
                            in_q -> fetch_question_name(Qs, Name, Nr+1, in_pagebreak);
                            in_pagebreak -> fetch_question_name(Qs, Name, Nr, in_pagebreak)
                        end;
                    false ->
                        fetch_question_name(Qs, Name, Nr, in_q)
                end
        end.


%% @doc Fetch the Nth page. Multiple page breaks in a row count as a single page break.
%%      Returns the position at the page breaks before the page, so that eventual jump
%%      expressions can be evaluated.
fetch_page(Nr, []) ->
    {[], Nr};
fetch_page(Nr, L) ->
    fetch_page(1, Nr, L).

fetch_page(_, Nr, []) ->
    {[], Nr};
fetch_page(N, Nr, L) when N >= Nr ->
    {L, N};
fetch_page(N, Nr, L) when N == Nr - 1 ->
    L1 = lists:dropwhile(fun(B) -> not is_page_end(B) end, L),
    {L1, Nr};
fetch_page(N, Nr, [B|Bs]) when N < Nr ->
    case is_page_end(B) of
        true ->
            L1 = lists:dropwhile(fun is_page_end/1, Bs),
            fetch_page(N+1, Nr, L1);
        false ->
            fetch_page(N, Nr, Bs)
    end;
fetch_page(N, Nr, [_|Bs]) ->
    fetch_page(N, Nr, Bs).


takepage(L) ->
    takepage(L, []).

    takepage([], Acc) ->
        lists:reverse(Acc);
    takepage([Q|L], Acc) ->
        case proplists:get_value(type, Q) of
            <<"survey_page_break">> -> lists:reverse(Acc);
            <<"survey_stop">> -> lists:reverse([Q|Acc]);
            _ ->
                case proplists:get_value(name, Q) of
                    <<"survey_feedback">> ->  takepage(L, Acc);
                    _ -> takepage(L, [Q|Acc])
                end
        end.

is_page_end(Block) ->
    case proplists:get_value(type, Block) of
        <<"survey_page_break">> -> true;
        <<"survey_stop">> -> true;
        _ -> false
    end.


%% @doc Collect all answers per question, save to the database.
%% @todo Check if we are missing any answers
do_submit(SurveyId, Questions, Answers, Context) ->
    {FoundAnswers, Missing} = collect_answers(Questions, Answers, Context),
    case z_notifier:first(#survey_submit{id=SurveyId, handler=m_rsc:p(SurveyId, survey_handler, Context),
                                         answers=FoundAnswers, missing=Missing, answers_raw=Answers},
                          Context)
    of
        undefined ->
            StorageAnswers = survey_answers_to_storage(FoundAnswers),
            {ok, ResultId} = insert_survey_submission(SurveyId, StorageAnswers, Context),
            maybe_mail(SurveyId, Answers, ResultId, Context),
            ok;
        ok ->
            maybe_mail(SurveyId, Answers, undefined, Context),
            ok;
        {ok, _Context1} = Handled ->
            maybe_mail(SurveyId, Answers, undefined, Context),
            Handled;
        {error, _Reason} = Error ->
            Error
    end.

insert_survey_submission(SurveyId, StorageAnswers, Context) ->
    {UserId, PersistentId} = case z_acl:user(Context) of
                                undefined -> {undefined, persistent_id(Context)};
                                UId -> {UId, undefined}
                             end,
    m_survey:insert_survey_submission(SurveyId, UserId, PersistentId, StorageAnswers, Context).

% persistent_id(#context{session_id = undefined}) -> undefined;
persistent_id(Context) -> z_context:persistent_id(Context).

maybe_mail(SurveyId, Answers, ResultId, Context) ->
    case probably_email(SurveyId, Context) of
        true ->
            PrepAnswers = survey_answer_prep:readable(SurveyId, Answers, Context),
            Attachments = uploads(Context),
            SurveyResult = case ResultId of
                undefined -> undefined;
                _ -> m_survey:single_result(SurveyId, ResultId, Context)
            end,
            mail_respondent(SurveyId, Answers, PrepAnswers, SurveyResult, Context),
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
    case m_rsc:p_no_acl(SurveyId, survey_email, Context) of
        undefined -> skip;
        <<>> -> skip;
        Email ->
            Es = z_email_utils:extract_emails(Email),
            ContextLang = z_context:set_language(z_language:default_language(Context), Context),
            lists:foreach(
                fun(E) ->
                    case z_email_utils:is_email(E) of
                        true ->
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
                            z_email:send(EmailRec, ContextLang);
                        false ->
                            ok
                    end
                end,
                Es)
    end.

mail_respondent(SurveyId, Answers, PrepAnswers, SurveyResult, Context) ->
    case z_convert:to_bool(m_rsc:p_no_acl(SurveyId, survey_email_respondent, Context)) of
        true ->
            case find_email_respondent(Answers, Context) of
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

find_email_respondent([], Context) ->
    m_rsc:p_no_acl(z_acl:user(Context), email, Context);
find_email_respondent([{<<"email">>, Ans}|As], Context) ->
    Ans1 = z_string:trim(Ans),
    case z_utils:is_empty(Ans1) of
        true -> find_email_respondent(As, Context);
        false -> Ans1
    end;
find_email_respondent([_Ans|As], Context) ->
    find_email_respondent(As, Context).


%% @doc Collect all answers, report any missing answers.
%% @spec collect_answers(list(), proplist(), Context) -> {AnswerList, MissingNames}
collect_answers(Qs, Answers, Context) ->
    collect_answers(Qs, Answers, [], [], Context).


collect_answers([], _Answers, FoundAnswers, Missing, _Context) ->
    {FoundAnswers, Missing};
collect_answers([Q|Qs], Answers, FoundAnswers, Missing, Context) ->
    case proplists:get_value(type, Q) of
        <<"survey_", _/binary>> = Type ->
            Module = module_name(Type),
            QName = proplists:get_value(name, Q),
            case Module:answer(Q, Answers, Context) of
                {ok, none} ->
                    collect_answers(Qs, Answers, FoundAnswers, Missing, Context);
                {ok, AnswerList} ->
                    collect_answers(Qs, Answers, [{QName, AnswerList}|FoundAnswers], Missing, Context);
                {error, missing} ->
                    case z_convert:to_bool(proplists:get_value(is_required, Q)) of
                        true ->
                            collect_answers(Qs, Answers, FoundAnswers, [QName|Missing], Context);
                        false ->
                            collect_answers(Qs, Answers, FoundAnswers, Missing, Context)
                    end
            end;
        _ ->
            collect_answers(Qs, Answers, FoundAnswers, Missing, Context)
    end.

%% @doc Save the modified survey results
admin_edit_survey_result(SurveyId, Questions, Answers, {editing, AnswerId, Actions}, Context) ->
    case z_acl:rsc_editable(SurveyId, Context)
        orelse (
            z_convert:to_integer(m_rsc:p(SurveyId, survey_multiple, Context)) =:= 2
            andalso is_answer_user(AnswerId, Context))
    of        true ->
            {FoundAnswers, _Missing} = collect_answers(Questions, Answers, Context),
            StorageAnswers = survey_answers_to_storage(FoundAnswers),
            m_survey:replace_survey_submission(SurveyId, AnswerId, StorageAnswers, Context),
            case Actions of
                [] ->
                    Context1 = z_render:dialog_close(Context),
                    z_render:update(
                            "survey-results",
                            #render{
                                template="_admin_survey_editor_results.tpl",
                                vars=[
                                    {id, SurveyId}
                                ]
                            },
                            Context1);
                _ ->
                    z_render:wire(Actions, Context)
            end;
        false ->
            z_render:growl(?__("You are not allowed to change these results.", Context), Context)
    end.

is_answer_user({user, UserId}, Context) when is_integer(UserId) ->
    UserId =:= z_acl:user(Context);
is_answer_user(AnswerId, Context) when is_integer(AnswerId) ->
    case z_acl:user(Context) of
        UserId when is_integer(UserId) ->
            m_survey:is_answer_user(AnswerId, UserId, Context);
        _ -> false
    end;
is_answer_user(_, _Context) ->
    false.

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
    module_name(list_to_binary(atom_to_list(A)));
module_name(<<"survey_", Type/binary>>) -> list_to_atom("survey_q_"++z_convert:to_list(Type));
module_name(_) -> undefined.
