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
-mod_prio(800).
-mod_schema(2).
-mod_depends([admin]).
-mod_provides([survey, poll]).

%% interface functions
-export([
    manage_schema/2,
    event/2,
    observe_admin_edit_blocks/3,

    render_next_page/6,
    go_button_target/4,
    module_name/1
]).

-include_lib("zotonic.hrl").
-include("survey.hrl").


%% @doc Schema for mod_survey lives in separate module
manage_schema(What, Context) ->
    mod_survey_schema:manage_schema(What, Context).

event(#postback{message={survey_start, Args}}, Context) ->
    {id, SurveyId} = proplists:lookup(id, Args),
    render_update(render_next_page(SurveyId, 1, exact, [], [], Context), Args, Context);

event(#submit{message={survey_next, Args}}, Context) ->
    {id, SurveyId} = proplists:lookup(id, Args),
    {page_nr, PageNr} = proplists:lookup(page_nr, Args),
    {answers, Answers} = proplists:lookup(answers, Args),
    {history, History} = proplists:lookup(history, Args),
    render_update(render_next_page(SurveyId, PageNr+1, forward, Answers, History, Context), Args, Context);

event(#postback{message={survey_back, Args}}, Context) ->
    {id, SurveyId} = proplists:lookup(id, Args),
    {answers, Answers} = proplists:lookup(answers, Args),
    {history, History} = proplists:lookup(history, Args),
    case History of
        [_,PageNr|History1] ->
            render_update(render_next_page(SurveyId, PageNr, exact, Answers, History1, Context), Args, Context);
        _History ->
            render_update(render_next_page(SurveyId, 0, exact, Answers, [], Context), Args, Context)
    end;

event(#postback{message={survey_remove_result, [{id, SurveyId}, {persistent_id, PersistentId}, {user_id, UserId}]}}, Context) ->
    m_survey:delete_result(SurveyId, UserId, PersistentId, Context),
    Target = "survey-result-"++z_convert:to_list(UserId)++"-"++z_convert:to_list(PersistentId),
    z_render:wire([ 
            {growl, [{text, ?__("Survey result deleted.", Context)}]},
            {slide_fade_out, [{target, Target}]}
        ], Context);

event(#postback{message={admin_show_emails, [{id, SurveyId}]}}, Context) ->
    [Headers0|Data] = m_survey:survey_results(SurveyId, Context),
    Headers = lists:map(fun(X) -> list_to_atom(binary_to_list(X)) end, Headers0),
    All = [lists:zip(Headers, Row) || Row <- Data],
    z_render:dialog(?__("E-mail addresses", Context),
                    "_dialog_survey_email_addresses.tpl",
                    [{id, SurveyId}, {all, All}],
                    Context).

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
                    {survey_matching, ?__("Matching", Context)},
                    {survey_narrative, ?__("Narrative", Context)},
                    {survey_short_answer, ?__("Short answer", Context)},
                    {survey_long_answer, ?__("Long answer", Context)},
                    {survey_country, ?__("Country select", Context)},
                    {survey_button, ?__("Button", Context)},
                    {survey_page_break, ?__("Page break", Context)}
                ]}
                | Menu
            ];
        false ->
            Menu
    end.


%%====================================================================
%% support functions
%%====================================================================

render_update(#context{} = RenderContext, _Args, _Context) ->
    RenderContext;
render_update(#render{} = Render, Args, Context) ->
    TargetId = proplists:get_value(element_id, Args, "survey-question"),
    z_render:update(TargetId, Render, Context).


%% @doc Fetch the next page from the survey, update the page view
-spec render_next_page(integer(), integer(), exact|forward, list(), list(), #context{}) -> #render{} | #context{}.
render_next_page(Id, 0, _Direction, _Answers, _History, Context) ->
    z_render:wire({redirect, [{id, Id}]}, Context);
render_next_page(Id, PageNr, Direction, Answers, History, Context) ->
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
                    Vars = [ {id, Id},
                             {q, As},
                             {page_nr, NewPageNr},
                             {questions, L},
                             {pages, count_pages(Questions)},
                             {answers, Answers2},
                             {history, [NewPageNr|History]}],
                    #render{template="_survey_question_page.tpl", vars=Vars};

                {error, {not_found, Name} = Reason} ->
                    lager:error("Survey ~p, error ~p on page ~p", [Id, Reason, PageNr]),
                    z_render:growl_error("Error in survey, could not find page "++z_convert:to_list(Name), Context);

                {error, Reason} ->
                    lager:error("Survey ~p, error ~p on page ~p", [Id, Reason, PageNr]),
                    z_render:growl_error("Error evaluating submit.", Context);

                stop ->
                    render_next_page(Id, 0, Direction, Answers, History, Context);

                submit ->
                    case z_session:get(mod_survey_editing, Context) of
                        {U, P} -> 
                            admin_edit_survey_result(Id, U, P, Questions, Answers2, Context);
                        _ ->
                            %% That was the last page. Show a thank you and save the result.
                            case do_submit(Id, Questions, Answers2, Context) of
                                ok ->
                                    mail_result(Id, Answers2, Context),
                                    case z_convert:to_bool(m_rsc:p(Id, survey_show_results, Context)) of
                                        true ->
                                            #render{template="_survey_results.tpl", vars=[{id,Id}, {inline, true}, {history,History}, {q, As}]};
                                        false ->
                                            #render{template="_survey_end.tpl", vars=[{id,Id}, {history,History}, {q, As}]}
                                    end;
                                {ok, ContextOrRender} ->
                                    mail_result(Id, Answers2, Context),
                                    ContextOrRender;
                                {error, _Reason} ->
                                    #render{template="_survey_error.tpl", vars=[{id,Id}, {history,History}, {q, As}]}
                            end
                    end
            end;
        _NoBlocks ->
            % No survey defined, show an error page.
            #render{template="_survey_error.tpl", vars=[{id,Id}, {q, As}]}
    end.

    get_args(Context) ->
        Args = [ {z_convert:to_binary(K), z_convert:to_binary(V)} || {K,V} <- z_context:get_q_all_noz(Context) ],
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
        case is_page_break(Q) of
            true ->
                L1 = lists:dropwhile(fun is_page_break/1, L),
                count_pages(L1, N+1);
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
                L2 = lists:takewhile(fun(Q) -> not is_page_break(Q) end, L1),
                {L2,Nr1}
        end.

    go_page(Nr, Qs, _Answers, exact, _Context) ->
        case fetch_page(Nr, Qs) of
            stop -> stop;
            submit -> submit;
            {[], _Nr} -> submit;
            {L,Nr1} ->
                L1 = lists:dropwhile(fun is_page_break/1, L),
                L2 = lists:takewhile(fun(Q) -> not is_page_break(Q) end, L1),
                {L2,Nr1}
        end;
    go_page(Nr, Qs, Answers, forward, Context) ->
        case eval_page_jumps(fetch_page(Nr, Qs), Answers, Context) of
            {error, _} = Error -> Error;
            stop -> stop;
            submit -> submit;
            {L1, Nr1} ->
                L2 = lists:takewhile(fun(Q) -> not is_page_break(Q) end, L1),
                {L2,Nr1}
        end.



    eval_page_jumps({[], _Nr}, _Answers, _Context) ->
        submit;
    eval_page_jumps({[Q|L],Nr} = QsNr, Answers, Context) ->
        case is_page_break(Q) or is_button(Q) of
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
                            NextQsNr -> eval_page_jumps(NextQsNr, Answers, Context)
                        end;
                    {error, Reason} ->
                        {error, Reason}
                end;
            false ->
                QsNr
        end.

    test(Q, Answers, Context) ->
        case is_page_break(Q) of
            true ->
                survey_q_page_break:test(Q, Answers, Context);
            false ->
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
                case is_page_break(Q) of
                    true ->
                        case State of
                            in_q -> fetch_question_name(Qs, Name, Nr+1, in_pagebreak);
                            in_page_break -> fetch_question_name(Qs, Name, Nr, in_pagebreak)
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
        L1 = lists:dropwhile(fun(B) -> not is_page_break(B) end, L),
        {L1, Nr};
    fetch_page(N, Nr, [B|Bs]) when N < Nr ->
        case is_page_break(B) of
            true ->
                L1 = lists:dropwhile(fun is_page_break/1, Bs),
                fetch_page(N+1, Nr, L1);
            false ->
                fetch_page(N, Nr, Bs)
        end;
    fetch_page(N, Nr, [_|Bs]) ->
        fetch_page(N, Nr, Bs).


is_page_break(Block) ->
    proplists:get_value(type, Block) =:= <<"survey_page_break">>.

is_button(Block) ->
    proplists:get_value(type, Block) =:= <<"survey_button">>.


%% @doc Collect all answers per question, save to the database.
%% @todo Check if we are missing any answers
do_submit(SurveyId, Questions, Answers, Context) ->
    {FoundAnswers, Missing} = collect_answers(Questions, Answers, Context),
    case z_notifier:first(#survey_submit{id=SurveyId, answers=FoundAnswers, missing=Missing, answers_raw=Answers}, Context) of
        undefined ->
            m_survey:insert_survey_submission(SurveyId, FoundAnswers, Context),
            ok;
        {ok, _Context1} = Handled ->
            Handled;
        {error, _Reason} = Error ->
            Error
    end.


%% @doc mail the survey result to an e-mail address
mail_result(SurveyId, Answers, Context) ->
    case m_rsc:p_no_acl(SurveyId, survey_email, Context) of
        undefined ->
            skip;
        Email ->
            Vars = [
                {id, SurveyId},
                {answers, Answers}
            ],
            z_email:send_render(Email, "email_survey_result.tpl", Vars, Context),
            ok
    end.


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
admin_edit_survey_result(Id, U, P, Questions, Answers, Context) ->
    z_session:set(mod_survey_editing, undefined, Context),
    {FoundAnswers, _Missing} = collect_answers(Questions, Answers, Context),
    m_survey:insert_survey_submission(Id, U, P, FoundAnswers, Context),
    Context1 = z_render:dialog_close(Context),
    z_render:update("survey-results", z_template:render("_admin_survey_editor_results.tpl", [{id, Id}], Context), Context1).



module_name(<<"survey_", Type/binary>>) -> list_to_atom("survey_q_"++z_convert:to_list(Type));
module_name(_) -> undefined.
