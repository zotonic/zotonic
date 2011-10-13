%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2011 Marc Worrell
%% @doc Survey module.  Define surveys and let people fill them in.

%% Copyright 2010-2011 Marc Worrell
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
-mod_schema(1).

%% interface functions
-export([
	manage_schema/2,
    event/2,
    redraw_questions/2,
    new_question/1,
    delete_question/3,

    render_next_page/6,
    question_to_props/1,
    module_name/1
]).

-include_lib("zotonic.hrl").
-include("survey.hrl").


%% @doc Schema for mod_survey lives in separate module
manage_schema(What, Context) ->
    mod_survey_schema:manage_schema(What, Context).


%% @doc Handle drag/drop events from the survey admin
event({sort, Items, {dragdrop, {survey, [{id,Id}]}, _Delegate, "survey"}}, Context) ->
    event_sort(Id, Items, Context);

event({postback, {survey_start, Args}, _, _}, Context) ->
    {id, SurveyId} = proplists:lookup(id, Args),
    render_update(render_next_page(SurveyId, 1, exact, [], [], Context), Args, Context);

event({submit, {survey_next, Args}, _, _}, Context) ->
    {id, SurveyId} = proplists:lookup(id, Args),
    {page_nr, PageNr} = proplists:lookup(page_nr, Args),
    {answers, Answers} = proplists:lookup(answers, Args),
    {history, History} = proplists:lookup(history, Args),
    render_update(render_next_page(SurveyId, PageNr+1, forward, Answers, History, Context), Args, Context);

event({postback, {survey_back, Args}, _, _}, Context) ->
    {id, SurveyId} = proplists:lookup(id, Args),
    % {page_nr, PageNr} = proplists:lookup(page_nr, Args),
    {answers, Answers} = proplists:lookup(answers, Args),
    {history, History} = proplists:lookup(history, Args),
    case History of
        [_,PageNr|History1] ->
            render_update(render_next_page(SurveyId, PageNr, exact, Answers, History1, Context), Args, Context);
        _History ->
            render_update(render_next_page(SurveyId, 0, exact, Answers, [], Context), Args, Context)
    end;

event({postback, {survey_remove_result, [{id, SurveyId}, {persistent_id, PersistentId}, {user_id, UserId}]}, _, _}, Context) ->
    m_survey:delete_result(SurveyId, UserId, PersistentId, Context),
    Target = "survey-result-"++z_convert:to_list(UserId)++"-"++z_convert:to_list(PersistentId),
	z_render:wire([ {growl, [{text, ?__("Survey result deleted.", Context)}]},
					{slide_fade_out, [{target, Target}]}
				], Context).





%%====================================================================
%% support functions
%%====================================================================

render_update(Render, Args, Context) ->
    TargetId = proplists:get_value(element_id, Args, "survey-question"),
    z_render:update(TargetId, Render, Context).


%% @doc Handle the sort of a list.  First check if there is any new item added.
event_sort(Id, SortItems, Context) ->
    case has_new_q(SortItems) of
        true ->
            %% There is a new question added, redraw the list with the new item in edit state.
            {QuestionIds, NewQuestionId, NewQuestion} = items2id_new(SortItems),
            {ok, Id} = add_question(Id, QuestionIds, NewQuestionId, NewQuestion, Context),
            redraw_questions(Id, Context);
        false ->
            %% Order changed
            save_question_order(Id, items2id(SortItems), Context),
            Context
    end.

%% @doc Replace the new item in the item list with a new id, return new item and its id
items2id_new(Items) ->
    items2id_new(Items, []).

items2id_new([{dragdrop, {q, NewItemOpts}, _, _}|T], Acc) ->
    NewItemId = z_ids:identifier(10), 
    NewItem = new_question(proplists:get_value(type, NewItemOpts)),
    {lists:reverse(Acc, [NewItemId|items2id(T)]), NewItemId, NewItem};
items2id_new([{dragdrop, _, _, ItemId}|T], Acc) ->
    items2id_new(T, [ItemId|Acc]).

%% @doc Fetch all question ids from the sort list
items2id(Items) ->
    items2id(Items, []).
        
    items2id([], Acc) ->
        lists:reverse(Acc);
    items2id([{dragdrop, _, _, ItemId}|T], Acc) ->
        items2id(T, [ItemId|Acc]).


%% @doc Update the rsc with the new question and the new question order.
add_question(Id, QuestionIds, NewQuestionId, NewQuestion, Context) ->
    New = case m_rsc:p(Id, survey, Context) of
        undefined ->
            {survey, [NewQuestionId], [{NewQuestionId, NewQuestion}]};
        {survey, _SurveyIds, SurveyQuestions} ->
            {survey, QuestionIds, [{NewQuestionId, NewQuestion}|SurveyQuestions]} 
    end,
    m_rsc_update:update(Id, [{survey, New}], Context).


%% @doc Delete a question, redraw the question list.
%% @todo Make this more efficient by only removing the li with QuestionId.
delete_question(Id, QuestionId, Context) ->
    case m_rsc:p(Id, survey, Context) of
        undefined ->
            Context;
        {survey, SurveyIds, SurveyQuestions} ->
            Ids1 = lists:delete(QuestionId, SurveyIds),
            Questions1 = z_utils:prop_delete(QuestionId, SurveyQuestions),
            m_rsc:update(Id, [{survey, {survey, Ids1, Questions1}}], Context),
            redraw_questions(Id, Context)
    end.
    

%% @doc Update the rsc with the new question order.
save_question_order(Id, QuestionIds, Context) ->
    {survey, _SurveyIds, SurveyQuestions} = m_rsc:p(Id, survey, Context),
    m_rsc_update:update(Id, [{survey, {survey, QuestionIds, SurveyQuestions}}], Context).


%% @doc Check if the sort list contains a newly dropped question.
has_new_q([]) ->
    false;
has_new_q([{dragdrop, {q, _}, _, _}|_]) ->
    true;
has_new_q([_|T]) ->
    has_new_q(T).


%% @doc Generate the html for the survey editor in the admin, update the displayed survey.
redraw_questions(Id, Context) ->
    Html = z_template:render("_admin_survey_questions_edit.tpl", [{id, Id}], Context),
    Context1 = z_render:update("survey", Html, Context),
    Context1.


%% @doc Return the default state for each item type.
new_question(Type) ->
    Mod = module_name(Type),
    Mod:new().


%% @doc Fetch the next page from the survey, update the page view
render_next_page(Id, 0, _Direction, Answers, _History, Context) ->
    z_render:update("survey-question", 
                    #render{
                        template="_survey_start.tpl", 
                        vars=[{id,Id},{answers,Answers},{history,[]}]
                    },
                    Context);
render_next_page(Id, PageNr, Direction, Answers, History, Context) ->
    As = z_context:get_q_all_noz(Context),
    Answers1 = lists:foldl(fun({Arg,_Val}, Acc) -> proplists:delete(Arg, Acc) end, Answers, As),
    Answers2 = Answers1 ++ As,
    case m_rsc:p(Id, survey, Context) of
        {survey, QuestionIds, Questions} ->
            Qs1 = [ Q || {_, Question}=Q <- Questions, Question /= undefined ],

            case go_page(PageNr, Qs1, Answers2, Direction, Context) of
                {L,NewPageNr} when is_list(L) ->
                    % A new list of questions, PageNr might be another than expected
                    Vars = [ {id, Id},
                             {page_nr, NewPageNr},
                             {questions, [ [{id, QId} | question_to_props(Q)] || {QId, Q} <- L ]},
                             {pages, count_pages(Qs1)},
                             {answers, Answers2},
                             {history, [NewPageNr|History]}],
                    #render{template="_survey_question_page.tpl", vars=Vars};
                last ->
                    case z_session:get(mod_survey_editing, Context) of
                        {U, P} -> 
                            admin_edit_survey_result(Id, U, P, QuestionIds, Questions, Answers2, Context);
                        _ ->
                            %% That was the last page. Show a thank you and save the result.
                            case do_submit(Id, QuestionIds, Questions, Answers2, Context) of
                                ok ->
                                    case z_convert:to_bool(m_rsc:p(Id, survey_show_results, Context)) of
                                        true ->
                                            #render{template="_survey_results.tpl", vars=[{id,Id}, {inline, true}, {history,History}]};
                                        false ->
                                            #render{template="_survey_end.tpl", vars=[{id,Id}, {history,History}]}
                                    end;
                                {error, _Reason} ->
                                    #render{template="_survey_error.tpl", vars=[{id,Id}, {history,History}]}
                            end
                    end
            end;
        _NoSurvey ->
            % No survey defined, show an error page.
            #render{template="_survey_empty.tpl", vars=[{id,Id}]}
    end.


    %% @doc Count the number of pages in the survey
    count_pages([]) ->
        0;
    count_pages(L) ->
        count_pages(L, 1).

    count_pages([], N) ->
        N;
    count_pages([#survey_question{type=pagebreak}|L], N) ->
        L1 = lists:dropwhile(fun(#survey_question{type=pagebreak}) -> true; (_) -> false end, L),
        count_pages(L1, N+1);
    count_pages([_|L], N) ->
        count_pages(L, N).


    go_page(Nr, Qs, _Answers, exact, _Context) ->
        case fetch_page(Nr, Qs) of
            last ->
                last;
            {L,Nr1} ->
                L1 = lists:dropwhile(fun(#survey_question{type=pagebreak}) -> true; (_) -> false end, L),
                L2 = lists:takewhile(fun(#survey_question{type=pagebreak}) -> false; (_) -> true end, L1),
                {L2,Nr1}
        end;
    go_page(Nr, Qs, Answers, forward, Context) ->
        eval_page_jumps(fetch_page(Nr, Qs), Answers, Context).


    eval_page_jumps({[{_Id, #survey_question{type=pagebreak} = Q}|L],Nr}, Answers, Context) ->
        case survey_q_pagebreak:test(Q, Answers, Context) of
            ok -> 
                eval_page_jumps({L,Nr}, Answers, Context);
            {jump, Name} ->
                % Go to question 'name', count pagebreaks in between for the new page nr
                % Only allow jumping forward to prevent endless loops.
                eval_page_jumps(fetch_question_name(L, z_convert:to_list(Name), Nr, in_pagebreak), Answers, Context);
            {error, Reason} ->
                {error, Reason}
        end;
    eval_page_jumps({[], _Nr}, _Answers, _Context) ->
        last;
    eval_page_jumps(Other, _Answers, _Context) ->
        Other.


    fetch_question_name([], _Name, Nr, _State) ->
        {[], Nr};
    fetch_question_name([{_, #survey_question{name=Name}}|_] = Qs, Name, Nr, _State) ->
        {Qs, Nr};
    fetch_question_name([{_, #survey_question{type=pagebreak}}|Qs], Name, Nr, in_q) ->
        fetch_question_name(Qs, Name, Nr+1, in_pagebreak);
    fetch_question_name([{_, #survey_question{type=pagebreak}}|Qs], Name, Nr, in_pagebreak) ->
        fetch_question_name(Qs, Name, Nr, in_pagebreak);
    fetch_question_name([_|Qs], Name, Nr, _State) ->
        fetch_question_name(Qs, Name, Nr, in_q).


    %% @doc Fetch the Nth page. Multiple page breaks in a row count as a single page break.
    %%      Returns the question list at the point of the pagebreak, so any pagebreak jumps 
    %%      can be made.
    fetch_page(_Nr, []) ->
        last;
    fetch_page(Nr, L) ->
        fetch_page(1, Nr, L).

    fetch_page(_, _, []) ->
        last;
    fetch_page(N, Nr, L) when N >= Nr ->
        {L, N};
    fetch_page(N, Nr, [{_Id, #survey_question{type=pagebreak}}|_] = L) when N == Nr - 1 ->
        {L, Nr};
    fetch_page(N, Nr, [{_Id, #survey_question{type=pagebreak}}|L]) when N < Nr ->
        L1 = lists:dropwhile(fun(#survey_question{type=pagebreak}) -> true; (_) -> false end, L),
        fetch_page(N+1, Nr, L1);
    fetch_page(N, Nr, [_|L]) ->
        fetch_page(N, Nr, L).


%% @doc Map a question to template friendly properties
question_to_props(Q) ->
    [
        {name, Q#survey_question.name},
        {type, Q#survey_question.type},
        {question, Q#survey_question.question},
        {text, Q#survey_question.text},
        {parts, Q#survey_question.parts},
        {html, Q#survey_question.html},
        {is_required, Q#survey_question.is_required}
    ].


%% @doc Collect all answers per question, save to the database.
do_submit(SurveyId, QuestionIds, Questions, Answers, Context) ->
    {FoundAnswers, Missing} = collect_answers(QuestionIds, Questions, Answers),
    case Missing of
        [] ->
            m_survey:insert_survey_submission(SurveyId, FoundAnswers, Context),
            z_notifier:notify(#survey_submit{id=SurveyId, answers=FoundAnswers}, Context),
            ok;
        _ -> 
            {error, notfound}
    end.


%% @doc Collect all answers, report any missing answers.
%% @spec collect_answers(list(), proplist(), Context) -> {AnswerList, MissingIdsList}
collect_answers(QIds, Qs, Answers) ->
    collect_answers(QIds, Qs, Answers, [], []).


collect_answers([], _Qs, _Answers, FoundAnswers, Missing) ->
    {FoundAnswers, Missing};
collect_answers([QId|QIds], Qs, Answers, FoundAnswers, Missing) ->
    Q = proplists:get_value(QId, Qs),
    Module = module_name(Q),
    case Module:answer(Q, Answers) of
        {ok, none} ->
            collect_answers(QIds, Qs, Answers, FoundAnswers, Missing);
        {ok, AnswerList} -> 
            collect_answers(QIds, Qs, Answers, [{QId, AnswerList}|FoundAnswers], Missing);
        {error, missing} -> 
            case Q#survey_question.is_required of
                true ->
                    collect_answers(QIds, Qs, Answers, FoundAnswers, [QId|Missing]);
                false ->
                    collect_answers(QIds, Qs, Answers, FoundAnswers, Missing)
            end
    end.

%% @doc Save the modified survey results
admin_edit_survey_result(Id, U, P, QuestionIds, Questions, Answers, Context) ->
    z_session:set(mod_survey_editing, undefined, Context),
    {FoundAnswers, _Missing} = collect_answers(QuestionIds, Questions, Answers),
    m_survey:insert_survey_submission(Id, U, P, FoundAnswers, Context),
    Context1 = z_render:dialog_close(Context),
    z_render:update("survey-results", z_template:render("_admin_survey_editor_results.tpl", [{id, Id}], Context), Context1).


module_name(L) when is_list(L) ->
    module_name(list_to_atom(L));
module_name(Type) when is_atom(Type) ->
    module_name(#survey_question{type=Type});
module_name(#survey_question{type=Type}) ->
    list_to_atom("survey_q_"++atom_to_list(Type)).
