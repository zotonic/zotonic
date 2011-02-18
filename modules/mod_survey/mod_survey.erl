%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @date 2010-03-23
%% @doc Survey module.  Define surveys and let people fill them in.

%% Copyright 2010 Marc Worrell
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

-export([init/1]).

%% interface functions
-export([
    event/2,
    redraw_questions/2,
    delete_question/3,
    
    question_to_props/1,
    module_name/1
]).

-include_lib("zotonic.hrl").
-include("survey.hrl").

%% @doc Initilize the data model.
init(Context) ->
    m_survey:install(Context),
    z_datamodel:manage(?MODULE, datamodel(), Context).

%% @doc Handle drag/drop events from the survey admin
event({sort, Items, {dragdrop, {survey, [{id,Id}]}, _Delegate, "survey"}}, Context) ->
    event_sort(Id, Items, Context);

event({submit, {survey_submit, [{id,SurveyId}]}, FormId, _FormId}, Context) ->
    survey_submit:submit(SurveyId, FormId, Context);

event({postback, {survey_start, [{id, SurveyId}]}, _, _}, Context) ->
    render_next_page(SurveyId, 1, [], Context);

event({submit, {survey_next, Args}, _, _}, Context) ->
    {id, SurveyId} = proplists:lookup(id, Args),
    {page_nr, PageNr} = proplists:lookup(page_nr, Args),
    {answers, Answers} = proplists:lookup(answers, Args),
    render_next_page(SurveyId, PageNr+1, Answers, Context);

event({postback, {survey_back, Args}, _, _}, Context) ->
    {id, SurveyId} = proplists:lookup(id, Args),
    {page_nr, PageNr} = proplists:lookup(page_nr, Args),
    {answers, Answers} = proplists:lookup(answers, Args),
    render_next_page(SurveyId, PageNr-1, Answers, Context).



%%====================================================================
%% support functions
%%====================================================================


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
    Mod = list_to_atom("survey_q_"++z_convert:to_list(Type)),
    Mod:new().


%% @doc Fetch the next page from the survey, update the page view
render_next_page(Id, 0, Answers, Context) ->
    z_render:update("survey-question", #render{template="_survey_start.tpl", vars=[{id,Id},{answers,Answers}]}, Context);
render_next_page(Id, PageNr, Answers, Context) ->
    As = z_context:get_q_all_noz(Context),
    Answers1 = lists:foldl(fun({Arg,_Val}, Acc) -> proplists:delete(Arg, Acc) end, Answers, As),
    Answers2 = Answers1 ++ As,
    case m_rsc:p(Id, survey, Context) of
        {survey, QuestionIds, Questions} ->
            Qs = [ proplists:get_value(QId, Questions) || QId <- QuestionIds ],
            Qs1 = [ Q || Q <- Qs, Q /= undefined ],

            case fetch_page(PageNr, Qs1) of
                {L,NewPageNr} when is_list(L) ->
                    % A new list of questions, PageNr might be another than expected
                    Vars = [ {id, Id},
                             {page_nr, NewPageNr},
                             {questions, [ question_to_props(Q) || Q <- L ]},
                             {pages, count_pages(Qs1)},
                             {answers, Answers2}],
                    z_render:update("survey-question", #render{template="_survey_question_page.tpl", vars=Vars}, Context);
                last ->
                    % That was the last page. Show a thank you and save the result.
                    case do_submit(Id, QuestionIds, Questions, Answers2, Context) of
                        ok ->
                            z_render:update("survey-question", #render{template="_survey_end.tpl", vars=[{id,Id}]}, Context);
                        {error, _Reason} ->
                            z_render:update("survey-question", #render{template="_survey_error.tpl", vars=[{id,Id}]}, Context)
                    end
            end;
        _NoSurvey ->
            % No survey defined, show an error page.
            z_render:update("survey-question", #render{template="_survey_empty.tpl", vars=[{id,Id}]}, Context)
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

    %% @doc Fetch the Nth page.  Could return another page due to jumps in the pagebreaks.
    fetch_page(_Nr, []) ->
        last;
    fetch_page(Nr, L) ->
        fetch_page(1, Nr, L).

    fetch_page(_, _, []) ->
        last;
    fetch_page(N, Nr, L) when N >= Nr ->
        L1 = lists:takewhile(fun(#survey_question{type=pagebreak}) -> false; (_) -> true end, L),
        {L1, N};
    fetch_page(N, Nr, [#survey_question{type=pagebreak}|L]) when N < Nr ->
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
            ok;
        _ -> 
            {error, notfound}
    end.


%% @doc Collect all answers, report any missing answers.
%% @type collect_answers(proplist(), Context) -> {AnswerList, MissingIdsList}
collect_answers(QIds, Qs, Answers) ->
    collect_answers(QIds, Qs, Answers, [], []).


collect_answers([], _Qs, _Answers, FoundAnswers, Missing) ->
    {FoundAnswers, Missing};
collect_answers([QId|QIds], Qs, Answers, FoundAnswers, Missing) ->
    Q = proplists:get_value(QId, Qs),
    Module = module_name(Q),
    case Module:answer(Q, Answers) of
        {ok, none} -> collect_answers(QIds, Qs, Answers, FoundAnswers, Missing);
        {ok, AnswerList} -> collect_answers(QIds, Qs, Answers, [{QId, AnswerList}|FoundAnswers], Missing);
        {error, missing} -> collect_answers(QIds, Qs, Answers, FoundAnswers, [QId|Missing])
    end.

module_name(#survey_question{type=Type}) ->
    list_to_atom("survey_q_"++atom_to_list(Type)).



datamodel() ->
    [
        {categories, [
                {survey, undefined, [{title, "Survey"}]}
            ]}
    ].


