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
    delete_question/3
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
    survey_submit:submit(SurveyId, FormId, Context).

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


datamodel() ->
    [
        {categories, [
                {survey, undefined, [{title, "Survey"}]}
            ]}
    ].


