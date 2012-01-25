%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2011 Marc Worrell
%% @doc Open a dialog to edit a survey question.

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

-module(action_survey_dialog_survey_question).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zotonic.hrl").
-include("../survey.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    Id = z_convert:to_integer(proplists:get_value(id, Args)),
    QuestionId = proplists:get_value(question_id, Args),
    Postback = {dialog_survey_question, Id, QuestionId},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Fill the dialog with the duplicate page form. The form will be posted back to this module.
%% @spec event(Event, Context1) -> Context2
event(#postback{message={dialog_survey_question, Id, QuestionId}}, Context) ->
    Q = get_question(Id, QuestionId, Context),
    Vars = [
        {id, Id},
        {question_id, QuestionId},
        {delegate, atom_to_list(?MODULE)}
    ] ++ question_props(Q),
    z_render:dialog("Edit survey question.", "_action_dialog_survey_question.tpl", Vars, Context);

event(#submit{message={survey_question_save, ActionProps}}, Context) ->
    Id = proplists:get_value(id, ActionProps),
    QuestionId = proplists:get_value(question_id, ActionProps),
    Name = z_string:to_slug(z_string:trim(z_context:get_q("name", Context, ""))),
    Question = z_string:trim(z_context:get_q("question", Context, "")),
    Text = z_string:trim(z_context:get_q("text", Context, "")),
    IsRequired = z_convert:to_bool(z_context:get_q("is_required", Context, "")),
    Q = get_question(Id, QuestionId, Context),
    Q1 = question_render(Q#survey_question{name=Name, question=Question, text=Text, is_required=IsRequired}),
    save_question(Id, QuestionId, Q1, Context),
    Context1 = mod_survey:redraw_questions(Id, Context),
    z_render:dialog_close(Context1).


get_question(Id, QuestionId, Context) ->
    {survey, _QIds, Qs} = m_rsc:p(Id, survey, Context),
    proplists:get_value(QuestionId, Qs).

save_question(Id, QuestionId, Question, Context) ->
    {survey, QIds, Qs} = m_rsc:p(Id, survey, Context),
    m_rsc:update(Id, [{survey, {survey, QIds, z_utils:prop_replace(QuestionId, Question, Qs)}}], Context).


question_props(Q) ->
    Mod = mod_survey:module_name(Q),
    Mod:question_props(Q).

question_render(Q) ->
    Mod = list_to_atom("survey_q_"++z_convert:to_list(Q#survey_question.type)),
    Mod:render(Q).
