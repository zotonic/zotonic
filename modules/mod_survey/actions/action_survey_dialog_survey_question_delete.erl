%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2011 Marc Worrell
%% @doc Delete a question from a survey, request confirmation.

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

-module(action_survey_dialog_survey_question_delete).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    Id = z_convert:to_integer(proplists:get_value(id, Args)),
    QuestionId = proplists:get_value(question_id, Args),
    Postback = {survey_delete_question_dialog, Id, QuestionId},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Fill the dialog with the delete confirmation template. The next step will ask to delete the resource
%% @spec event(Event, Context1) -> Context2
event(#postback{message={survey_delete_question_dialog, Id, QuestionId}}, Context) ->
    case z_acl:rsc_editable(Id, Context) of
        true ->
            Vars = [
                {id, Id},
                {question_id, QuestionId},
                {delegate, ?MODULE}
            ],
            z_render:dialog("Confirm delete", "_action_dialog_survey_question_delete.tpl", Vars, Context);
        false ->
            z_render:growl_error("You are not allowed to edit this page.", Context)
    end;

event(#postback{message={survey_delete_question, Props}}, Context) ->
    Id = proplists:get_value(id, Props),
    QuestionId = proplists:get_value(question_id, Props),
    mod_survey:delete_question(Id, QuestionId, Context).
    
    

