%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011-2021 Marc Worrell
%% @doc Render a complete survey as a single page. Optionally
%% with answers from a previous submit.
%% @end

%% Copyright 2011-2021 Marc Worrell
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

-module(scomp_survey_poll).
-moduledoc("
Show a given survey (with the `id` parameter) as a “poll”. This presents a simpler interface, in which the user is
directly asked to enter some information, e.g. make a choice between certain things:


```django
{% poll id=123 %}
```
").

-behaviour(zotonic_scomp).
-export([vary/2, render/3]).
-export([single_result/3]).

-include_lib("zotonic_core/include/zotonic.hrl").

vary(_,_) -> nocache.

render(Args, _Vars, Context) ->
    {id, SurveyId} = proplists:lookup(id, Args),
    ElementId = proplists:get_value(element_id, Args, "survey-question"),
    AnswerId = proplists:get_value(answer_id, Args),
    Actions = proplists:get_all_values(action, Args),
    Actions1 = [ Action || Action <- Actions, Action =/= undefined ],
    Render = case z_acl:rsc_editable(SurveyId, Context) of
        true when is_integer(AnswerId) ->
            {UserId, Answers} = single_result(SurveyId, AnswerId, Context),
            Editing = {editing, AnswerId, Actions1},
            Args1 = [ {answer_user_id, UserId} | Args ],
            mod_survey:render_next_page(SurveyId, 1, exact, Answers, [], Editing, Args1, Context);
        _NotEditing ->
            mod_survey:render_next_page(SurveyId, 1, exact, [], [], undefined, Args, Context)
    end,
    {ok, z_template:render(Render#render{vars=[{element_id, ElementId}|Render#render.vars]}, Context)}.

single_result(SurveyId, AnswerId, Context) ->
    case m_survey:single_result(SurveyId, AnswerId, Context) of
        None when None =:= undefined; None =:= [] ->
            {undefined, []};
        Result ->
            Answers = proplists:get_value(answers, Result, []),
            Answers1 = lists:map(
                fun({QName, Ans}) ->
                    Answer = proplists:get_value(answer, Ans),
                    {QName, Answer}
                end,
                Answers),
            {proplists:get_value(user_id, Result), Answers1}
    end.
