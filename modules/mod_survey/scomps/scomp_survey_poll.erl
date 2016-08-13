%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell

%% Copyright 2011 Marc Worrell
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

-behaviour(gen_scomp).
-export([vary/2, render/3]).

-include("zotonic.hrl").
-include("../survey.hrl").

vary(_,_) -> nocache.

render(Args, _Vars, Context) ->
    {id, SurveyId} = proplists:lookup(id, Args),
    ElementId = proplists:get_value(element_id, Args, "survey-question"),
    UserId = z_convert:to_integer(proplists:get_value(user_id, Args)),
    PersistentId = proplists:get_value(persistent_id, Args),
    Actions = proplists:get_all_values(action, Args),
    Actions1 = [ Action || Action <- Actions, Action =/= undefined ],
    Render = case {UserId, PersistentId, z_acl:rsc_editable(SurveyId, Context)} of
                {UserId, PersistentId, true} when is_integer(UserId); is_binary(PersistentId) ->
                    Answers = m_survey:single_result(SurveyId, UserId, PersistentId, Context),
                    Answers1 = lists:flatten([ Vs || {_,Vs} <- Answers ]),
                    Editing = {editing, UserId, PersistentId, Actions1},
                    mod_survey:render_next_page(SurveyId, 1, exact, Answers1, [], Editing, Context);
                _ ->
                    mod_survey:render_next_page(SurveyId, 1, exact, [], [], undefined, Context)
             end,
    {ok, z_template:render(Render#render{vars=[{element_id, ElementId}|Render#render.vars]}, Context)}.
