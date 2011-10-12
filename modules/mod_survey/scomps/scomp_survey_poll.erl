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
    PersistentId = proplists:get_value(persistent_id, Args),
    UserId = proplists:get_value(user_id, Args),
    Answers = m_survey:single_result(SurveyId, UserId, PersistentId, Context),
    case z_convert:to_bool(proplists:get_value(editing, Args)) andalso z_acl:rsc_editable(SurveyId, Context) of
        true ->
            z_session:set(mod_survey_editing, {UserId, PersistentId}, Context);
        false -> nop
    end,
    Render = mod_survey:render_next_page(SurveyId, 1, exact, Answers, [], Context),
    {ok, z_template:render(Render#render{vars=[{element_id, ElementId}|Render#render.vars]}, Context)}.
