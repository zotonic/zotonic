%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Check if the survey allows to go back in the given history.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(filter_survey_is_history_back).

-export([
    survey_is_history_back/3
]).

survey_is_history_back(undefined, _History, _Context) ->
    false;
survey_is_history_back(SurveyId, [_CurrentPageNr, PrevPageNr | _], Context) when is_integer(SurveyId), is_integer(PrevPageNr) ->
    mod_survey:is_page_back_allowed(SurveyId, PrevPageNr, Context);
survey_is_history_back(SurveyId, History, Context) when not is_integer(SurveyId) ->
    survey_is_history_back(m_rsc:rid(SurveyId, Context), History, Context);
survey_is_history_back(_SurveyId, _History, _Context) ->
    false.
