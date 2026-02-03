%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Filter to check if a survey is configured to save intermediate
%% results.
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

-module(filter_survey_is_save_intermediate).
-moduledoc("
Check if the given survey is configured to save intermediate results.
Always returns a boolean value.
").

-export([
    survey_is_save_intermediate/2
    ]).

survey_is_save_intermediate(undefined, _Context) ->
    false;
survey_is_save_intermediate(SurveyId, Context) when is_integer(SurveyId) ->
    mod_survey:is_save_intermediate(SurveyId, Context);
survey_is_save_intermediate(SurveyId, Context) ->
    survey_is_save_intermediate(m_rsc:rid(SurveyId, Context), Context).
