%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012-2023 Marc Worrell

%% Copyright 2012-2023 Marc Worrell
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

-module(survey_q_upload).

-export([
    answer/4,
    prep_answer_header/2,
    prep_answer/3,
    prep_block/2,
    prep_chart/3
]).


answer(_SurveyId, _Block, _Answers, _Context) ->
    {ok, none}.

prep_answer_header(_Q, _Context) ->
    [].

prep_answer(_Q, _Answer, _Context) ->
    [].

prep_block(B, _Context) ->
    B.

prep_chart(_Q, _As, _Context) ->
    undefined.

