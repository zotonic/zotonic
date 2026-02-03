%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012-2026 Marc Worrell
%% @doc Filter to split multi-value survey answers on the '#' character.
%% @end

%% Copyright 2012-2026 Marc Worrell
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

-module(filter_survey_answer_split).
-moduledoc("
Filter to split multi-value survey answers on the '#' character.

If a question can have multiple answers then they can be stored in a single
string separated by the '#' character. This filter splits such strings.

Examples of usage:

  {{ answer | survey_answer_split }}

will split the variable 'answer' on the '#' character.

Question types that can have multiple answers are:

  * thurstone
  * multiple choice
  * category select
").

-export([
    survey_answer_split/3
]).

survey_answer_split(V, _Block, _Context) when is_binary(V) ->
    binary:split(V, <<$#>>);
survey_answer_split(V, _Block, _Context) ->
    V.


