%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011-2012 Marc Worrell

%% Copyright 2011-2012 Marc Worrell
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

-module(survey_q_long_answer).

-export([
    answer/3,
    prep_answer_header/2,
    prep_answer/3,
    to_block/1
]).

-include("../survey.hrl").
 

answer(Block, Answers, _Context) ->
    Name = proplists:get_value(name, Block),
    case proplists:get_value(z_convert:to_list(Name), Answers) of
        undefined ->
            {error, missing};
        Value ->
            case z_string:trim(Value) of
                [] -> {error, missing};
                V -> {ok, [{Name, {text, z_convert:to_binary(V)}}]}
            end
    end.

prep_answer_header(Block, _Context) ->
    proplists:get_value(name, Block).

prep_answer(_Q, [], _Context) ->
    <<>>;
prep_answer(_Q, [{_Name, {_Value, Text}}], _Context) ->
    z_convert:to_binary(Text).


to_block(Q) ->
    [
        {type, survey_long_answer},
        {is_required, Q#survey_question.is_required},
        {name, z_convert:to_binary(Q#survey_question.name)},
        {prompt, z_convert:to_binary(Q#survey_question.question)}
    ].

