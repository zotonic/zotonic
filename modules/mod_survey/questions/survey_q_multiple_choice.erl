%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2013 Arjan Scherpenisse

%% Copyright 2013 Arjan Scherpenisse
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

-module(survey_q_multiple_choice).

-export([
    answer/3,
    prep_chart/3,
    prep_answer_header/2,
    prep_answer/3,
    prep_block/2,
    prep_totals/3,
    to_block/1
]).

-include_lib("zotonic.hrl").
-include("../survey.hrl").

answer(Block, Answers, _Context) ->
    Name = proplists:get_value(name, Block),
    case proplists:get_value(Name, Answers) of
        undefined -> {error, missing};
        V -> {ok, [{Name, case proplists:get_value(is_numeric, Block, false) of
                              true -> z_convert:to_integer(V);
                              _ -> V
                          end}]}
    end.


prep_totals(Block, [{_, Vals}], _) ->
    case proplists:get_value(is_numeric, Block) of
        true ->
            lists:foldl(fun({K, V}, Sum) ->
                                try
                                    (z_convert:to_integer(K) * z_convert:to_integer(V)) + Sum
                                catch
                                    error:badarg ->
                                        Sum
                                end
                        end,
                        0,
                        Vals);
        false ->
            undefined
    end.

prep_chart(_Q, [], _Context) ->
    undefined;
prep_chart(Q, [{_, Vals}]=Vs, Context) ->
    T = prep_totals(Q, Vs, Context),
    [
     {question, z_html:escape(proplists:get_value(prompt, Q), Context)},
     {values, Vals},
     {type, "pie"},
     {data, lists:map(fun tuple_to_list/1, Vals)},
     {has_totals, T =/= undefined},
     {totals, T}
    ].

prep_answer_header(Q, _Context) ->
    proplists:get_value(name, Q).

prep_answer(_Q, [], _Context) ->
    <<>>;
prep_answer(_Q, [{_Name, {Value, _Text}}|_], _Context) ->
    Value.

prep_block(B, _Context) ->
    B.


to_block(Q) ->
    [
        {type, survey_truefalse},
        {is_required, Q#survey_question.is_required},
        {name, z_convert:to_binary(Q#survey_question.name)},
        {prompt, z_convert:to_binary(Q#survey_question.question)}
    ].
