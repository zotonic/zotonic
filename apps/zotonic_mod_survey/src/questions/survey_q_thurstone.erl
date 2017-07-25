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

-module(survey_q_thurstone).

-export([
    answer/3,
    prep_chart/3,
    prep_answer_header/2,
    prep_answer/3,
    prep_block/2,
    to_block/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_survey/include/survey.hrl").


answer(Block, Answers, Context) ->
    Name = proplists:get_value(name, Block),
    Props = filter_survey_prepare_thurstone:survey_prepare_thurstone(Block, Context),
    Options = proplists:get_value(answers, Props),
    case proplists:get_value(Name, Answers) of
        undefined ->
            {error, missing};
        Label when is_binary(Label) ->
            case proplists:is_defined(Label, Options) of
                true -> {ok, [{Name, Label}]};
                false -> {error, missing}
            end;
        Value when is_list(Value) ->
            Defined = lists:filter(fun(Lab) -> proplists:is_defined(Lab, Options) end, Value),
            Flattened = string:join([ z_convert:to_list(V) || V <- Defined ], "#"),
            {ok, [{Name, {text, list_to_binary(Flattened)}}]}
    end.


prep_chart(_Q, [], _Context) ->
    undefined;
prep_chart(Block, [{Name, {text, Vals0}}], Context) ->
    prep_chart(Block, [{Name, Vals0}], Context);
prep_chart(Block, [{_, Vals}], Context) ->
    Props = filter_survey_prepare_thurstone:survey_prepare_thurstone(Block, Context),
    Answers = proplists:get_value(answers, Props),
    Labels = [ Lab || {Lab,_} <- Answers ],
    Values = [ proplists:get_value(C, Vals, 0) || C <- Labels ],
    Sum = case lists:sum(Values) of 0 -> 1; N -> N end,
    Perc = [ round(V*100/Sum) || V <- Values ],
    [
        {question, proplists:get_value(prompt, Block)},
        {values, lists:zip(Labels, Values)},
        {type, "pie"},
        {data, [{L,P} || {L,P} <- lists:zip(Labels, Perc), P /= 0]},
        {answers, Answers}
    ].

prep_answer_header(Q, _Context) ->
    Name = proplists:get_value(name, Q),
    case is_multiple(Q) of
        true -> [ <<Name/binary, $:, K/binary>> || {K,_} <- proplists:get_value(answers, Q) ];
        false -> Name
    end.

prep_answer(Q, [], _Context) ->
    prep(Q, []);
prep_answer(Q, [{_Name, {undefined, Text}}|_], _Context) ->
    prep(Q, binary:split(Text, <<$#>>, [global]));
prep_answer(Q, [{_Name, {Value, _Text}}|_], _Context) ->
    prep(Q, [Value]).

prep(Q, Vs) ->
    case is_multiple(Q) of
        false ->
            case Vs of
                [V|_] -> V;
                [] -> undefined
            end;
        true ->
            [
                case lists:member(K, Vs) of
                    true -> K;
                    false -> <<>>
                end
                || {K, _} <- proplists:get_value(answers, Q)
            ]
    end.

is_multiple(Q) ->
    case proplists:get_value(input_type, Q) of
        <<"multi">> ->
            true;
        undefined ->
            % Older surveys had the is_multiple property
            z_convert:to_bool(proplists:get_value(is_multiple, Q));
        _ ->
            false
    end.


prep_block(Block, Context) ->
    Props = filter_survey_prepare_thurstone:survey_prepare_thurstone(Block, Context),
    Props ++ Block.


to_block(Q) ->
    [
        {type, survey_thurstone},
        {is_required, Q#survey_question.is_required},
        {is_multiple, false},
        {name, z_convert:to_binary(Q#survey_question.name)},
        {prompt, z_convert:to_binary(Q#survey_question.question)},
        {answers, z_convert:to_binary(Q#survey_question.text)}
    ].

