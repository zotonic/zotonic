%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2022 Marc Worrell
%% @doc Calculate the total test score for a survey.

%% Copyright 2022 Marc Worrell
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

-module(survey_test_results).

-export([
    calc_test_results/3,
    max_points/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-spec max_points(integer(), #context{}) -> integer().
max_points(Id, Context) ->
    case m_rsc:p(Id, blocks, Context) of
        [] ->
            0;
        Blocks when is_list(Blocks) ->
            lists:sum(lists:map(fun max_points_block/1, Blocks));
        _ ->
            0
    end.

max_points_block(#{ <<"is_test">> := true } = Block) ->
    Module = mod_survey:module_name(maps:get(<<"type">>, Block, undefined)),
    Module:test_max_points(Block);
max_points_block(_Block) ->
    0.

%% @doc Check all questions if they are test questions an calculate the test result.
-spec calc_test_results(m_rsc:resource_id(), list( proplists:proplist() ), z:context()) -> {integer(), list()}.
calc_test_results(SurveyId, Answers, Context) ->
    case m_rsc:p_no_acl(SurveyId, blocks, Context) of
        Blocks when is_list(Blocks)->
            count_points(Answers, Blocks, 0, [], Context);
        _ ->
            count_points(Answers, [], 0, [], Context)
    end.

count_points([], _Blocks, PtAcc, AsAcc, _Context) ->
    {PtAcc, lists:reverse(AsAcc)};
count_points([{Name,A}|As], Blocks, PtAcc, AsAcc, Context) ->
    case proplists:get_value(fixed_points, A) of
        N when is_integer(N) ->
            count_points(As, Blocks, PtAcc+N, [{Name,A}|AsAcc], Context);
        undefined ->
            Block = find_block(proplists:get_value(block, A), Blocks),
            case z_convert:to_bool(maps:get(<<"is_test">>, Block, false)) of
                true ->
                    % Check if given answer is correct
                    Type = maps:get(<<"type">>, Block),
                    {PtQ, AP} = question_points(Type, A, Block, Context),
                    count_points(As, Blocks, PtAcc+PtQ, [{Name,AP}|AsAcc], Context);
                false ->
                    count_points(As, Blocks, PtAcc, [{Name,A}|AsAcc], Context)
            end
    end.

-spec question_points(binary(), proplists:proplist(), map(), z:context()) -> {integer(), proplists:proplist()}.
question_points(<<"survey_thurstone">>, A, #{ <<"is_test">> := true } = Block, Context) ->
    Props = filter_survey_prepare_thurstone:survey_prepare_thurstone(Block, false, Context),
    QuestionOptions = maps:get(<<"answers">>, Props, []),
    Answered = make_list(proplists:get_value(answer, A)),
    IsNeg = z_convert:to_bool(maps:get(<<"is_test_neg">>, Props, false)),
    InputType = maps:get(<<"input_type">>, Props, <<>>),
    AnswerPoints = points(InputType, IsNeg, QuestionOptions, Answered),
    SummedPoints = erlang:max(0, sum(AnswerPoints)),
    A1 = [
        {points, SummedPoints},
        {answer_points, AnswerPoints}
        | A
    ],
    {SummedPoints, A1};
question_points(<<"survey_matching">>, A, _Block, _Context) ->
    {0, A};
question_points(_Type, A, _Block, _Context) ->
    {0, A}.


points(<<"multi">>, IsNeg, QuestionOptions, Answered) ->
    lists:map(
        fun(Q) ->
            Value = maps:get(<<"value">>, Q, undefined),
            IsCorrect = maps:get(<<"is_correct">>, Q, false),
            Points = case z_convert:to_integer(maps:get(<<"points_int">>, Q, 1)) of
                undefined -> 1;
                Pts -> Pts
            end,
            case lists:member(Value, Answered) of
                true when IsCorrect -> {Value, Points};
                true when IsNeg -> {Value, -Points};
                true -> {Value, 0};
                false when IsCorrect, IsNeg -> {Value, -Points};
                false when IsCorrect -> {Value, 0};
                false -> {Value, Points}
            end
        end,
        QuestionOptions);
points(_InputType, _IsNeg, QuestionOptions, Answered) ->
    lists:map(
        fun(Q) ->
            Value = maps:get(<<"value">>, Q, undefined),
            IsCorrect = maps:get(<<"is_correct">>, Q, false),
            Points = case z_convert:to_integer(maps:get(<<"points_int">>, Q, 1)) of
                undefined -> 1;
                Pts -> Pts
            end,
            case lists:member(Value, Answered) of
                true when IsCorrect -> {Value, Points};
                true -> {Value, 0};
                false -> {Value, 0}
            end
        end,
        QuestionOptions).

sum([]) -> 0;
sum(L) -> lists:sum([Pt || {_,Pt} <- L]).

make_list(B) when is_binary(B) -> B;
make_list(L) when is_list(L) -> L.

find_block(_Name, []) ->
    #{};
find_block(Name, [B|Bs]) ->
    case maps:get(<<"name">>, B, undefined) of
        Name -> B;
        _ -> find_block(Name, Bs)
    end.
