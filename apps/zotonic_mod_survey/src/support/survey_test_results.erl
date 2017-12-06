-module(survey_test_results).

-export([
    calc_test_results/3,
    max_points/2,
    block_test_points/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_survey/include/survey.hrl").


-spec max_points(integer(), #context{}) -> integer().
max_points(Id, Context) ->
    case m_rsc:p(Id, blocks, Context) of
        undefined -> 0;
        [] -> 0;
        Blocks ->
            lists:sum(lists:map(fun max_points_block/1, Blocks))
    end.

max_points_block(Block) ->
    case z_convert:to_bool(proplists:get_value(is_test, Block)) of
        true ->
            Module = mod_survey:module_name(proplists:get_value(type, Block)),
            Module:test_max_points(Block);
        false ->
            0
    end.

%% @doc Check all questions if they are test questions an calculate the test result.
-spec calc_test_results(integer(), list(), #context{}) -> {integer(), #context{}}.
calc_test_results(SurveyId, Answers, Context) ->
    case m_rsc:p_no_acl(SurveyId, blocks, Context) of
        Blocks when is_list(Blocks)->
            count_points(Answers, Blocks, 0, [], Context);
        _ ->
            {0, Answers}
    end.

count_points([], _Blocks, PtAcc, AsAcc, _Context) ->
    {PtAcc, lists:reverse(AsAcc)};
count_points([{Name,A}|As], Blocks, PtAcc, AsAcc, Context) ->
    Block = find_block(proplists:get_value(block, A), Blocks),
    case z_convert:to_bool(proplists:get_value(is_test, Block)) of
        true ->
            % Check if given answer is correct
            Type = proplists:get_value(type, Block),
            {PtQ, AP} = question_points(Type, A, Block, Context),
            count_points(As, Blocks, PtAcc+PtQ, [{Name,AP}|AsAcc], Context);
        false ->
            count_points(As, Blocks, PtAcc, [{Name,A}|AsAcc], Context)
    end.

question_points(<<"survey_thurstone">>, A, Block, Context) ->
    case block_test_points(Block) of
        GoodPoints when is_integer(GoodPoints) ->
            Props = filter_survey_prepare_thurstone:survey_prepare_thurstone(Block, false, Context),
            QuestionOptions = proplists:get_value(answers, Props),
            Answered = make_list(proplists:get_value(answer, A)),
            IsMulti = survey_q_thurstone:is_multiple(Block),
            WrongPoints = case IsMulti of
                true ->
                    case z_convert:to_bool(proplists:get_value(is_test_neg, Block, false)) of
                        true -> 0 - GoodPoints;
                        false -> 0
                    end;
                false ->
                    0
            end,
            AnswerPoints = [
                case is_correct(IsMulti, Answered, Q) of
                    true -> {proplists:get_value(value, Q), GoodPoints};
                    false -> {proplists:get_value(value, Q), WrongPoints}
                end
                || Q <- QuestionOptions
            ],
            Points = erlang:max(0, sum(AnswerPoints)),
            A1 = [
                {points,Points},
                {answer_points, AnswerPoints}
                | A
            ],
            {Points, A1};
        _ ->
            {0, A}
    end;
question_points(<<"survey_matching">>, A, _Block, _Context) ->
    ?DEBUG({todo, matching_score}),
    {0, A};
question_points(_Type, A, _Block, _Context) ->
    {0, A}.

block_test_points(Block) ->
    case z_convert:to_bool(proplists:get_value(is_test, Block)) of
        false -> undefined;
        true ->
            case proplists:get_value(test_points, Block) of
                undefined -> 1;
                <<>> -> 1;
                TestPoints -> z_convert:to_integer(TestPoints)
            end
    end.

sum([]) -> 0;
sum(L) -> lists:sum([Pt || {_,Pt} <- L]).

make_list(B) when is_binary(B) -> B;
make_list(L) when is_list(L) -> L.

find_block(_Name, []) -> [];
find_block(Name, [B|Bs]) ->
    case proplists:get_value(name, B) of
        Name -> B;
        _ -> find_block(Name, Bs)
    end.

is_correct(true, Answers, Q) ->
    V = proplists:get_value(value, Q),
    case proplists:get_value(is_correct, Q) of
        true -> lists:member(V, Answers);
        false -> not lists:member(V, Answers)
    end;
is_correct(false, Answers, Q) ->
    V = proplists:get_value(value, Q),
    case proplists:get_value(is_correct, Q) of
        true -> lists:member(V, Answers);
        false -> false
    end.

