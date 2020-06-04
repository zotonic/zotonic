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
    prep_answer_score/3,
    prep_block/2,
    to_block/1,
    is_multiple/1,
    test_max_points/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_survey/include/survey.hrl").


-spec answer( map(), list(), z:context() ) -> {ok, list()} | {error, missing}.
answer(Block, Answers, Context) ->
    Name = maps:get(<<"name">>, Block, undefined),
    Props = filter_survey_prepare_thurstone:survey_prepare_thurstone(Block, false, Context),
    Options = maps:get(<<"answers">>, Props),
    case proplists:get_value(Name, Answers) of
        undefined ->
            {error, missing};
        Label when is_binary(Label) ->
            case is_defined_value(Label, Options) of
                true -> {ok, [{Name, [Label]}]};
                false -> {error, missing}
            end;
        Value when is_list(Value) ->
            Defined = lists:filter(fun(V) -> is_defined_value(V, Options) end, Value),
            {ok, [{Name, Defined}]}
    end.

is_defined_value(_Val, []) -> false;
is_defined_value(Val, [Opt|Options]) ->
    case maps:get(<<"value">>, Opt) of
        Val -> true;
        _ -> is_defined_value(Val, Options)
    end.

-spec prep_chart( map(), list(), z:context() ) -> map() | undefined.
prep_chart(_Q, [], _Context) ->
    undefined;
prep_chart(Block, [{Name, {text, Vals0}}], Context) ->
    prep_chart(Block, [{Name, Vals0}], Context);
prep_chart(Block, [{_, Vals}], Context) ->
    Props = filter_survey_prepare_thurstone:survey_prepare_thurstone(Block, false, Context),
    Answers = maps:get(<<"answers">>, Props),
    Labels = [ maps:get(<<"option">>, Ans) || Ans <- Answers ],
    ValueLabels = [ maps:get(<<"value">>, Ans) || Ans <- Answers ],
    Values = [ proplists:get_value(C, Vals, 0) || C <- ValueLabels ],
    Sum = case lists:sum(Values) of 0 -> 1; N -> N end,
    Perc = [ round(V*100/Sum) || V <- Values ],
    #{
        <<"question">> => maps:get(<<"prompt">>, Block, undefined),
        <<"values">> => lists:zip(Labels, Values),
        <<"type">> => <<"pie">>,
        <<"data">> => [{L,P} || {L,P} <- lists:zip(Labels, Perc), P /= 0],
        <<"answers">> => Answers
    }.

prep_answer_header(Q, _Context) ->
    Name = maps:get(<<"name">>, Q, undefined),
    case is_multiple(Q) of
        true ->
            [
                <<Name/binary, $:, (maps:get(<<"value">>, Ans))/binary>>
                || Ans <- maps:get(<<"answers">>, Q, [])
            ];
        false ->
            Name
    end.

prep_answer(PreppedBlock, [], Context) ->
    prep(PreppedBlock, [], Context);
prep_answer(PreppedBlock, [{_Name, Ans}|_], Context) when is_list(Ans) ->
    prep(PreppedBlock, Ans, Context);
prep_answer(PreppedBlock, [{_Name, Value}|_], Context) ->
    prep(PreppedBlock, [Value], Context).

prep(PreppedBlock, Vs, _Context) ->
    case is_multiple(PreppedBlock) of
        false ->
            case Vs of
                [V|_] -> V;
                [] -> undefined
            end;
        true ->
            [
                begin
                    K = maps:get(<<"value">>, Ans),
                    case lists:member(K, Vs) of
                        true -> K;
                        false -> <<>>
                    end
                end
                || Ans <- maps:get(<<"answers">>, PreppedBlock, [])
            ]
    end.

prep_answer_score(PreppedBlock, [], Context) ->
    prep_score(PreppedBlock, [], Context);
prep_answer_score(PreppedBlock, [{_Name,Ans}|_], Context) when is_list(Ans) ->
    prep_score(PreppedBlock, Ans, Context).

prep_score(PreppedBlock, StoredAnswer, _Context) ->
    Answer = ensure_list(proplists:get_value(answer, StoredAnswer, [])),
    Points = ensure_list(proplists:get_value(answer_points, StoredAnswer, [])),
    case is_multiple(PreppedBlock) of
        false ->
            K = case Answer of
                [Ans|_] -> Ans;
                _ -> <<>>
            end,
            [K, proplists:get_value(K, Points, 0)];
        true ->
            lists:flatten([
                begin
                    K = maps:get(<<"value">>, Ans),
                    case lists:member(K, Answer) of
                        true ->
                            [K, proplists:get_value(K, Points, 0)];
                        false ->
                            [<<>>, proplists:get_value(K, Points, 0)]
                    end
                end
                || Ans <- maps:get(<<"answers">>, PreppedBlock, [])
            ])
    end.

ensure_list(L) when is_list(L) -> L;
ensure_list(V) -> [V].


is_multiple(Q) ->
    case maps:get(<<"input_type">>, Q, undefined) of
        <<"multi">> ->
            true;
        undefined ->
            % Older surveys had the is_multiple property
            z_convert:to_bool(maps:get(<<"is_multiple">>, Q, false));
        _ ->
            false
    end.


prep_block(Block, Context) ->
    Props = filter_survey_prepare_thurstone:survey_prepare_thurstone(Block, false, Context),
    maps:merge(Props, Block).


to_block(Q) ->
    #{
        <<"type">> => <<"survey_thurstone">>,
        <<"is_required">> => Q#survey_question.is_required,
        <<"is_multiple">> => false,
        <<"name">> => z_convert:to_binary(Q#survey_question.name),
        <<"prompt">> => z_convert:to_binary(Q#survey_question.question),
        <<"answers">> => z_convert:to_binary(Q#survey_question.text)
    }.

test_max_points(Block) ->
    IsMultiple = is_multiple(Block),
    case survey_test_results:block_test_points(Block) of
        undefined -> 0;
        Points when not IsMultiple ->
            % Only a single correct answer possible
            Points;
        Points when IsMultiple ->
            % Every answer is counted
            Options = thurstone_options(Block),
            Options1 = [ z_string:trim(Opt) || Opt <- Options ],
            length([ Opt || Opt <- Options1, Opt /= <<>> ]) * Points
    end.

thurstone_options(Block) ->
    case maps:get(<<"answers">>, Block, <<>>) of
        {trans, [{_,Text}|_]} ->
            binary:split(Text, <<"\n">>, [global]);
        Text when is_binary(Text) ->
            binary:split(Text, <<"\n">>, [global]);
        _ ->
            []
    end.
