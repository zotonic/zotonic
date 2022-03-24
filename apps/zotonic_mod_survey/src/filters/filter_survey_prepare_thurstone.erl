%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012-2022 Marc Worrell
%% @doc Prepeare a thurstone question for further processing. This maps
%% Zotonic 0.x Thurstone answers text to 1.x format with a list of answers.

%% Copyright 2012-2022 Marc Worrell
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

-module(filter_survey_prepare_thurstone).

-export([
    survey_prepare_thurstone/1,
    survey_prepare_thurstone/2,
    survey_prepare_thurstone/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Used by other modules to normalize Thurstone questions.
-spec survey_prepare_thurstone( map() ) -> map().
survey_prepare_thurstone(Blk) ->
    survey_prepare_thurstone_1(Blk, false).

%% @doc Filter function to normalize Thurstone questions.
-spec survey_prepare_thurstone( map(), z:context() ) -> map().
survey_prepare_thurstone(Blk, Context) ->
    survey_prepare_thurstone(Blk, undefined, Context).

%% @doc Filter function to normalize Thurstone questions, with optional
%% randomize option to return a randomly ordered answers list.
-spec survey_prepare_thurstone( map(), boolean() | undefined, z:context() ) -> map().
survey_prepare_thurstone(Blk, undefined, _Context) ->
    survey_prepare_thurstone_1(Blk, maps:get(<<"is_random">>, Blk, false));
survey_prepare_thurstone(Blk, IsRandom, _Context) ->
    survey_prepare_thurstone_1(Blk, IsRandom).

survey_prepare_thurstone_1(Blk, IsRandom) ->
    Answers = prepare_answers(Blk),
    Answers1 = maybe_randomize(z_convert:to_bool(IsRandom), Answers),
    Blk1 = case z_convert:to_bool(maps:get(<<"is_test">>, Blk, false)) of
        true ->
            Blk#{
                <<"is_test">> => true,
                <<"is_test_direct">> => z_convert:to_bool(maps:get(<<"is_test_direct">>, Blk, false)),
                <<"answers">> => Answers1
            };
        false ->
            Blk#{
                <<"is_test">> => false,
                <<"is_test_direct">> => false,
                <<"answers">> => Answers1
            }
    end,
    maps:without([ <<"is_test_neg">>, <<"test_points">> ], Blk1).

maybe_randomize(false, List) -> List;
maybe_randomize(true, List) -> z_utils:randomize(List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% Conversion of 0.x Thurstone questions to 1.x formatting %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prepare_answers(#{ <<"answers">> := As }) when is_list(As) ->
    % Nothing to do - 1.x version with a list (of maps)
    As;
prepare_answers(#{ <<"answers">> := #trans{} = A } = Block) ->
    % 0.x with a translatable text
    prepare_answers_1(A, Block);
prepare_answers(#{ <<"answers">> := A } = Block) when is_binary(A) ->
    % 0.x with a non translated binary text
    prepare_answers_1(A, Block);
prepare_answers(_Block) ->
    [].

% Split the pre-1.x answers text into separate answers.
prepare_answers_1(Text, Block) ->
    Lines = split_text(Text),
    case z_convert:to_bool(maps:get(<<"is_test">>, Block, false)) of
        true ->
            InputType = maps:get(<<"input_type">>, Block, <<>>),
            TestPoints = case z_convert:to_integer(maps:get(<<"test_points">>, Block, 1)) of
                undefined -> 1;
                TP -> TP
            end,
            lists:map(
                fun
                    (#{ <<"is_correct">> := true } = Ans) ->
                        Ans#{
                            <<"points_int">> => TestPoints
                        };
                    (#{ <<"is_correct">> := false } = Ans) when InputType =:= <<"multi">> ->
                        Ans#{
                            <<"points_int">> => TestPoints
                        };
                    (Ans) ->
                        Ans#{
                            <<"is_correct">> := false,
                            <<"points_int">> => 0
                        }
                end,
                Lines);
        false ->
            Lines
    end.

split_text(#trans{ tr = [] }) ->
    [];
split_text(#trans{ tr = Tr }) ->
    IsoAns = lists:map(
        fun({Iso,Text}) ->
            As = split_markers(split_lines(Text)),
            lists:map(
                fun(#{ <<"option">> := T } = Ans) ->
                    Ans#{
                        <<"option">> => #trans{ tr = [ {Iso, T} ]}
                    }
                end,
                As)
        end,
        Tr),
    [ First | Rest ] = IsoAns,
    % Merge the language variations of the Rest into the first
    % answer. We combine on the value, which must be the same.
    % If a value is not in the first, then it is added.
    lists:foldl(
        fun(Ans, Acc) ->
            merge_answer_text(Ans, Acc)
        end,
        First,
        Rest);
split_text(Text) when is_binary(Text) ->
    split_markers(split_lines(Text)).

merge_answer_text(Ans, AnsAcc) ->
    lists:foldl(
        fun(#{ <<"value">> := V, <<"option">> := T }, Acc) ->
            merge_answer_text_value(Acc, V, T, [])
        end,
        AnsAcc,
        Ans).

merge_answer_text_value([], V, T, AnsAcc) ->
    A = #{
        <<"value">> => V,
        <<"option">> => T
    },
    lists:reverse([ A | AnsAcc ]);
merge_answer_text_value([ #{ <<"value">> := V } = A | As ], V, T, AnsAcc) ->
    A1 = A#{
        <<"option">> := merge_trans(maps:get(<<"option">>, A), T)
    },
    lists:reverse([ A1 | AnsAcc ], As);
merge_answer_text_value([ A | As ], V, T, AnsAcc) ->
    merge_answer_text_value(As, V, T, [ A | AnsAcc ]).

merge_trans(#trans{ tr = A }, #trans{ tr = B }) ->
    #trans{ tr = A ++ B }.

split_lines(undefined) ->
    [];
split_lines(Text) ->
    Options = binary:split(z_string:trim(Text), <<"\n">>, [global]),
    Lines = [ z_string:trim(Option) || Option <- Options ],
    lists:filter(
        fun
            (<<>>) -> false;
            (_) -> true
        end,
        Lines).

split_markers(Qs) ->
    split_markers(Qs, 1, []).

split_markers([], _N, Acc) ->
    lists:reverse(Acc);
split_markers([Opt|Qs], N, Acc) ->
    split_markers(Qs, N+1, [split_marker(Opt, N)|Acc]).

split_marker(<<$*,Line/binary>>, N) ->
    split_marker_1(true, z_string:trim(Line), N);
split_marker(Line, N) ->
    split_marker_1(false, Line, N).

split_marker_1(IsCorrect, Line, N) ->
    case split_kv(Line) of
        [Value,Option] ->
            #{
                <<"value">> => Value,
                <<"option">> => Option,
                <<"is_correct">> => IsCorrect
            };
        [Option] ->
            #{
                <<"value">> => z_convert:to_binary(N),
                <<"option">> => Option,
                <<"is_correct">> => IsCorrect
            }
    end.

split_kv(Line) ->
    split_kv(Line, <<>>).

split_kv(<<>>, Acc) -> [Acc];
split_kv(<<"&#", Rest/binary>>, Acc) -> split_kv(Rest, <<Acc/binary, "&#">>);
split_kv(<<"#", Rest/binary>>, Acc) -> [Acc,Rest];
split_kv(<<C/utf8, Rest/binary>>, Acc) -> split_kv(Rest, <<Acc/binary, C/utf8>>).
