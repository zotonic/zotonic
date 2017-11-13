%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012 Marc Worrell

%% Copyright 2012 Marc Worrell
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
    survey_prepare_thurstone/2,
    survey_prepare_thurstone/3
]).

survey_prepare_thurstone(Blk, Context) ->
    survey_prepare_thurstone(Blk, undefined, Context).

survey_prepare_thurstone(Blk, undefined, Context) ->
    survey_prepare_thurstone_1(Blk, proplists:get_value(is_random, Blk, false), Context);
survey_prepare_thurstone(Blk, IsRandom, Context) ->
    survey_prepare_thurstone_1(Blk, IsRandom, Context).

survey_prepare_thurstone_1(Blk, IsRandom, Context) ->
    Answers = z_trans:lookup_fallback(
                    proplists:get_value(answers, Blk, <<>>),
                    Context),
    Qs = maybe_randomize(
                z_convert:to_bool(IsRandom),
                split_markers(split_lines(Answers))),
    case z_convert:to_bool(proplists:get_value(is_test, Blk)) of
        true ->
            [
                {is_test, true},
                {is_test_direct, z_convert:to_bool(proplists:get_value(is_test_direct, Blk))},
                {answers, Qs}
            ];
        false ->
            [
                {is_test, false},
                {answers, Qs}
            ]
    end.

maybe_randomize(false, List) -> List;
maybe_randomize(true, List) -> z_utils:randomize(List).

split_lines(Text) ->
    Options = binary:split(z_string:trim(Text), <<"\n">>, [global]),
    [ z_string:trim(Option) || Option <- Options ].


split_markers(Qs) ->
    split_markers(Qs, 1, []).

split_markers([], _N, Acc) ->
    lists:reverse(Acc);
split_markers([[]|Qs], N, Acc) ->
    split_markers(Qs, N, Acc);
split_markers([Opt|Qs], N, Acc) ->
    split_markers(Qs, N+1, [split_marker(Opt, N)|Acc]).

split_marker(<<$*,Line/binary>>, N) ->
    split_marker_1(true, z_string:trim(Line), N);
split_marker(Line, N) ->
    split_marker_1(false, Line, N).

split_marker_1(IsCorrect, Line, N) ->
    case split_kv(Line) of
        [Value,Option] ->
            [
                {value, Value},
                {option, Option},
                {is_correct, IsCorrect}
            ];
        [Option] ->
            [
                {value, z_convert:to_binary(N)},
                {option, Option},
                {is_correct, IsCorrect}
            ]
    end.

split_kv(Line) ->
    split_kv(Line, <<>>).

split_kv(<<>>, Acc) -> [Acc];
split_kv(<<"&#", Rest/binary>>, Acc) -> split_kv(Rest, <<Acc/binary, "&#">>);
split_kv(<<"#", Rest/binary>>, Acc) -> [Acc,Rest];
split_kv(<<C/utf8, Rest/binary>>, Acc) -> split_kv(Rest, <<Acc/binary, C/utf8>>).
