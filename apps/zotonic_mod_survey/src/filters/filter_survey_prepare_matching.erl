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

-module(filter_survey_prepare_matching).

-export([
    survey_prepare_matching/2,
    split_markers/1
]).

survey_prepare_matching(Blk, Context) ->
    Matching = z_trans:lookup_fallback(
                    maps:get(<<"matching">>, Blk, <<>>),
                    Context),
    Pairs = maybe_randomize(
                z_convert:to_bool(maps:get(<<"is_random">>, Blk, false)),
                [ split_option(Line) || Line <- split_lines(Matching) ]),
    {Qs,As} = lists:unzip(Pairs),
    Qs1 = split_markers(Qs),
    As1 = split_markers(As),
    case z_convert:to_bool(maps:get(<<"is_test">>, Blk, false)) of
        true ->
            #{
                <<"is_test">> => true,
                <<"is_test_direct">> => z_convert:to_bool(maps:get(<<"is_test_direct">>, Blk, false)),
                <<"items">> => Qs1,
                <<"options">> => z_utils:randomize(As1)
            };
        false ->
            #{
                <<"is_test">> => false,
                <<"items">> => Qs1,
                <<"options">> => z_utils:randomize(As1)
            }
    end.

maybe_randomize(false, List) -> List;
maybe_randomize(true, List) -> z_utils:randomize(List).

split_lines(Text) ->
    Options = binary:split(z_string:trim(Text), <<"\n">>, [ global ]),
    [ z_string:trim(Option) || Option <- Options ].

split_option(Option) ->
    case binary:split(Option, <<"=">>) of
        [ Q, M ] ->
            {z_string:trim(Q), z_string:trim(M)};
        [ Q ] ->
            {z_string:trim(Q), <<>>}
    end.

split_markers(Qs) ->
    split_markers(Qs, 1, []).

split_markers([], _N, Acc) ->
    lists:reverse(Acc);
split_markers([[]|Qs], N, Acc) ->
    split_markers(Qs, N, Acc);
split_markers([Opt|Qs], N, Acc) ->
    split_markers(Qs, N+1, [split_marker(Opt, N)|Acc]).

split_marker(X, N) ->
    case split_kv(z_convert:to_binary(X)) of
        [Opt] -> {z_convert:to_binary(N), Opt};
        [Val,Opt] -> {Val,Opt}
    end.

split_kv(Line) ->
    split_kv(Line, <<>>).

split_kv(<<>>, Acc) -> [Acc];
split_kv(<<"&#", Rest/binary>>, Acc) -> split_kv(Rest, <<Acc/binary, "&#">>);
split_kv(<<"#", Rest/binary>>, Acc) -> [Acc,Rest];
split_kv(<<C/utf8, Rest/binary>>, Acc) -> split_kv(Rest, <<Acc/binary, C/utf8>>).
