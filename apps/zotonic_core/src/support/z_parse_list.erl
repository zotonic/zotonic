%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2018-2023 Marc Worrell
%% @doc Simplified parsing of lists containing strings, integers, names or sub-lists.
%% @end

%% Copyright 2018-2023 Marc Worrell
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

-module(z_parse_list).

-export([
    parse/1
]).

parse(undefined) ->
    [];
parse(<<>>) ->
    [];
parse(Bin) when is_binary(Bin) ->
    {List, _Rest} = parse_term(Bin, []),
    lists:reverse(List);
parse(L) when is_list(L) ->
    L;
parse(V) ->
    V.

parse_term(<<>>, List) ->
    {List, <<>>};
parse_term(<<$,, Rest/binary>>, List) ->
    parse_term(Rest, List);
parse_term(<<$[, Rest/binary>>, List) ->
    {SubList, Rest1} = parse_term(Rest, []),
    parse_term(Rest1, [  lists:reverse(SubList)| List ]);
parse_term(<<$], Rest/binary>>, List) ->
    {List, Rest};
parse_term(<<$", Rest/binary>>, List) ->
    {S, Rest1} = parse_string(Rest, $", <<>>),
    parse_term(Rest1, [ S | List ]);
parse_term(<<$', Rest/binary>>, List) ->
    {S, Rest1} = parse_string(Rest, $', <<>>),
    parse_term(Rest1, [ S | List ]);
parse_term(<<C, Rest/binary>>, List) when C =< 32 ->
    parse_term(Rest, List);
parse_term(Bin, List) ->
    {V, Rest} = parse_value(Bin, <<>>),
    parse_term(Rest, [ maybe_map_value(V) | List ]).

% Simple value, just drop all spaces and concatenate other characters till one of "[]," is found.
parse_value(<<>>, V) -> {V, <<>>};
parse_value(<<C, Rest/binary>>, V) when C =< 32 -> parse_value(Rest, V);
parse_value(<<$,, _/binary>> = Rest, V) -> {V, Rest};
parse_value(<<$], _/binary>> = Rest, V) -> {V, Rest};
parse_value(<<$[, _/binary>> = Rest, V) -> {V, Rest};
parse_value(<<C/utf8, Rest/binary>>, V) -> parse_value(Rest, <<V/binary, C/utf8>>).

% String value, keep spaces.
parse_string(<<>>, _Q, S) ->
    {S, <<>>};
parse_string(<<$", Rest/binary>>, $", S) ->
    {S, Rest};
parse_string(<<$', Rest/binary>>, $', S) ->
    {S, Rest};
parse_string(<<$\\, C/utf8, Rest/binary>>, Q, S) ->
    parse_string(Rest, Q, <<S/binary, (map_escape(C))/binary>>);
parse_string(<<C/utf8, Rest/binary>>, Q, S) ->
    parse_string(Rest, Q, <<S/binary, C/utf8>>).

map_escape($n) -> <<10>>;
map_escape($r) -> <<13>>;
map_escape($t) -> <<9>>;
map_escape(C) -> <<C/utf8>>.

% Simple values can be mapped
maybe_map_value(<<"true">>) -> true;
maybe_map_value(<<"false">>) -> false;
maybe_map_value(<<"undefined">>) -> undefined;
maybe_map_value(<<"null">>) -> undefined;
maybe_map_value(<<Sign, V/binary>> = B) when Sign =:= $-; Sign =:= $+ ->
    case z_utils:only_digits(V) of
        true -> binary_to_integer(B);
        false -> B
    end;
maybe_map_value(V) ->
    case z_utils:only_digits(V) of
        true -> binary_to_integer(V);
        false -> V
    end.
