%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2018 Marc Worrell
%% @doc Simplified parsing of lists containing strings, integers, names or sub-lists.

-module(search_parse_list).

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
    {S, Rest1} = parse_string(Rest, <<>>),
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
parse_string(<<>>, S) ->
    {S, <<>>};
parse_string(<<$", Rest/binary>>, S) ->
    {S, Rest};
parse_string(<<$\\, C/utf8, Rest/binary>>, S) ->
    parse_string(Rest, <<S/binary, (map_escape(C))/binary>>);
parse_string(<<C/utf8, Rest/binary>>, S) ->
    parse_string(Rest, <<S/binary, C/utf8>>).

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
