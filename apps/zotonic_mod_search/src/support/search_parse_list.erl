-module(search_parse_list).

-export([
    parse/1
]).

parse(undefined) ->
    [];
parse(<<>>) ->
    [];
parse(Bin) when is_binary(Bin) ->
    {List,_Rest} = parse_term(Bin, <<>>, []),
    lists:reverse(List);
parse(L) when is_list(L) ->
    L;
parse(V) ->
    V.

parse_term(<<C, Rest/binary>>, Term, List) when C =< 32 ->
    parse_term(Rest, Term, List);
parse_term(<<$,, Rest/binary>>, Term, List) ->
    parse_term(Rest, <<>>, [Term|List]);
parse_term(<<$[, Rest/binary>>, <<>>, List) ->
    {SubList,Rest1} = parse_term(Rest, <<>>, []),
    parse_term(Rest1, <<>>, [lists:reverse(SubList)|List]);
parse_term(<<$], Rest/binary>>, <<>>, List) ->
    {List, Rest};
parse_term(<<$], Rest/binary>>, Term, List) ->
    {[Term|List], Rest};
parse_term(<<C/utf8, Rest/binary>>, Term, List) ->
    parse_term(Rest, <<Term/binary, C/utf8>>, List);
parse_term(<<>>, <<>>, List) ->
    {List, <<>>};
parse_term(<<>>, Term, List) ->
    {[Term|List], <<>>}.




