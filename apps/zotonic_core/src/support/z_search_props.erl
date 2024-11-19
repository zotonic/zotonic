%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2023 Marc Worrell
%% @doc Map query args to query maps and vice versa. Special backwards compatible handling
%% for repeating terms with the same name.
%% @end

%% Copyright 2023 Marc Worrell
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

-module(z_search_props).

-export([
    from_text/1,
    from_list/1,
    from_qargs/1,
    from_map/1
]).

% -type termvalue() :: #{
%         <<"term">> := binary(),
%         <<"value">> => term(),
%         <<"operator">> => binary()
%     }.
%
% -type termoperator() :: #{
%         <<"operator">> => binary(),
%         <<"terms">> := list( queryterm() )
%     }.
%
% -type queryterm() :: termvalue() | termoperator().
%
% -type querymap() :: #{
%         <<"q">> := list( queryterm() ),
%         <<"page">> => pos_integer(),
%         <<"pagelen">> => pos_integer(),
%         <<"options">> => map()
%     }.

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Translate a stored query text to a query term map.
-spec from_text(Text) -> Query when
    Text :: binary() | string() | undefined,
    Query :: map().
from_text(undefined) ->
    #{
        <<"q">> => [],
        <<"page">> => 1,
        <<"pagelen">> => undefined,
        <<"options">> => #{}
    };
from_text(Text) ->
    Text1 = z_string:trim(unicode:characters_to_binary(Text, utf8)),
    case Text1 of
        <<"{", _/binary>> ->
            Map = jsx:decode(Text1),
            from_map_1(Map);
        _ ->
            % Pairs of term/value
            TermArgs = parse_query_text(Text1),
            TermArgs1 = lists:filter(
                fun
                    ({<<"page">>, _}) -> false;
                    ({<<"pagelen">>, _}) -> false;
                    ({<<"options">>, _}) -> false;
                    (_) -> true
                end,
                TermArgs),
            Map = maybe_from_zprops(TermArgs1),
            TermList = map_terms(Map),
            #{
                <<"q">> => combine_category_terms(TermList),
                <<"page">> => maybe_page(TermArgs),
                <<"pagelen">> => maybe_pagelen(TermArgs),
                <<"options">> => options(TermArgs)
            }
    end.

%% @doc Parses a query text. Every line is an argument; of which the first
%% '=' separates argument key from argument value.
-spec parse_query_text( binary() ) -> list( {binary(), term()} ).
parse_query_text(Text) when is_binary(Text) ->
    Lines = binary:split(Text, <<"\n">>, [global]),
    KVs = [ split_arg(z_string:trim(Line)) || Line <- Lines],
    Args = [ {K, V} || {K,V} <- KVs, K =/= <<>> ],
    Args1 = [ {K,V} || {K,V} <- Args, K =/= undefined ],
    maybe_map_keys(Args1).

split_arg(<<>>) ->
    {<<>>, <<>>};
split_arg(B) ->
    case binary:split(B, <<"=">>) of
        [K,V] -> {z_string:trim(K), z_string:trim(V)};
        [K] -> {z_string:trim(K), <<"true">>}
    end.



%% @doc Translate the query arguments to a query term map. Drops
%% query terms with empty values, as unselected or empty inputs in
%% search forms can post empty values.
-spec from_list(Args) -> Query when
    Args :: list(KeyValue),
    KeyValue :: {Key, Value}
              | Key
              | list(),     % [Key,Value] or [Key]
    Key :: binary() | atom(),
    Value :: binary() | number() | boolean() | list(),
    Query :: map().
from_list(Args0) when is_list(Args0) ->
    Args = maybe_map_keys(Args0),
    Args1 = z_utils:prop_delete(<<"pagelen">>,
                                z_utils:prop_delete(<<"page">>, Args)),
    TermArgs = lists:filter(
            fun({K, _V}) -> not z_context:is_zotonic_arg(K) end,
            Args1),
    TermArgs1 = lists:filter(
            fun
                ({_, <<>>}) -> false;
                ({_, []}) -> false;
                ({_, undefined}) -> false;
                (_) -> true
            end,
            TermArgs),
    MapOrList = maybe_from_zprops(TermArgs1),
    TermList = map_terms(MapOrList),
    #{
        <<"q">> => combine_category_terms(TermList),
        <<"page">> => maybe_page(Args),
        <<"pagelen">> => maybe_pagelen(Args),
        <<"options">> => options(Args)
    }.


%% @doc Translate the query arguments to a query term map.
-spec from_qargs(QArgs) -> Query when
    QArgs :: z:context() | list({Key, Value}),
    Key :: binary() | atom(),
    Value :: binary() | number() | boolean() | list(),
    Query :: map().
from_qargs(#context{} = Context) ->
    QArgs = z_context:get_q_all_noz(Context),
    from_qargs(QArgs);
from_qargs(QArgs0) when is_list(QArgs0) ->
    QArgs = maybe_map_keys(QArgs0),
    TermArgs = lists:filtermap(
            fun
                ({<<"q.">>, _} = Arg) -> {true, Arg};
                ({<<"q[">>, _} = Arg) -> {true, Arg};
                ({<<"qargs">>, _}) -> false;
                ({<<"q", _Term/binary>>, <<>>}) -> false;
                ({<<"qs">>, V}) -> {true, {<<"text">>, V}};
                ({<<"q", Term/binary>>, V}) -> {true, {Term, V}};
                (_) -> false
            end,
            QArgs),
    from_list(TermArgs).

%% @doc Translate a map with query term keys to a query term map.
-spec from_map(TermMap) -> Query when
    TermMap :: map(),
    Query :: map().
from_map(M) ->
    from_map_1(map_binary_keys(M)).

from_map_1(#{ <<"q">> := Terms } = M) when is_list(Terms) ->
    Terms1 = lists:filtermap(fun map_term/1, Terms),
    M#{
        <<"q">> => Terms1
    };
from_map_1(#{ <<"q">> := Terms } = M) when is_map(Terms) ->
    Terms1 = map_terms(Terms),
    M#{
        <<"q">> => Terms1
    };
from_map_1(Map) when is_map(Map) ->
    Map1 = maps:without([ <<"page">>, <<"pagelen">>, <<"options">> ], Map),
    TermList = map_terms(Map1),
    #{
        <<"q">> => combine_category_terms(TermList),
        <<"page">> => maybe_page(Map),
        <<"pagelen">> => maybe_pagelen(Map),
        <<"options">> => options(Map)
    }.

map_binary_keys(M) when is_map(M) ->
    maps:fold(
        fun(K, V, Acc) ->
            K1 = z_convert:to_binary(K),
            V1 = map_binary_keys(V),
            Acc#{ K1 => V1 }
        end,
        #{},
        M);
map_binary_keys(M) when is_list(M) ->
    lists:map(fun map_binary_keys/1, M);
map_binary_keys(V) ->
    V.

maybe_map_keys(Qs) ->
    lists:map(
        fun
            ({K, V}) -> maybe_rename_arg({K, V});
            ([K, V]) -> maybe_rename_arg({K, V});
            ([K]) -> maybe_rename_arg({K, true});
            (K) when is_atom(K); is_binary(K) -> maybe_rename_arg({K, true})
        end,
        Qs).


maybe_from_zprops(TermArgs) ->
    case lists:any(
        fun({K, _}) ->
            binary:match(K, <<".">>) =/= nomatch
            orelse binary:match(K, <<"[">>) =/= nomatch
        end,
        TermArgs)
    of
        true ->
            % Structured query args - reconstitute the
            % hierarchical structure.
            {ok, Map} = z_props:from_qs(TermArgs),
            Map;
        false ->
            % Just a simple list, might have repeating args
            % with the same name. Keep this as a list of terms.
            TermArgs
    end.

maybe_rename_arg({K, V}) when is_atom(K) ->
    maybe_rename_arg({atom_to_binary(K, utf8), V});
maybe_rename_arg({K, V}) when is_binary(K) ->
    [ K1 | _ ] = binary:split(K, <<"~">>),
    case is_filter_arg(K1) of
        true ->
            Parts = binary:split(K1, <<".">>, [ global ]),
            K2 = case lists:last(Parts) of
                Special when Special =:= <<"value">>; Special =:= <<"operator">> ->
                    K3 = lists:reverse(tl(lists:reverse(Parts))),
                    iolist_to_binary([ lists:join($:, K3), $., Special]);
                _ ->
                    iolist_to_binary(lists:join($:, Parts))
            end,
            {K2, V};
        false ->
            {K1, V}
    end.

is_filter_arg(<<"filter.", _/binary>>) -> true;
is_filter_arg(<<"pivot.", _/binary>>) -> true;
is_filter_arg(<<"facet.", _/binary>>) -> true;
is_filter_arg(_) -> false.

maybe_page(#{ <<"page">> := Page }) ->
    try
        max(1, z_convert:to_integer(Page))
    catch
        _:_ -> 1
    end;
maybe_page(#{}) ->
    1;
maybe_page(QArgs) when is_list(QArgs) ->
    case proplists:get_value(<<"page">>, QArgs) of
        undefined -> 1;
        <<>> -> 1;
        Page ->
            try
                max(1, z_convert:to_integer(Page))
            catch
                _:_ -> 1
            end
    end.

maybe_pagelen(#{ <<"pagelen">> := Pagelen }) ->
    try
        max(1, z_convert:to_integer(Pagelen))
    catch
        _:_ -> undefined
    end;
maybe_pagelen(#{}) ->
    undefined;
maybe_pagelen(QArgs) when is_list(QArgs) ->
    case proplists:get_value(<<"pagelen">>, QArgs) of
        undefined -> undefined;
        <<>> -> undefined;
        Pagelen ->
            try
                max(1, z_convert:to_integer(Pagelen))
            catch
                _:_ -> undefined
            end
    end.

options(#{ <<"options">> := Opts }) when is_map(Opts) ->
    maps:fold(
        fun(K, V, Acc) ->
            K1 = z_convert:to_binary(K),
            Acc#{ K1 => V }
        end,
        #{},
        Opts);
options(#{ <<"options">> := Opts }) when is_binary(Opts) ->
    OptsList = z_parse_list:parse(Opts),
    OptsList1 = [ {z_convert:to_binary(K), true} || K <- OptsList ],
    maps:from_list(OptsList1);
options(#{ <<"options">> := Opts }) when is_list(Opts) ->
    OptsList = lists:map(
        fun
            ({K, V}) ->
                {z_convert:to_binary(K), V};
            (K) when is_binary(K); is_atom(K) ->
                {z_convert:to_binary(K), true}
        end,
        Opts),
    maps:from_list(OptsList);
options(#{}) ->
    #{};
options(QArgs) when is_list(QArgs) ->
    Terms = lists:filtermap(
            fun
                ({<<"options.", _/binary>>, _} = Arg) -> {true, Arg};
                ({<<"options">>, _} = Arg) -> {true, Arg};
                (_) -> false
            end,
            QArgs),
    {ok, Opts} = z_props:from_qs(Terms),
    options(Opts).


map_terms(Terms) ->
    lists:flatten(map_terms_1(Terms)).

map_terms_1(Map) when is_map(Map) ->
    maps:fold(
        fun(K, V, Acc) ->
            case map_term({K, V}) of
                {true, []} ->
                    Acc;
                {true, TermOrTerms} ->
                    [ TermOrTerms | Acc ];
                false ->
                    Acc
            end
        end,
        [],
        Map);
map_terms_1(List) when is_list(List) ->
    lists:filtermap(fun map_term/1, List).

map_term({K, V}) when is_atom(K) ->
    map_term({atom_to_binary(K, utf8), V});
map_term({K, V}) when
    K =:= <<"anyof">>;
    K =:= <<"allof">>;
    K =:= <<"noneof">> ->
    {true, #{
        <<"operator">> => K,
        <<"terms">> => combine_category_terms(map_terms(V))
    }};
map_term({K, #{ <<"all">> := All, <<"any">> := Any }}) ->
    All1 = filter_empty( lists:map(fun(V) -> {K, V} end, All) ),
    Terms = case lists:filter(fun z_utils:is_empty/1, Any) of
        [] -> All1;
        Any1 -> {true, [ {K, Any1} | All1 ]}
    end,
    {true, lists:map(fun map_term/1, Terms)};
map_term({K, #{ <<"all">> := All }}) ->
    Terms = filter_empty( lists:map(fun(V) -> {K, V} end, All) ),
    {true, lists:map(fun map_term/1, Terms)};
map_term({K, #{ <<"any">> := Any }}) ->
    Terms = case lists:filter(fun z_utils:is_empty/1, Any) of
        [] -> [];
        Any1 -> {K, Any1}
    end,
    {true, lists:map(fun map_term/1, Terms)};
map_term({K, V}) ->
    {K1, V1} = maybe_rename_arg({K, V}),
    case V1 of
        #{
            <<"value">> := Vx,
            <<"operator">> := Op
        } ->
            {true, #{
                <<"term">> => K1,
                <<"value">> => map_value(K1, Vx),
                <<"operator">> => Op
            }};
        _ ->
            {true, #{
                <<"term">> => K1,
                <<"value">> => map_value(K1, V1)
            }}
    end;
map_term(#{ <<"terms">> := Terms } = Term) when is_list(Terms) ->
    Terms1 = lists:filtermap(fun map_term/1, Terms),
    {true, Term#{ <<"terms">> => Terms1 }};
map_term(#{ <<"term">> := K } = Term) ->
    {K1, _} = maybe_rename_arg({K, undefined}),
    {true, Term#{ <<"term">> => K1 }};
map_term(Term) ->
    ?LOG_INFO(#{
        in => zotonic_mod_search,
        text => <<"Dropping unknown query term">>,
        result => error,
        reason => unknown_query_term,
        term => Term
    }),
    false.

map_value(<<"text">>, V) ->
    z_string:trim(z_convert:to_binary(V));
map_value(<<"is_", _/binary>>, <<>>) ->
    undefined;
map_value(<<"is_published", _/binary>>, All) when All =:= <<"all">>; All =:= all ->
    <<"all">>;
map_value(<<"is_", _/binary>>, V) when is_binary(V) ->
    z_convert:to_bool(V);
map_value(_K, V) when is_binary(V) ->
    case binary:match(V, <<",">>) =:= nomatch
        andalso binary:match(V, <<"[">>) =:= nomatch
    of
        true -> V;
        false -> z_parse_list:parse(V)
    end;
map_value(_K, V) ->
    V.

filter_empty(Q) when is_list(Q) ->
    lists:filter(fun({_, X}) -> not(empty_term(X)) end, Q).

empty_term([]) -> true;
empty_term(<<>>) -> true;
empty_term(undefined) -> true;
empty_term(null) -> true;
empty_term([X, _]) -> empty_term(X);
empty_term(_) -> false.

%% @doc Combine keys like cat, cat_exact and cat_exclude terms in a single term.
combine_category_terms(Terms) ->
    lists:foldl(
        fun(K, Acc) ->
            combine_terms(K, Acc)
        end,
        Terms,
        [
            <<"cat">>,
            <<"cat_exclude">>,
            <<"cat_exact">>,
            <<"id">>,
            <<"id_exclude">>
        ]).

combine_terms(K, Terms) ->
    case lists:partition(
        fun
            (#{ <<"term">> := T }) -> T =:= K;
            (_) -> false
        end,
        Terms)
    of
        {[], _} ->
            Terms;
        {[_], _} ->
            Terms;
        {Ks, Other} ->
            Values = lists:flatten([ V || #{ <<"value">> := V } <- Ks ]),
            Term = #{
                <<"term">> => K,
                <<"value">> => Values
            },
            [ Term | Other ]
    end.

