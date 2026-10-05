%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Test SPARQL request validation, pagination and result serialization.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(z_sparql_protocol_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

request_test() ->
    Query = <<"SELECT ?s WHERE { ?s zotonic:id ?id }">>,
    ?assertEqual({ok, #{<<"query">> => Query, <<"args">> => #{}}},
        z_sparql_protocol:normalize(#{<<"query">> => Query})),
    ?assertEqual({ok, #{<<"query">> => Query, <<"args">> => #{<<"id">> => 42}}},
        z_sparql_protocol:normalize(#{<<"query">> => Query, <<"args">> => <<"{\"id\":42}">>})),
    lists:foreach(fun(Payload) ->
        ?assertMatch({error, _}, z_sparql_protocol:normalize(Payload))
    end, [null, [], #{}, #{<<"query">> => []}, #{<<"query">> => <<>>},
        #{<<"query">> => Query, <<"args">> => <<"[1]">>},
        #{<<"query">> => Query, <<"args">> => <<"{">>},
        #{<<"query">> => Query, <<"default-graph-uri">> => <<"https://example.org/">>}]).

pagination_test() ->
    Plan = #{limit => 7, offset => 3},
    ?assertEqual({ok, {4, 7}}, z_sparql_protocol:pagination(#{}, Plan, 20)),
    ?assertEqual({ok, {1, 0}}, z_sparql_protocol:pagination(#{}, #{limit => 0, offset => undefined}, 20)),
    ?assertEqual({ok, {4, 20}}, z_sparql_protocol:pagination(#{}, Plan#{limit => undefined}, 20)),
    ?assertEqual({ok, {21, 20}}, z_sparql_protocol:pagination(#{<<"page">> => 2}, Plan, 20)),
    ?assertEqual({ok, {1, 5}}, z_sparql_protocol:pagination(#{<<"pagelen">> => 5}, Plan, 20)),
    ?assertEqual({ok, {11, 5}}, z_sparql_protocol:pagination(
        #{<<"page">> => <<"3">>, <<"pagelen">> => <<"5">>}, Plan, 20)),
    lists:foreach(fun(Empty) ->
        ?assertEqual({ok, {1, 20}}, z_sparql_protocol:pagination(
            #{<<"page">> => Empty, <<"pagelen">> => Empty}, Plan, 20)),
        ?assertEqual({ok, {1, 5}}, z_sparql_protocol:pagination(
            #{<<"page">> => Empty, <<"pagelen">> => 5}, Plan, 20)),
        ?assertEqual({ok, {21, 20}}, z_sparql_protocol:pagination(
            #{<<"page">> => 2, <<"pagelen">> => Empty}, Plan, 20))
    end, [<<>>, null, undefined]),
    lists:foreach(fun(Value) ->
        lists:foreach(fun(Key) ->
            ?assertEqual({error, invalid_pagination}, z_sparql_protocol:pagination(#{Key => Value}, Plan, 20))
        end, [<<"page">>, <<"pagelen">>])
    end, [0, -1, <<"x">>, [], 1.5]).

results_test() ->
    Context = #context{},
    Vars = [<<"s">>, <<"label">>, <<"n">>, <<"missing">>],
    Xsd = <<"http://www.w3.org/2001/XMLSchema#integer">>,
    Row = {<<"https://example.org/item">>, <<"iri">>, undefined, undefined,
        <<"Example">>, <<"literal">>, undefined, <<"en">>,
        42, <<"literal">>, Xsd, undefined,
        undefined, <<"literal">>, Xsd, undefined},
    ?assertEqual(#{<<"head">> => #{<<"vars">> => Vars},
        <<"results">> => #{<<"bindings">> => [#{
            <<"s">> => #{<<"type">> => <<"uri">>, <<"value">> => <<"https://example.org/item">>},
            <<"label">> => #{<<"type">> => <<"literal">>, <<"value">> => <<"Example">>, <<"xml:lang">> => <<"en">>},
            <<"n">> => #{<<"type">> => <<"literal">>, <<"value">> => <<"42">>, <<"datatype">> => Xsd}
        }]}}, z_sparql_results:document(Vars, [Row], Context)),
    ?assertMatch(#{<<"head">> := #{<<"vars">> := Vars}, <<"results">> := #{<<"bindings">> := []}},
        z_sparql_results:document(Vars, [], Context)),
    ?assertEqual(z_sparql_results:binding(#{<<"a">> => 1}, <<"bnode">>, undefined, undefined, Context),
        z_sparql_results:binding(#{<<"a">> => 1}, <<"bnode">>, undefined, undefined, Context)),
    ?assertThrow({error, unsupported_result_term}, z_sparql_results:binding([], undefined, undefined, undefined, Context)).

numeric_lexical_test() ->
    Context = #context{},
    lists:foreach(fun({Value, Type, Expected}) ->
        ?assertMatch(#{<<"value">> := Expected}, z_sparql_results:binding(
            Value, <<"literal">>, <<"http://www.w3.org/2001/XMLSchema#", Type/binary>>, undefined, Context))
    end, [{1.0, <<"integer">>, <<"1">>},
          {1.0e-7, <<"decimal">>, <<"0.00000010">>},
          {-1.5e-7, <<"decimal">>, <<"-0.00000015">>},
          {1.5e20, <<"decimal">>, <<"150000000000000000000">>}]).
