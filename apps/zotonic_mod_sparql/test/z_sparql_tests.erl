-module(z_sparql_tests).

-include_lib("eunit/include/eunit.hrl").

parse_list_test() ->
    ?assertMatch(
        {ok, {query, [], {ask, [], {group, []}, _}}},
        z_sparql:parse("ASK WHERE {}")),
    ?assertMatch(
        {ok, {query, [], {select, default, all, [], {group, []}, _}}},
        z_sparql:parse("SELECT * WHERE {}")).

parse_select_test() ->
    Filename = filename:join([filename:dirname(?FILE), "data", "select.sparql"]),
    {ok, Query} = file:read_file(Filename),
    ?assertMatch(
        {ok,
            {query,
                [{prefix, <<"foaf:">>, <<"http://xmlns.com/foaf/0.1/">>}],
                {select,
                    default,
                    [{var, <<"name">>}],
                    [],
                    {group,
                        [
                            {triple_pattern,
                                {subject,
                                    {var, <<"x">>},
                                    [
                                        {predicate,
                                            {pname, <<"foaf:name">>},
                                            [{var, <<"name">>}]}
                                    ]}}
                        ]},
                    _}}},
        z_sparql:parse(Query)).

scanner_error_test() ->
    ?assertMatch(
        {error, <<_/binary>>},
        z_sparql:parse(<<"SELECT @ WHERE {}">>)).

parser_error_test() ->
    ?assertMatch(
        {error, {_Location, z_sparql_parser, _Message}},
        z_sparql:parse(<<"SELECT WHERE {}">>)).
