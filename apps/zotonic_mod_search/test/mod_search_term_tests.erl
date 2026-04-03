-module(mod_search_term_tests).
-moduledoc("
EUnit tests for search term/query map normalization.
").

-include_lib("eunit/include/eunit.hrl").
props_map1_test() ->
    T = #{
        <<"page">> => 1,
        <<"pagelen">> => 20,
        <<"text">> => <<"test">>,
        <<"anyof">> => [
            #{ <<"hasobject">> => [ 100 ] },
            #{ <<"hasobject">> => [ 101, 102 ] }
        ]
    },
    M = #{
        <<"options">> => #{},
        <<"page">> => 1,
        <<"pagelen">> => 20,
        <<"q">> => [
            #{ <<"term">> => <<"text">>, <<"value">> => <<"test">> },
            #{
                <<"operator">> => <<"anyof">>,
                <<"terms">> => [
                    #{ <<"term">> => <<"hasobject">>, <<"value">> => [ 100 ] },
                    #{ <<"term">> => <<"hasobject">>, <<"value">> => [ 101, 102 ]}
                ]
            }
        ]
    },
    ?assertEqual(M, z_search_props:from_map(T)),
    ok.

props_map2_test() ->
    T = #{
        <<"page">> => 1,
        <<"pagelen">> => 20,
        <<"text">> => <<"test">>,
        <<"anyof">> => [
            #{ <<"term">> => <<"hasobject">>, <<"value">> => [ 100 ] },
            #{ <<"hasobject">> => [ 101, 102 ] }
        ]
    },
    M = #{
        <<"options">> => #{},
        <<"page">> => 1,
        <<"pagelen">> => 20,
        <<"q">> => [
            #{ <<"term">> => <<"text">>, <<"value">> => <<"test">> },
            #{
                <<"operator">> => <<"anyof">>,
                <<"terms">> => [
                    #{ <<"term">> => <<"hasobject">>, <<"value">> => [ 100 ] },
                    #{ <<"term">> => <<"hasobject">>, <<"value">> => [ 101, 102 ]}
                ]
            }
        ]
    },
    ?assertEqual(M, z_search_props:from_map(T)),
    ok.

props_map3_test() ->
    T = #{
        <<"options">> => #{
            <<"foo">> => <<"bar">>
        },
        <<"page">> => 1,
        <<"pagelen">> => 20,
        <<"q">> => [
            #{ <<"term">> => <<"text">>, <<"value">> => <<"test">> },
            #{
                <<"operator">> => <<"anyof">>,
                <<"terms">> => [
                    #{ <<"term">> => <<"hasobject">>, <<"value">> => [ 100 ] },
                    #{ <<"hasobject">> => [ 101, 102 ] }
                ]
            }
        ]
    },
    M = #{
        <<"options">> => #{
            <<"foo">> => <<"bar">>
        },
        <<"page">> => 1,
        <<"pagelen">> => 20,
        <<"q">> => [
            #{ <<"term">> => <<"text">>, <<"value">> => <<"test">> },
            #{
                <<"operator">> => <<"anyof">>,
                <<"terms">> => [
                    #{ <<"term">> => <<"hasobject">>, <<"value">> => [ 100 ] },
                    #{ <<"term">> => <<"hasobject">>, <<"value">> => [ 101, 102 ]}
                ]
            }
        ]
    },
    ?assertEqual(M, z_search_props:from_map(T)),
    ok.

filters_to_nested_terms_nested_test() ->
    Filters = {'and', [
        {'or', [
            [<<"pivot.title">>, <<"Nested Filter Alpha">>],
            [<<"pivot.title">>, <<"Nested Filter Beta">>]
        ]},
        [<<"is_featured">>, true]
    ]},
    Expected = #{
        <<"operator">> => <<"allof">>,
        <<"terms">> => [
            #{
                <<"operator">> => <<"anyof">>,
                <<"terms">> => [
                    #{
                        <<"term">> => <<"pivot:title">>,
                        <<"operator">> => <<"=">>,
                        <<"value">> => <<"Nested Filter Alpha">>
                    },
                    #{
                        <<"term">> => <<"pivot:title">>,
                        <<"operator">> => <<"=">>,
                        <<"value">> => <<"Nested Filter Beta">>
                    }
                ]
            },
            #{
                <<"term">> => <<"filter:is_featured">>,
                <<"operator">> => <<"=">>,
                <<"value">> => true
            }
        ]
    },
    ?assertEqual(Expected, search_query:filters_to_nested_terms(Filters)),
    ok.
