-module(mod_search_term_tests).

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