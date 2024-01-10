-module(mod_translation_translate_rsc_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").


remove_language_map_test() ->
    Rsc = #{
        <<"language">> => [ en, nl, de ],
        <<"title">> => #trans{ tr = [ {en, <<"t">>}, {nl, <<>>}, {de, <<>>} ]},
        <<"translation_status">> => #{
            <<"nl">> => <<"1">>
        },
        <<"blocks">> => [
            #{
                <<"name">> => <<"a">>,
                <<"whatever">> => #trans{ tr = [ {en, <<"a">>}, {nl, <<>>}, {de, <<>>} ]}
            }
        ]
    },
    Rsc1 = #{
        <<"language">> => [ en ],
        <<"title">> => #trans{ tr = [ {en, <<"t">>} ]},
        <<"translation_status">> => #{},
        <<"blocks">> => [
            #{
                <<"name">> => <<"a">>,
                <<"whatever">> => #trans{ tr = [ {en, <<"a">>} ]}
            }
        ]
    },
    {ok, Out} = translation_translate_rsc:remove_translation_map(Rsc, [nl, de]),
    ?assertEqual(Rsc1, Out).

add_language_map_test() ->
    Context = z_acl:sudo( z_context:new(zotonic_site_testsandbox) ),
    Rsc = #{
        <<"language">> => [ en ],
        <<"translation_status">> => #{},
        <<"title">> => #trans{ tr = [ {en, <<"Yes">>} ]},
        <<"blocks">> => [
            #{
                <<"name">> => <<"a">>,
                <<"whatever">> => #trans{ tr = [ {en, <<"No">>} ]}
            }
        ]
    },
    Rsc1 = #{
        <<"language">> => [ en, nl ],
        <<"translation_status">> => #{
            <<"nl">> => <<"1">>
        },
        <<"title">> => #trans{ tr = [ {en, <<"Yes">>}, {nl, <<"Ja">>} ]},
        <<"blocks">> => [
            #{
                <<"name">> => <<"a">>,
                <<"whatever">> => #trans{ tr = [ {en, <<"No">>}, {nl, <<"Nee">>} ]}
            }
        ]
    },
    {ok, Out} = translation_translate_rsc:add_translation_map(Rsc, en, nl, false, Context),
    ?assertEqual(Rsc1, Out).

add_language_map_overwrite_test() ->
    Context = z_acl:sudo( z_context:new(zotonic_site_testsandbox) ),
    Rsc1 = #{
        <<"language">> => [ en, nl ],
        <<"translation_status">> => #{},
        <<"title">> => #trans{ tr = [ {en, <<"Yes">>}, {nl, <<"AAA">>} ]},
        <<"blocks">> => [
            #{
                <<"name">> => <<"a">>,
                <<"whatever">> => #trans{ tr = [ {en, <<"No">>}, {nl, <<"BBB">>} ]}
            }
        ]
    },
    Rsc1 = #{
        <<"language">> => [ en, nl ],
        <<"translation_status">> => #{
            <<"nl">> => <<"1">>
        },
        <<"title">> => #trans{ tr = [ {en, <<"Yes">>}, {nl, <<"Ja">>} ]},
        <<"blocks">> => [
            #{
                <<"name">> => <<"a">>,
                <<"whatever">> => #trans{ tr = [ {en, <<"No">>}, {nl, <<"Nee">>} ]}
            }
        ]
    },
    {ok, Out} = translation_translate_rsc:add_translation_map(Rsc, en, nl, true, Context),
    ?assertEqual(Rsc1, Out).
