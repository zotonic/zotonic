-module(mod_translation_translate_rsc_tests).
-moduledoc("
EUnit tests for resource translation map transformations.
").

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

add_language_map_bin_test() ->
    Context = z_acl:sudo( z_context:new(zotonic_site_testsandbox) ),
    Rsc = #{
        <<"language">> => [ en ],
        <<"translation_status">> => #{},
        <<"title">> => <<"Yes">>,
        <<"blocks">> => [
            #{
                <<"name">> => <<"a">>,
                <<"title">> => <<"No">>,
                <<"whatever">> => <<"No">>
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
                <<"title">> => #trans{ tr = [ {en, <<"No">>}, {nl, <<"Nee">>} ]},
                <<"whatever">> => <<"No">>
            }
        ]
    },
    {ok, Out} = translation_translate_rsc:add_translation_map(Rsc, en, nl, false, Context),
    ?assertEqual(Rsc1, Out).

add_language_map_overwrite_test() ->
    Context = z_acl:sudo( z_context:new(zotonic_site_testsandbox) ),
    Rsc = #{
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

%% @doc Empty and missing source values clear destinations only when overwriting.
overwrite_empty_source_test() ->
    overwrite_empty_source(z_acl:sudo(z_context:new(zotonic_site_testsandbox))).

%% @doc Check map transformations and saved resource translations with no service texts.
overwrite_empty_source(Context) ->
    Empty = #trans{tr = [{en, <<>>}, {nl, <<"Old translation">>}]},
    Missing = #trans{tr = [{nl, <<"Old translation">>}]},
    Props = #{
        <<"language">> => [en, nl],
        <<"title">> => Empty,
        <<"summary">> => Missing,
        <<"data_json">> => Empty,
        <<"blocks">> => [#{<<"type">> => <<"text">>, <<"body">> => Empty,
            <<"nested">> => #{<<"summary">> => Missing}}]
    },
    {ok, Preserved} = translation_translate_rsc:add_translation_map(Props, en, nl, false, Context),
    ?assertEqual(Props, Preserved),
    {ok, Cleared} = translation_translate_rsc:add_translation_map(Props, en, nl, true, Context),
    ?assertEqual(#trans{tr = [{en, <<>>}, {nl, <<>>}]}, maps:get(<<"title">>, Cleared)),
    ?assertEqual(#trans{tr = [{nl, <<>>}]}, maps:get(<<"summary">>, Cleared)),
    ?assertEqual(Empty, maps:get(<<"data_json">>, Cleared)),
    [Block] = maps:get(<<"blocks">>, Cleared),
    ?assertEqual(maps:get(<<"title">>, Cleared), maps:get(<<"body">>, Block)),
    ?assertEqual(#{<<"summary">> => maps:get(<<"summary">>, Cleared)}, maps:get(<<"nested">>, Block)),
    {ok, Id} = m_rsc:insert(Props#{<<"category_id">> => text}, Context),
    try
        ok = translation_translate_rsc:add_translation(Id, en, nl, false, true, Context),
        ?assertEqual(Empty, m_rsc:p(Id, title, Context)),
        ok = translation_translate_rsc:add_translation(Id, en, nl, true, true, Context),
        ?assertEqual(undefined, m_rsc:p(Id, title, Context)),
        ?assertEqual(undefined, m_rsc:p(Id, summary, Context)),
        ?assertEqual([Block], m_rsc:p(Id, blocks, Context)),
        ?assertEqual(Empty, m_rsc:p(Id, data_json, Context))
    after
        ok = m_rsc:delete(Id, Context)
    end.

language_rsc_test() ->
    Context = z_acl:sudo( z_context:new(zotonic_site_testsandbox) ),
    Props = #{
        <<"is_published">> => true,
        <<"category_id">> => text,
        <<"language">> => [ en ],
        <<"title">> => #trans{ tr = [ {en, <<"Yes">>} ] },
        <<"body">> => <<"No"/utf8>>
    },
    {ok, Id} = m_rsc:insert(Props, Context),
    ok = translation_translate_rsc:add_translation(Id, en, nl, false, Context),
    Language = m_rsc:p(Id, <<"language">>, Context),
    TranslationStatus = m_rsc:p(Id, <<"translation_status">>, Context),
    Title = m_rsc:p(Id, <<"title">>, Context),
    Body = m_rsc:p(Id, <<"body">>, Context),
    ?assertEqual([en, nl], Language),
    ?assertEqual(#{ <<"nl">> => <<"1">> }, TranslationStatus),
    ?assertEqual(#trans{ tr = [ {en, <<"Yes">>}, {nl, <<"Ja">>} ] }, Title),
    ?assertEqual(#trans{ tr = [ {en, <<"No">>}, {nl, <<"Nee">>} ] }, Body),

    ok = translation_translate_rsc:remove_translation(Id, nl, Context),
    Language1 = m_rsc:p(Id, <<"language">>, Context),
    TranslationStatus1 = m_rsc:p(Id, <<"translation_status">>, #{}, Context),
    Title1 = m_rsc:p(Id, <<"title">>, Context),
    Body1 = m_rsc:p(Id, <<"body">>, Context),
    ?assertEqual([en], Language1),
    ?assertEqual(#{}, TranslationStatus1),
    ?assertEqual(#trans{ tr = [ {en, <<"Yes">>} ] }, Title1),
    ?assertEqual(#trans{ tr = [ {en, <<"No">>} ] }, Body1),

    ok = m_rsc:delete(Id, Context).
