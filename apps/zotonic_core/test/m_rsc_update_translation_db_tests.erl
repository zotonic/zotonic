%% @author Codex
%% @hidden

-module(m_rsc_update_translation_db_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../include/zotonic.hrl").

update_translation_list_input_test() ->
    AdminC = admin_context(),
    {ok, Id} = m_rsc:insert(
        #{
            <<"category_id">> => <<"text">>,
            <<"language">> => [ en ],
            <<"title">> => #trans{ tr = [ {en, <<"Hello">>} ] }
        },
        AdminC),
    try
        {ok, Id} = m_rsc:update_translation(
            Id,
            <<"nl">>,
            [ {<<"title">>, <<"Hallo">>} ],
            AdminC),
        Title = m_rsc:p(Id, <<"title">>, AdminC),
        ?assertEqual(<<"Hello">>, tr(en, Title)),
        ?assertEqual(<<"Hallo">>, tr(nl, Title)),
        ?assertEqual([ en, nl ], m_rsc:p(Id, <<"language">>, AdminC))
    after
        ok = m_rsc:delete(Id, AdminC)
    end.

update_translation_nested_map_preserves_top_level_test() ->
    AdminC = admin_context(),
    {ok, Id} = m_rsc:insert(
        #{
            <<"category_id">> => <<"text">>,
            <<"language">> => [ en ],
            <<"title">> => #trans{ tr = [ {en, <<"Hello">>} ] },
            <<"profile">> => #{
                <<"bio">> => #trans{ tr = [ {en, <<"About me">>} ] },
                <<"tagline">> => <<"Existing tagline">>,
                <<"rating">> => 5
            }
        },
        AdminC),
    try
        {ok, Id} = m_rsc:update_translation(
            Id,
            nl,
            #{
                <<"profile">> => #{
                    <<"bio">> => <<"Over mij">>
                }
            },
            AdminC),
        Profile = m_rsc:p(Id, <<"profile">>, AdminC),
        Bio = maps:get(<<"bio">>, Profile),
        ?assertEqual(<<"About me">>, tr(en, Bio)),
        ?assertEqual(<<"Over mij">>, tr(nl, Bio)),
        ?assertEqual(<<"Existing tagline">>, maps:get(<<"tagline">>, Profile)),
        ?assertEqual(5, maps:get(<<"rating">>, Profile))
    after
        ok = m_rsc:delete(Id, AdminC)
    end.

update_translation_new_property_creates_trans_test() ->
    AdminC = admin_context(),
    {ok, Id} = m_rsc:insert(
        #{
            <<"category_id">> => <<"text">>,
            <<"language">> => [ en ],
            <<"title">> => #trans{ tr = [ {en, <<"Hello">>} ] }
        },
        AdminC),
    try
        {ok, Id} = m_rsc:update_translation(
            Id,
            nl,
            #{
                <<"teaser">> => <<"Korte tekst">>
            },
            AdminC),
        Teaser = m_rsc:p(Id, <<"teaser">>, AdminC),
        ?assertEqual(<<>>, tr(en, Teaser)),
        ?assertEqual(<<"Korte tekst">>, tr(nl, Teaser))
    after
        ok = m_rsc:delete(Id, AdminC)
    end.

update_translation_blocks_merge_by_name_test() ->
    AdminC = admin_context(),
    {ok, Id} = m_rsc:insert(
        #{
            <<"category_id">> => <<"text">>,
            <<"language">> => [ en ],
            <<"title">> => #trans{ tr = [ {en, <<"Hello">>} ] },
            <<"blocks">> => [
                #{
                    <<"name">> => <<"b1">>,
                    <<"type">> => <<"header">>,
                    <<"body">> => #trans{ tr = [ {en, <<"One">>} ] }
                },
                #{
                    <<"name">> => <<"b2">>,
                    <<"type">> => <<"header">>,
                    <<"body">> => #trans{ tr = [ {en, <<"Two">>} ] }
                }
            ]
        },
        AdminC),
    try
        {ok, Id} = m_rsc:update_translation(
            Id,
            nl,
            #{
                <<"blocks">> => [
                    #{
                        <<"name">> => <<"b2">>,
                        <<"type">> => <<"header">>,
                        <<"body">> => <<"Twee">>
                    },
                    #{
                        <<"name">> => <<"b3">>,
                        <<"type">> => <<"header">>,
                        <<"body">> => <<"Ignored">>
                    }
                ]
            },
            AdminC),
        Blocks = m_rsc:p(Id, <<"blocks">>, AdminC),
        ?assertEqual(2, length(Blocks)),
        ?assertEqual([ <<"b1">>, <<"b2">> ], [ maps:get(<<"name">>, B) || B <- Blocks ]),
        B2 = block(<<"b2">>, Blocks),
        Body2 = maps:get(<<"body">>, B2),
        ?assertEqual(<<"Two">>, tr(en, Body2)),
        ?assertEqual(<<"Twee">>, tr(nl, Body2))
    after
        ok = m_rsc:delete(Id, AdminC)
    end.

update_translation_unsafe_not_escaped_test() ->
    AdminC = admin_context(),
    {ok, Id} = m_rsc:insert(
        #{
            <<"category_id">> => <<"text">>,
            <<"language">> => [ en ],
            <<"title">> => #trans{ tr = [ {en, <<"Hello">>} ] },
            <<"profile">> => #{
                <<"unsafe">> => #trans{ tr = [ {en, <<"<p>Safe</p>">>} ] }
            }
        },
        AdminC),
    try
        UnsafeText = <<"A & B">>,
        {ok, Id} = m_rsc:update_translation(
            Id,
            nl,
            #{
                <<"profile">> => #{
                    <<"unsafe">> => UnsafeText
                }
            },
            AdminC),
        Profile = m_rsc:p(Id, <<"profile">>, AdminC),
        BodyUnsafe = maps:get(<<"unsafe">>, Profile),
        ?assertEqual(<<"<p>Safe</p>">>, tr(en, BodyUnsafe)),
        ?assertEqual(UnsafeText, tr(nl, BodyUnsafe))
    after
        ok = m_rsc:delete(Id, AdminC)
    end.

admin_context() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    C = z_context:new(zotonic_site_testsandbox),
    z_acl:logon(?ACL_ADMIN_USER_ID, C).

tr(Lang, #trans{ tr = Tr }) ->
    proplists:get_value(Lang, Tr).

block(Name, [ #{ <<"name">> := Name } = B | _ ]) ->
    B;
block(Name, [ _ | Bs ]) ->
    block(Name, Bs).
