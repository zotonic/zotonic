-module(z_context_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic.hrl").

-define(SITE, zotonic_site_testsandbox).


pickle_roundtrip_test() ->
    Context = z_acl:logon(
        123,
        z_context:new(?SITE, [nl, en], <<"Europe/Amsterdam">>)
    ),
    PickledContext = z_context:pickle(Context),
    RestoredContext = z_context:depickle(PickledContext),

    ?assertMatch({pickled_context, ?SITE, 2, #{}}, PickledContext),
    assert_context_equal(Context, RestoredContext).


pickle_read_only_roundtrip_test() ->
    Context = z_acl:set_read_only(
        true,
        z_acl:logon(123, z_context:new(?SITE, de, <<"Europe/Berlin">>))
    ),
    RestoredContext = z_context:depickle(z_context:pickle(Context)),

    assert_context_equal(Context, RestoredContext),
    ?assert(z_acl:is_read_only(RestoredContext)),
    ?assertNot(z_acl:is_sudo(RestoredContext)).


pickle_sudo_roundtrip_test() ->
    Context = z_acl:sudo(z_context:new(?SITE, fr, <<"Europe/Paris">>)),
    RestoredContext = z_context:depickle(z_context:pickle(Context)),

    assert_context_equal(Context, RestoredContext),
    ?assert(z_acl:is_sudo(RestoredContext)),
    ?assertNot(z_acl:is_read_only(RestoredContext)).


depickle_version_2_defaults_test() ->
    RestoredContext = z_context:depickle(
        {pickled_context, ?SITE, 2, #{ user_id => undefined }}
    ),

    ?assertEqual(?SITE, z_context:site(RestoredContext)),
    ?assertEqual(undefined, RestoredContext#context.user_id),
    ?assertNot(z_acl:is_sudo(RestoredContext)),
    ?assertNot(z_acl:is_read_only(RestoredContext)).


depickle_legacy_context_test() ->
    LegacyContext = {pickled_context, ?SITE, 123, [nl, en], undefined},
    RestoredContext = z_context:depickle(LegacyContext),

    ?assertEqual(?SITE, z_context:site(RestoredContext)),
    ?assertEqual(123, RestoredContext#context.user_id),
    ?assertEqual(nl, z_context:language(RestoredContext)).


depickle_legacy_context_with_timezone_test() ->
    LegacyContext = {
        pickled_context,
        ?SITE,
        123,
        [de, en],
        <<"Europe/Berlin">>,
        undefined
    },
    RestoredContext = z_context:depickle(LegacyContext),

    ?assertEqual(?SITE, z_context:site(RestoredContext)),
    ?assertEqual(123, RestoredContext#context.user_id),
    ?assertEqual(de, z_context:language(RestoredContext)),
    ?assertEqual(<<"Europe/Berlin">>, z_context:tz(RestoredContext)).


depickle_site_formats_test() ->
    PickledContexts = [
        {pickled_context, ?SITE, undefined, [en], undefined},
        {pickled_context, ?SITE, undefined, [en], <<"UTC">>, undefined},
        {pickled_context, ?SITE, 2, #{ user_id => undefined }}
    ],
    lists:foreach(
        fun(PickledContext) ->
            ?assertEqual(?SITE, z_context:depickle_site(PickledContext))
        end,
        PickledContexts
    ).


assert_context_equal(Expected, Actual) ->
    ?assertEqual(z_context:site(Expected), z_context:site(Actual)),
    ?assertEqual(Expected#context.user_id, Actual#context.user_id),
    ?assertEqual(Expected#context.language, Actual#context.language),
    ?assertEqual(Expected#context.tz, Actual#context.tz),
    ?assertEqual(z_acl:is_sudo(Expected), z_acl:is_sudo(Actual)),
    ?assertEqual(z_acl:is_read_only(Expected), z_acl:is_read_only(Actual)).
