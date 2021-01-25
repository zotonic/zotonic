%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @hidden

-module(m_rsc_db_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic.hrl").

modify_rsc_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    C = z_context:new(zotonic_site_testsandbox),
    AdminC = z_acl:logon(?ACL_ADMIN_USER_ID, C),
    CatId = m_rsc:rid(text, C),

    ?assertEqual(eacces_or_nocategory,
            case m_rsc:insert([{title, "Hello."}], C) of
                {error, eacces} -> eacces_or_nocategory;
                {error, nocategory} -> eacces_or_nocategory;
                InsOther -> InsOther
            end),
    ?assertEqual({error, eacces}, m_rsc:insert([{title, "Hello."}, {category_id, CatId}], C)),

    {ok, Id} = m_rsc:insert([{title, "Hello."}, {category_id, CatId}], AdminC),

    %% Existence check
    ?assertEqual(true, m_rsc:exists(Id, AdminC)),
    ?assertEqual(true, m_rsc:exists(Id, C)),

    %% Check properties
    ?assertEqual(<<"Hello.">>, m_rsc:p(Id, title, AdminC)),
    ?assertEqual(1, m_rsc:p(Id, version, AdminC)),
    ?assertEqual(false, m_rsc:p(Id, is_featured, AdminC)),
    ?assertEqual(true, m_rsc:p(Id, is_authoritative, AdminC)),
    ?assertEqual(true, m_rsc:is_a(Id, text, AdminC)),

    ?assertEqual(false, m_rsc:p(Id, is_published, AdminC)),
    ?assertEqual(undefined, m_rsc:p(Id, publication_start, AdminC)),
    ?assertEqual(undefined, m_rsc:p(Id, title, C)), %% not visible for anonymous yet

    %% Update
    ?assertEqual({error, eacces}, m_rsc:update(Id, [{title, "Bye."}, {is_published, true}], C)),

    {ok, Id} = m_rsc:update(Id, [{title, "Bye."}, {is_published, true}], AdminC),
    ?assertEqual(<<"Bye.">>, m_rsc:p(Id, title, AdminC)),
    ?assertNotEqual(undefined, m_rsc:p(Id, publication_start, AdminC)),

    ?assertEqual(2, m_rsc:p(Id, version, AdminC)),

    %% Delete
    ?assertEqual({error, eacces}, m_rsc:delete(Id, C)),
    ?assertEqual(ok, m_rsc:delete(Id, AdminC)),

    %% verify that it's gone
    ?assertEqual(undefined, m_rsc:p(Id, title, AdminC)),

    %% Existence check
    ?assertEqual(false, m_rsc:exists(Id, AdminC)),
    ?assertEqual(false, m_rsc:exists(Id, C)),

    ok.


page_path_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    C = z_context:new(zotonic_site_testsandbox),
    AdminC = z_acl:logon(?ACL_ADMIN_USER_ID, C),

    {ok, Id} = m_rsc:insert(#{
            <<"title">> => <<"Hello.">>,
            <<"category">> => <<"text">>,
            <<"page_path">> => <<"/foo/bar">>
        }, AdminC),
    ?assertEqual(<<"/foo/bar">>, m_rsc:p(Id, page_path, AdminC)),
    ok = m_rsc:delete(Id, AdminC).

%% @doc Resource name instead of id as argument.
name_rid_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    C = z_context:new(zotonic_site_testsandbox),
    AdminC = z_acl:logon(?ACL_ADMIN_USER_ID, C),
    {ok, Id} = m_rsc:insert(#{
            <<"title">> => <<"What’s in a name?"/utf8>>,
            <<"category_id">> => <<"text">>,
            <<"name">> => <<"rose">>
        },
        AdminC),
    % {ok, _} = m_rsc:get_raw(rose, AdminC),
    ok = m_rsc_update:flush(rose, AdminC),
    {ok, Id} = m_rsc:update(rose, #{}, AdminC),
    {ok, _DuplicateId} = m_rsc:duplicate(rose, #{}, AdminC),
    ok = m_rsc:delete(rose, AdminC).

%% @doc Check normalization of dates
normalize_date_props_test() ->
    InPropsA = [
        {<<"dt:dmy:0:date_start">>, <<"13/7/-99">>},
        {<<"date_is_all_day">>, true}
    ],
    {ok, OutPropsA} = z_props:from_qs(InPropsA),
    ?assertEqual({{-99, 7, 13}, {0, 0, 0}}, maps:get(<<"date_start">>, OutPropsA)),

    InPropsB = [
        {<<"dt:ymd:0:date_start">>, <<"-99/7/13">>},
        {<<"date_is_all_day">>, true}
    ],
    {ok, OutPropsB} = z_props:from_qs(InPropsB),
    ?assertEqual({{-99, 7, 13}, {0, 0, 0}}, maps:get(<<"date_start">>, OutPropsB)),

    InPropsC = [
        {<<"dt:dmy:0:date_start">>, <<"31/12/1999">>},
        {<<"date_is_all_day">>, true}
    ],
    {ok, OutPropsC} = z_props:from_qs(InPropsC),
    ?assertEqual({{1999, 12, 31}, {0, 0, 0}}, maps:get(<<"date_start">>, OutPropsC)),

    InPropsD = [
        {<<"dt:ymd:0:date_start">>, <<"1999/12/31">>},
        {<<"date_is_all_day">>, true}
    ],
    {ok, OutPropsD} = z_props:from_qs(InPropsD),
    ?assertEqual({{1999, 12, 31}, {0, 0, 0}}, maps:get(<<"date_start">>, OutPropsD)),

    InPropsE = [
        {<<"dt:ymd:0:date_start">>, <<"1999-12-31">>},
        {<<"date_is_all_day">>, true}
    ],
    {ok, OutPropsE} = z_props:from_qs(InPropsE),
    ?assertEqual({{1999, 12, 31}, {0, 0, 0}}, maps:get(<<"date_start">>, OutPropsE)),

    ok.


%% @doc Test the language array
language_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    C = z_acl:sudo( z_context:new(zotonic_site_testsandbox) ),
    Props = #{
        <<"category">> => other,
        <<"is_published">> => true,
        <<"language">> => [ 'zh-hant-hk', en ],
        <<"title">> => #trans{
            tr = [ {en, <<"Hello">>}, {'zh-hant-hk', <<"香港"/utf8>>}]
        }
    },
    {ok, Id} = m_rsc:insert(Props, C),
    ?assertEqual( m_rsc:rid(other, C), m_rsc:p(Id, category_id, C) ),
    ?assertEqual( [ 'en', 'zh-hant-hk'], m_rsc:p(Id, language, C) ),
    Lng = z_db:q1("select language from rsc where id = $1", [ Id ], C),
    ?assertEqual( [ <<"en">>, <<"zh-hant-hk">> ], Lng ),
    m_rsc:delete(Id, C),
    ok.

