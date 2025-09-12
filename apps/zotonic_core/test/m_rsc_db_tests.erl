%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @hidden

-module(m_rsc_db_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/zotonic.hrl").

prepare_cols_test() ->
    ?assertEqual({ok, #{}}, z_db:prepare_cols([], #{})),

    % Props go to the right place
    ?assertEqual({ok, #{<<"a">> => <<"a value">>}},
                 z_db:prepare_cols([<<"a">>, <<"b">>], #{<<"a">> => <<"a value">>})),
    ?assertEqual({ok, #{<<"a">> => <<"a value">>, <<"b">> => <<"b value">>}},
                 z_db:prepare_cols([<<"a">>, <<"b">>], #{<<"a">> => <<"a value">>,
                                                         <<"b">> => <<"b value">>})),

    % Column is not known
    ?assertEqual({error, {unknown_column,[<<"c">>]}},
                 z_db:prepare_cols([<<"a">>, <<"b">>], #{<<"a">> => <<"a value">>,
                                                    <<"c">> => <<"c value">>})),

    % When there is a props column, unknown properties go to that column.
    ?assertEqual({ok,#{<<"a">> => <<"a value">>,
                       <<"props">> => #{<<"c">> => <<"c value">>}}},
                 z_db:prepare_cols([<<"a">>, <<"b">>, <<"props">>], #{<<"a">> => <<"a value">>,
                                                                      <<"c">> => <<"c value">>})),

    % An existing props map will be merged with any new values.
    ?assertEqual({ok,#{<<"a">> => <<"a value">>,
                       <<"props">> => #{<<"c">> => <<"c value">>,
                                        <<"d">> => <<"d value">>}}},
                 z_db:prepare_cols([<<"a">>, <<"b">>, <<"props">>],
                                   #{<<"a">> => <<"a value">>,
                                     <<"c">> => <<"c value">>,
                                     <<"props">> => #{<<"d">> => <<"d value">>}})),

    % When there is a props_json column, unknown properties go to that column.
    ?assertEqual({ok,#{<<"a">> => <<"a value">>,
                       <<"props_json">> => #{<<"c">> => <<"c value">>}}},
                 z_db:prepare_cols([<<"a">>, <<"b">>, <<"props">>, <<"props_json">>],
                                   #{<<"a">> => <<"a value">>,
                                     <<"c">> => <<"c value">>})),

    % When there is a props_json column, that gets priority
    ?assertEqual({ok,#{<<"a">> => <<"a value">>,
                       <<"props_json">> => #{<<"c">> => <<"c value">>}}},
                 z_db:prepare_cols([<<"a">>, <<"b">>, <<"props">>, <<"props_json">>],
                                   #{<<"a">> => <<"a value">>, <<"c">> => <<"c value">>})),
    ?assertEqual({ok,#{<<"a">> => <<"a value">>,
                       <<"props_json">> => #{<<"c">> => <<"c value">>}}},
                 z_db:prepare_cols([<<"a">>, <<"b">>, <<"props_json">>],
                                   #{<<"a">> => <<"a value">>, <<"c">> => <<"c value">>})),

    % existing props and props_json fields are merged
    ?assertEqual({ok,#{<<"a">> => <<"a value">>,
                       <<"props_json">> => #{<<"c">> => <<"c value">>,
                                             <<"e">> => <<"e value">>}}},
                 z_db:prepare_cols([<<"a">>, <<"b">>, <<"props">>, <<"props_json">>],
                                   #{<<"a">> => <<"a value">>,
                                     <<"c">> => <<"c value">>,
                                     <<"props_json">> => #{<<"e">> => <<"e value">>}
                                    })),

    ok.

merge_props_test() ->
    M = z_db:merge_props([[{id, 1},
                           {is_visible, true},
                           {rsc_id, 330},
                           {user_id, undefined},
                           {email, <<"test@example.com">>},
                           {name, <<"foo">>},
                           {keep_informed, false},
                           {props, undefined},
                           {created, {{2020,6,25},{10,54,37}}},
                           {props_json, undefined}],
                          [{id, 2},
                           {is_visible, true},
                           {rsc_id, 330},
                           {user_id, undefined},
                           {email, <<"test@example.com">>},
                           {name, <<"foo">>},
                           {keep_informed, false},
                           {props, #{message => <<"test test">>}},
                           {created, {{2020,6,25},{10,54,37}}},
                           {props_json, undefined}],
                          [{id, 3},
                           {is_visible, true},
                           {rsc_id, 330},
                           {user_id, undefined},
                           {email, <<"test@example.com">>},
                           {name, <<"foo">>},
                           {keep_informed, false},
                           {props, [{message, <<"test test">>}]},
                           {created, {{2020,6,25},{10,54,37}}},
                           {props_json, undefined}],
                          [{id, 4},
                           {is_visible, true},
                           {rsc_id, 330},
                           {user_id, undefined},
                           {email, <<"test@example.com">>},
                           {name, <<"foo">>},
                           {keep_informed, false},
                           {props, undefined},
                           {created, {{2020,6,25},{10,54,37}}},
                           {props_json, #{message => <<"test test">>}}],
                          [{id, 5},
                           {is_visible, true},
                           {rsc_id, 330},
                           {user_id, undefined},
                           {email, <<"test@example.com">>},
                           {name, <<"foo">>},
                           {keep_informed, false},
                           {props, #{<<"message">> => <<"test test">>}},
                           {created, {{2020,6,25},{11,54,55}}},
                           {props_json, #{message => <<"123">>}}],
                          [{id, 6},
                           {is_visible, true},
                           {rsc_id, 330},
                           {user_id, undefined},
                           {email, <<"test@example.com">>},
                           {name, <<"foo">>},
                           {keep_informed, false},
                           {props, [{message,  <<"test test">>}, {extra, <<"hello">>} ]},
                           {created, {{2020,6,25},{11,54,55}}},
                           {props_json, #{message => <<"123">>}}] ]),

    ?assertEqual([[{id, 1},
                   {is_visible, true},
                   {rsc_id, 330},
                   {user_id, undefined},
                   {email, <<"test@example.com">>},
                   {name, <<"foo">>},
                   {keep_informed, false},
                   {props, undefined},
                   {created, {{2020,6,25},{10,54,37}}},
                   {props_json, undefined}],
                  [{id, 2},
                   {is_visible, true},
                   {rsc_id, 330},
                   {user_id, undefined},
                   {email, <<"test@example.com">>},
                   {name, <<"foo">>},
                   {keep_informed, false},
                   {created, {{2020,6,25},{10,54,37}}},
                   {props_json, undefined},
                   {message, <<"test test">>}],
                  [{id, 3},
                   {is_visible, true},
                   {rsc_id, 330},
                   {user_id, undefined},
                   {email, <<"test@example.com">>},
                   {name, <<"foo">>},
                   {keep_informed, false},
                   {created, {{2020,6,25},{10,54,37}}},
                   {props_json, undefined},
                   {message, <<"test test">>}],
                  [{id, 4},
                   {is_visible, true},
                   {rsc_id, 330},
                   {user_id, undefined},
                   {email, <<"test@example.com">>},
                   {name, <<"foo">>},
                   {keep_informed, false},
                   {props, undefined},
                   {created, {{2020,6,25},{10,54,37}}},
                   {message, <<"test test">>}],
                  [{id, 5},
                   {is_visible, true},
                   {rsc_id, 330},
                   {user_id, undefined},
                   {email, <<"test@example.com">>},
                   {name, <<"foo">>},
                   {keep_informed, false},
                   {created, {{2020,6,25},{11,54,55}}},
                   {message, <<"123">>}],
                  [{id, 6},
                   {is_visible, true},
                   {rsc_id, 330},
                   {user_id, undefined},
                   {email, <<"test@example.com">>},
                   {name, <<"foo">>},
                   {keep_informed, false},
                   {created, {{2020,6,25},{11,54,55}}},
                   {extra, <<"hello">>},
                   {message, <<"123">>}]], M),
    ok.

qmap_props_json_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    C = z_context:new(zotonic_site_testsandbox),

    Columns = [
        #column_def{name=id, type="serial", is_nullable=false},
        #column_def{name=name, type="bytea"},
        #column_def{name=props_json, type="jsonb"} ],

    Table = qmap_props_json_test,

    ok = z_db:create_table(Table, Columns, C),
    {ok, _ } = z_db:insert(Table, #{ name => <<"a">>, prop_1 => <<"abc">>, prop_2 => <<"def">> }, C),
    {ok, _ } = z_db:insert(Table, #{ name => <<"b">>, prop_3 => <<"ghi">>, prop_4 => <<"jkl">> }, C),

    ?assertEqual(
       {ok, [#{<<"id">> => 1,
               <<"name">> => <<"a">>,
               <<"props_json">> =>
               #{<<"prop_1">> => <<"abc">>,
                 <<"prop_2">> => <<"def">>}},
             #{<<"id">> => 2,
               <<"name">> => <<"b">>,
               <<"props_json">> =>
               #{<<"prop_3">> => <<"ghi">>,
                 <<"prop_4">> => <<"jkl">>}}]},
       z_db:qmap(<<"select * from qmap_props_json_test order by id">>, C)),

    ?assertEqual(
       {ok, [#{<<"id">> => 1,
               <<"name">> => <<"a">>,
               <<"prop_1">> => <<"abc">>,
               <<"prop_2">> => <<"def">>},
             #{<<"id">> => 2,
               <<"name">> => <<"b">>,
               <<"prop_3">> => <<"ghi">>,
               <<"prop_4">> => <<"jkl">>}]},
       z_db:qmap_props(<<"select * from qmap_props_json_test order by id">>, C)),

    ?assertEqual(
       {ok, [#{id => 1,
               name => <<"a">>,
               <<"prop_1">> => <<"abc">>,
               <<"prop_2">> => <<"def">>},
             #{id => 2,
               name => <<"b">>,
               <<"prop_3">> => <<"ghi">>,
               <<"prop_4">> => <<"jkl">>}]},
       z_db:qmap_props(<<"select * from qmap_props_json_test order by id">>, [], [{keys, atom}], C)),

    ?assertEqual(
       [ [{id, 1},
          {name, <<"a">>},
          {prop_1, <<"abc">>},
          {prop_2, <<"def">>}],
         [{id, 2},
          {name, <<"b">>},
          {prop_3, <<"ghi">>},
          {prop_4, <<"jkl">>}] ],
       z_db:assoc_props(<<"select * from qmap_props_json_test order by id">>, [], C)),

    ok.

qmap_props_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    C = z_context:new(zotonic_site_testsandbox),

    Columns = [
        #column_def{name=id, type="serial", is_nullable=false},
        #column_def{name=name, type="bytea"},
        #column_def{name=props, type="bytea"} ],

    Table = qmap_props_test,

    ok = z_db:create_table(Table, Columns, C),
    {ok, _ } = z_db:insert(Table, #{ name => <<"a">>, prop_1 => <<"abc">>, prop_2 => <<"def">> }, C),
    {ok, _ } = z_db:insert(Table, #{ name => <<"b">>, prop_3 => <<"ghi">>, prop_4 => <<"jkl">> }, C),

    ?assertEqual(
       {ok, [#{<<"id">> => 1,
               <<"name">> => <<"a">>,
               <<"props">> =>
               #{<<"prop_1">> => <<"abc">>,
                 <<"prop_2">> => <<"def">>}},
             #{<<"id">> => 2,
               <<"name">> => <<"b">>,
               <<"props">> =>
               #{<<"prop_3">> => <<"ghi">>,
                 <<"prop_4">> => <<"jkl">>}}]},
       z_db:qmap(<<"select * from qmap_props_test order by id">>, C)),

    ?assertEqual(
       {ok, [#{<<"id">> => 1,
               <<"name">> => <<"a">>,
               <<"prop_1">> => <<"abc">>,
               <<"prop_2">> => <<"def">>},
             #{<<"id">> => 2,
               <<"name">> => <<"b">>,
               <<"prop_3">> => <<"ghi">>,
               <<"prop_4">> => <<"jkl">>}]},
       z_db:qmap_props(<<"select * from qmap_props_test order by id">>, C)),

    ?assertEqual(
       {ok, [#{id => 1,
               name => <<"a">>,
               <<"prop_1">> => <<"abc">>,
               <<"prop_2">> => <<"def">>},
             #{id => 2,
               name => <<"b">>,
               <<"prop_3">> => <<"ghi">>,
               <<"prop_4">> => <<"jkl">>}]},
       z_db:qmap_props(<<"select * from qmap_props_test order by id">>, [], [{keys, atom}], C)),

    ?assertEqual(
       [ [{id, 1},
          {name, <<"a">>},
          {prop_1, <<"abc">>},
          {prop_2, <<"def">>}],
         [{id, 2},
          {name, <<"b">>},
          {prop_3, <<"ghi">>},
          {prop_4, <<"jkl">>}] ],
       z_db:assoc_props(<<"select * from qmap_props_test order by id">>, [], C)),

    ok.

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
            <<"category_id">> => <<"text">>,
            <<"page_path">> => <<"/foo/bar">>
        }, AdminC),
    ?assertEqual(<<"/foo/bar">>, m_rsc:p(Id, page_path, AdminC)),
    ok = m_rsc:delete(Id, AdminC).

page_path_trans_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    C = z_context:new(zotonic_site_testsandbox),
    AdminC = z_acl:logon(?ACL_ADMIN_USER_ID, C),
    Path = #trans{ tr = [ {en, <<"/foo/bar">>}, {nl, <<"/aap/noot">>} ] },
    {ok, Id} = m_rsc:insert(#{
            <<"title">> => <<"Hello.">>,
            <<"category_id">> => <<"text">>,
            <<"page_path">> => Path
        }, AdminC),
    ?assertEqual(Path, m_rsc:p(Id, page_path, AdminC)),
    PivotPaths = z_db:q1("select pivot_page_path from rsc where id = $1", [ Id ], AdminC),
    % Expect sorted values
    ?assertEqual([ <<"/aap/noot">>, <<"/foo/bar">> ], PivotPaths),
    ok = m_rsc:delete(Id, AdminC).

page_path_escape_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    C = z_context:new(zotonic_site_testsandbox),
    AdminC = z_acl:logon(?ACL_ADMIN_USER_ID, C),
    {ok, Id} = m_rsc:insert(#{
            <<"title">> => <<"Hello.">>,
            <<"category_id">> => <<"text">>,
            <<"page_path">> => <<" foo /bar&">>
        }, AdminC),
    ?assertEqual(<<"/foo%20/bar-">>, m_rsc:p(Id, page_path, AdminC)),
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
    {ok, DuplicateId} = m_rsc:duplicate(rose, #{}, AdminC),
    ok = m_rsc:delete(rose, AdminC),
    false = m_rsc:exists(rose, AdminC),
    ok = m_rsc:delete(DuplicateId, AdminC).

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
        <<"category_id">> => other,
        <<"is_published">> => true,
        <<"language">> => [ 'zh-hant', en ],
        <<"title">> => #trans{
            tr = [ {en, <<"Hello">>}, {'zh-hant', <<"香港"/utf8>>}]
        }
    },
    {ok, Id} = m_rsc:insert(Props, C),
    ?assertEqual( m_rsc:rid(other, C), m_rsc:p(Id, category_id, C) ),
    ?assertEqual( [ 'en', 'zh-hant' ], m_rsc:p(Id, language, C) ),
    Lng = z_db:q1("select language from rsc where id = $1", [ Id ], C),
    ?assertEqual( [ <<"en">>, <<"zh-hant">> ], Lng ),
    m_rsc:delete(Id, C),
    ok.

