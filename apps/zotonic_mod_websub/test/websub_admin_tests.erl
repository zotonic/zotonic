-module(websub_admin_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

admin_test_() -> {timeout, 60, fun admin/0}.

admin() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    C = z_context:new(zotonic_site_testsandbox),
    ok = z_module_manager:upgrade_await(C),
    A = z_acl:logon(1, C),
    {ok, Id} = m_rsc:insert(#{<<"category_id">> => text, <<"title">> => <<"Subscription overview test">>}, A),
    try
        ?assertEqual({error, eacces}, m_websub:m_get([<<"subscriptions">>], #{payload => #{}}, C)),
        ?assertEqual({error, eacces}, m_websub:m_get([<<"subscriber_count">>, Id], #{}, C)),
        z_db:q("insert into websub_export (local_rsc_id, callback_url, topic_url, secret, lease)
            select $1, 'https://subscriber.test/capability-secret-' || n,
                'https://publisher.test/topic', 'hmac-secret', now() + interval '1 hour'
            from generate_series(1, 51) n", [Id], A),
        z_db:q("insert into websub_import (local_rsc_id, callback_url, topic_url, source_uri,
                secret, callback_token, is_enabled, next_check)
            values ($1, 'https://local.test/callback-secret', 'https://external.test/topic',
                'https://external.test/id/123', 'hmac-secret', 'token-secret', false, null)", [Id], A),
        Filters = #{<<"rsc_id">> => integer_to_binary(Id), <<"type">> => <<"export">>},
        #{rows := Rows, has_next := true} = overview(Filters, A),
        ?assertEqual(50, length(Rows)),
        lists:foreach(fun(Row) ->
            ?assertEqual(Id, maps:get(local_rsc_id, Row)),
            ?assertEqual(<<"export">>, maps:get(type, Row)),
            ?assertEqual(<<"active">>, maps:get(status, Row)),
            ?assertEqual(<<"subscriber.test">>, maps:get(peer_host, Row)),
            ?assertEqual([], maps:keys(maps:without(
                [id, type, local_rsc_id, lease, last_activity, has_error, status, peer_host], Row)))
        end, Rows),
        ?assertMatch(#{rows := [_], has_next := false}, overview(Filters#{<<"page">> => 2}, A)),
        ?assertMatch(#{rows := [#{type := <<"import">>, status := <<"stopped">>, peer_host := <<"external.test">>}]},
            overview(Filters#{<<"type">> => <<"import">>}, A)),
        ?assertMatch(#{rows := [_, _]}, overview(Filters#{<<"type">> => <<"all">>, <<"page">> => 2}, A)),
        ?assertMatch(#{rows := []}, overview(Filters#{<<"rsc_id">> => 2147483647}, A)),
        lists:foreach(fun(Bad) -> ?assertMatch(#{is_invalid := true}, overview(Bad, A)) end, [
            Filters#{<<"rsc_id">> => <<"not-an-id">>}, Filters#{<<"rsc_id">> => #{}},
            Filters#{<<"type">> => <<"invalid">>}, Filters#{<<"page">> => -1},
            Filters#{<<"page">> => 10001}, Filters#{<<"page">> => []}, []
        ]),
        ?assertEqual({ok, {51, []}}, m_websub:m_get([<<"subscriber_count">>, Id], #{}, A)),
        z_db:q("update websub_export set lease=now()-interval '1 second' where local_rsc_id=$1", [Id], A),
        ?assertEqual({ok, {0, []}}, m_websub:m_get([<<"subscriber_count">>, Id], #{}, A)),
        ?assertMatch(#{rows := [#{status := <<"expired">>} | _]}, overview(Filters, A)),
        filters(Id, Filters, A),
        render(Id, A, C)
    after
        z_db:q("delete from websub_import where local_rsc_id=$1", [Id], A),
        m_rsc:delete(Id, A)
    end.

%% Filters must run before LIMIT, including when the matching row is older
%% than the first page. URL authority matching ignores credentials and ports.
filters(Id, Filters, C) ->
    z_db:q("update websub_export set callback_url='https://user:password@MiXeD.test.:8443/callback',
        is_error=true where id=(select min(id) from websub_export where local_rsc_id=$1)", [Id], C),
    Combined = Filters#{<<"hostname">> => <<" MIXED.test. ">>, <<"status">> => <<"expired">>, <<"errors">> => <<"yes">>},
    ?assertMatch(#{rows := [#{has_error := true}], has_next := false}, overview(Combined, C)),
    ?assertMatch(#{rows := []}, overview(Combined#{<<"errors">> => <<"no">>}, C)),
    ?assertMatch(#{rows := []}, overview(Combined#{<<"status">> => <<"active">>}, C)),
    ?assertMatch(#{rows := []}, overview(Combined#{<<"hostname">> => <<"test">>}, C)),
    ?assertMatch(#{rows := []}, overview(Combined#{<<"hostname">> => <<"password">>}, C)),
    #{rows := Healthy, has_next := false} = overview(Filters#{<<"errors">> => <<"no">>}, C),
    ?assertEqual(50, length(Healthy)),
    Import = Filters#{<<"type">> => <<"import">>, <<"hostname">> => <<"EXTERNAL.test">>, <<"status">> => <<"stopped">>},
    ?assertMatch(#{rows := [_]}, overview(Import, C)),
    z_db:q("update websub_import set is_enabled=true, pending_mode='subscribe',
        credential_error='test-error' where local_rsc_id=$1", [Id], C),
    ?assertMatch(#{rows := [_]}, overview(Import#{<<"status">> => <<"pending">>, <<"errors">> => <<"yes">>}, C)),
    z_db:q("update websub_import set lease=now()+interval '1 hour', is_unsubscribed=false,
        pending_mode=null where local_rsc_id=$1", [Id], C),
    ?assertMatch(#{rows := [_]}, overview(Import#{<<"status">> => <<"active">>}, C)),
    z_db:q("update websub_import set source_uri='https://[2001:db8::1]:8443/id/1' where local_rsc_id=$1", [Id], C),
    ?assertMatch(#{rows := [_]}, overview(Import#{<<"hostname">> => <<"[2001:db8::1]">>, <<"status">> => <<"all">>}, C)),
    lists:foreach(fun(Bad) -> ?assertMatch(#{is_invalid := true}, overview(Bad, C)) end, [
        Filters#{<<"hostname">> => #{}}, Filters#{<<"hostname">> => <<"https://mixed.test/path">>},
        Filters#{<<"hostname">> => <<"..">>}, Filters#{<<"hostname">> => <<"[]">>},
        Filters#{<<"hostname">> => <<255>>}, Filters#{<<"hostname">> => <<"%">>}, Filters#{<<"status">> => <<"bad">>},
        Filters#{<<"errors">> => <<"bad">>}
    ]).

overview(Filters, Context) ->
    {ok, {Result, []}} = m_websub:m_get([<<"subscriptions">>], #{payload => Filters}, Context),
    Result.

render(Id, Admin, Anon) ->
    C = z_context:set_q([{<<"rsc_id">>, integer_to_binary(Id)}, {<<"type">>, <<"export">>}], Admin),
    Html = html("admin_websub.tpl", [], C),
    ?assertNotEqual(nomatch, binary:match(Html, <<"subscriber.test">>)),
    ?assertEqual(nomatch, binary:match(Html, <<"external.test">>)),
    ?assertEqual(nomatch, binary:match(Html, <<"capability-secret">>)),
    ?assertEqual(nomatch, binary:match(Html, <<"hmac-secret">>)),
    FilteredContext = z_context:set_q([{<<"rsc_id">>, integer_to_binary(Id)},
        {<<"hostname">>, <<"subscriber.test">>}, {<<"status">>, <<"expired">>}, {<<"errors">>, <<"no">>}], Admin),
    FilteredHtml = html("admin_websub.tpl", [], FilteredContext),
    ?assertEqual(nomatch, binary:match(FilteredHtml, <<"MiXeD.test">>)),
    ?assertNotEqual(nomatch, binary:match(FilteredHtml, <<"value=\"expired\" selected">>)),
    ?assertNotEqual(nomatch, binary:match(FilteredHtml, <<"value=\"no\" selected">>)),
    Denied = html("admin_websub.tpl", [], Anon),
    ?assertEqual(nomatch, binary:match(Denied, <<"subscriber.test">>)),
    Sidebar = html("_admin_edit_sidebar_websub.tpl", [{id, Id}], Admin),
    ?assertNotEqual(nomatch, binary:match(Sidebar, <<"type=export">>)),
    ?assertNotEqual(nomatch, binary:match(Sidebar, <<"rsc_id=", (integer_to_binary(Id))/binary>>)).

html("admin_websub.tpl" = Template, Vars, Context) ->
    {Html, _} = z_template:render_block_to_iolist(content, Template, Vars, Context),
    iolist_to_binary(Html);
html(Template, Vars, Context) ->
    {Html, _} = z_template:render_to_iolist(Template, Vars, Context),
    iolist_to_binary(Html).

collection_options_template_test() ->
    C = z_acl:logon(1, z_context:new(zotonic_site_testsandbox)),
    {ok, Id} = m_rsc:insert(#{
        <<"category_id">> => collection,
        <<"is_authoritative">> => false,
        <<"uri">> => <<"https://collection.test/id/options">>
    }, C),
    try
        Html = html("_dialog_rsc_import_options.tpl", [
            {id, Id},
            {import_options, [{import_edges, 1}, {is_subscribe_haspart, true}]}
        ], C),
        ?assertNotEqual(nomatch, binary:match(Html,
            <<"name=\"z_import_subscribe_haspart\" value=\"1\" checked">>))
    after
        m_rsc:delete(Id, C)
    end.
