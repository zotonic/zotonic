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
        #search_result{result = Rows, total = 51, pages = 2,
            page = 1, pagelen = 50, prev = 1, next = 2}
            = overview(Filters, A),
        ?assertEqual(50, length(Rows)),
        lists:foreach(fun(Row) ->
            ?assertEqual(Id, maps:get(local_rsc_id, Row)),
            ?assertEqual(<<"export">>, maps:get(type, Row)),
            ?assertEqual(<<"active">>, maps:get(status, Row)),
            ?assertEqual(<<"subscriber.test">>, maps:get(peer_host, Row)),
            ?assertEqual([], maps:keys(maps:without(
                [id, type, local_rsc_id, lease, last_activity, has_error, status, peer_host], Row)))
        end, Rows),
        ?assertMatch(#search_result{result = [_], total = 51, pages = 2, page = 2, prev = 1, next = false},
            overview(Filters#{<<"page">> => 2}, A)),
        ?assertMatch(#search_result{result = [], total = 51, pages = 2, page = 3, prev = 2, next = false},
            overview(Filters#{<<"page">> => 3}, A)),
        ?assertMatch(#search_result{result = [#{type := <<"import">>, status := <<"stopped">>, peer_host := <<"external.test">>}]},
            overview(Filters#{<<"type">> => <<"import">>}, A)),
        ?assertMatch(#search_result{result = [_, _], total = 52}, overview(Filters#{<<"type">> => <<"all">>, <<"page">> => 2}, A)),
        ?assertMatch(#search_result{result = [], total = 0, pages = 0}, overview(Filters#{<<"rsc_id">> => 2147483647}, A)),
        lists:foreach(fun(Bad) -> ?assertMatch(#{is_invalid := true}, overview(Bad, A)) end, [
            Filters#{<<"rsc_id">> => <<"not-an-id">>}, Filters#{<<"rsc_id">> => #{}},
            Filters#{<<"type">> => <<"invalid">>}, Filters#{<<"page">> => -1},
            Filters#{<<"page">> => 10001}, Filters#{<<"page">> => []}, []
        ]),
        ?assertEqual({ok, {51, []}}, m_websub:m_get([<<"subscriber_count">>, Id], #{}, A)),
        z_db:q("update websub_export set lease=now()-interval '1 second' where local_rsc_id=$1", [Id], A),
        ?assertEqual({ok, {0, []}}, m_websub:m_get([<<"subscriber_count">>, Id], #{}, A)),
        ?assertMatch(#search_result{result = [#{status := <<"expired">>} | _]}, overview(Filters, A)),
        filters(Id, Filters, A),
        render(Id, A, C),
        retry_button(Id, A)
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
    ?assertMatch(#search_result{result = [#{has_error := true}], total = 1, next = false}, overview(Combined, C)),
    ?assertMatch(#search_result{result = [], total = 0}, overview(Combined#{<<"errors">> => <<"no">>}, C)),
    ?assertMatch(#search_result{result = [], total = 0}, overview(Combined#{<<"status">> => <<"active">>}, C)),
    ?assertMatch(#search_result{result = [], total = 0}, overview(Combined#{<<"hostname">> => <<"test">>}, C)),
    ?assertMatch(#search_result{result = [], total = 0}, overview(Combined#{<<"hostname">> => <<"password">>}, C)),
    #search_result{result = Healthy, total = 50, pages = 1, next = false} = overview(Filters#{<<"errors">> => <<"no">>}, C),
    ?assertEqual(50, length(Healthy)),
    Import = Filters#{<<"type">> => <<"import">>, <<"hostname">> => <<"EXTERNAL.test">>, <<"status">> => <<"stopped">>},
    ?assertMatch(#search_result{result = [_]}, overview(Import, C)),
    z_db:q("update websub_import set is_enabled=true, pending_mode='subscribe',
        credential_error='test-error' where local_rsc_id=$1", [Id], C),
    ?assertMatch(#search_result{result = [_]}, overview(Import#{<<"status">> => <<"pending">>, <<"errors">> => <<"yes">>}, C)),
    z_db:q("update websub_import set lease=now()+interval '1 hour', is_unsubscribed=false,
        pending_mode=null where local_rsc_id=$1", [Id], C),
    ?assertMatch(#search_result{result = [_]}, overview(Import#{<<"status">> => <<"active">>}, C)),
    z_db:q("update websub_import set source_uri='https://[2001:db8::1]:8443/id/1' where local_rsc_id=$1", [Id], C),
    ?assertMatch(#search_result{result = [_]}, overview(Import#{<<"hostname">> => <<"[2001:db8::1]">>, <<"status">> => <<"all">>}, C)),
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
    C = z_context:set_q([{<<"qrsc_id">>, integer_to_binary(Id)}, {<<"qtype">>, <<"export">>}], Admin),
    Html = html("admin_websub.tpl", [], C),
    ?assertNotEqual(nomatch, binary:match(Html, <<"subscriber.test">>)),
    ?assertEqual(nomatch, binary:match(Html, <<"external.test">>)),
    ?assertEqual(nomatch, binary:match(Html, <<"capability-secret">>)),
    ?assertEqual(nomatch, binary:match(Html, <<"hmac-secret">>)),
    ?assertNotEqual(nomatch, binary:match(Html, <<"pagination">>)),
    {match, [PageUrl]} = re:run(Html, <<"href=\"([^\"]*page=2[^\"]*)\"">>,
        [{capture, [1], binary}]),
    ?assertNotEqual(nomatch, binary:match(PageUrl, <<"qtype=export">>)),
    ?assertNotEqual(nomatch, binary:match(PageUrl, <<"qrsc_id=", (integer_to_binary(Id))/binary>>)),
    FilteredContext = z_context:set_q([{<<"qrsc_id">>, integer_to_binary(Id)},
        {<<"qhostname">>, <<"subscriber.test">>}, {<<"qstatus">>, <<"expired">>}, {<<"qerrors">>, <<"no">>}], Admin),
    FilteredHtml = html("admin_websub.tpl", [], FilteredContext),
    ?assertEqual(nomatch, binary:match(FilteredHtml, <<"MiXeD.test">>)),
    ?assertNotEqual(nomatch, binary:match(FilteredHtml, <<"value=\"expired\" selected">>)),
    ?assertNotEqual(nomatch, binary:match(FilteredHtml, <<"value=\"no\" selected">>)),
    Denied = html("admin_websub.tpl", [], Anon),
    ?assertEqual(nomatch, binary:match(Denied, <<"subscriber.test">>)),
    Sidebar = html("_admin_edit_sidebar_websub.tpl", [{id, Id}], Admin),
    ?assertNotEqual(nomatch, binary:match(Sidebar, <<"qtype=export">>)),
    ?assertNotEqual(nomatch, binary:match(Sidebar, <<"qrsc_id=", (integer_to_binary(Id))/binary>>)).

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
            {import_options, [{import_edges, 1}, {is_subscribe_connections, true}]}
        ], C),
        ?assertNotEqual(nomatch, binary:match(Html,
            <<"name=\"z_import_subscribe_connections\" value=\"1\" checked">>))
    after
        m_rsc:delete(Id, C)
    end.

retry_button(Id, C) ->
    ImportContext = z_context:set_q([{<<"qrsc_id">>, integer_to_binary(Id)},
        {<<"qtype">>, <<"import">>}], C),
    Retry = <<"title=\"Retry subscription\"">>,
    ?assertEqual(nomatch, binary:match(html("admin_websub.tpl", [], ImportContext), Retry)),
    {ok, Id} = m_rsc:update(Id, #{<<"is_authoritative">> => false,
        <<"uri">> => <<"https://external.test/id/123">>}, C),
    ?assertNotEqual(nomatch, binary:match(html("admin_websub.tpl", [], ImportContext), Retry)),
    LiveHtml = html("_admin_rsc_import_status.tpl", [{id, Id}], C),
    ?assertNotEqual(nomatch, binary:match(LiveHtml, <<"Automatic updates are active.">>)),
    ExportContext = z_context:set_q(<<"qtype">>, <<"export">>, ImportContext),
    ?assertEqual(nomatch, binary:match(html("admin_websub.tpl", [], ExportContext), Retry)),
    z_db:q("update websub_import set credential_error=null, last_error=null where local_rsc_id=$1", [Id], C),
    ?assertEqual(nomatch, binary:match(html("admin_websub.tpl", [], ImportContext), Retry)).
