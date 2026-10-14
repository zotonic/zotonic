%% @copyright 2026 Marc Worrell
%% Run with bin/zotonic runtests import_feed_db_tests (disposable testsandbox).

%% Copyright 2026 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(import_feed_db_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([observe/2]).

import_test_() -> {timeout, 120, fun import_lifecycle/0}.
import_lifecycle() ->
    C = z_context:set_tz(<<"Europe/Amsterdam">>, z_acl:logon(1, z_context:new(zotonic_site_testsandbox))),
    ok = z_module_manager:activate_await(mod_import_feed, C),
    ok = m_import_feed:install(C),
    ImportUser = import_user(C),
    Input = #{<<"user_id">> => ImportUser, <<"url">> => <<"https://example.org/feed-test.xml">>, <<"title">> => <<"Test">>,
        <<"category_id">> => m_rsc:rid(article, C), <<"poll_minutes">> => 60,
        <<"media_mode">> => <<"none">>, <<"is_paused">> => true},
    {ok, FeedId} = m_import_feed:save(undefined, Input, C),
    {ok, F} = m_import_feed:get(FeedId, C),
    Entry = #{uri => <<"https://example.org/feed-test/1">>, guid => <<"one">>,
        url => <<"https://example.org/article">>, title => <<"Test <script>bad</script>">>,
        body => <<"<p>Body</p><script>bad()</script>">>, summary => <<"Summary">>,
        author => <<"Author">>, published => {{2026,10,12},{8,0,0}}, modified => undefined,
        tags => [], media => [], raw => {<<"item">>, [], []}},
    try
        ?assertEqual({error, eacces}, m_import_feed:get(FeedId, z_acl:anondo(C))),
        ?assertEqual(imported, z_import_feed_import:run(Entry, F, C)),
        Id = m_rsc:rid(maps:get(uri, Entry), C),
        ?assert(is_integer(Id)),
        ?assertEqual(false, m_rsc:p(Id, is_authoritative, C)),
        ?assertEqual({{2026,10,12},{8,0,0}}, m_rsc:p(Id, publication_start, C)),
        ?assertEqual(nomatch, binary:match(m_rsc:p(Id, body, C), <<"<script">>)),
        ?assertEqual(skipped, z_import_feed_import:run(Entry, F, C)),
        ?assertEqual(updated, z_import_feed_import:run(Entry#{title => <<"Updated">>}, F, C)),
        ?assertEqual(Id, m_rsc:rid(maps:get(uri, Entry), C)),
        ?assertEqual(updated, z_import_feed_import:run(Entry, F#{is_authoritative => true}, C)),
        ?assertEqual(true, m_rsc:p(Id, is_authoritative, C)),
        ?assertEqual(updated, z_import_feed_import:run(Entry#{title => <<"Local copy updated">>}, F#{is_authoritative => true}, C)),
        ?assertEqual(1, length(m_import_feed:items(FeedId, 0, C))),
        {Rendered, _} = z_template:render_to_iolist("_admin_import_feed_edit.tpl", [{feed, F}], C),
        Html = iolist_to_binary(Rendered),
        ?assertNotEqual(nomatch, binary:match(Html, <<"feed-settings">>)),
        {Discovery, _} = z_template:render_to_iolist("_admin_import_feed_discovery.tpl",
            [{feeds, [#{title => <<"Example feed">>, url => <<"https://example.org/feed">>}]}], C),
        ?assertNotEqual(nomatch, binary:match(iolist_to_binary(Discovery), <<"Example feed">>)),
        %% Observer suppresses default importing and sees raw data/edges/dependents.
        z_notifier:observe(import_resource, {?MODULE, observe}, 1, C),
        E2 = Entry#{uri => <<"https://example.org/feed-test/2">>},
        ?assertEqual(skipped, z_import_feed_import:run(E2, F, C)),
        ?assertEqual(undefined, m_rsc:rid(maps:get(uri, E2), C)),
        ErrorUri = <<"https://example.org/feed-test/error">>,
        ?assertMatch({error, _}, z_import_feed_import:run(Entry#{uri => ErrorUri}, F, C)),
        ?assertEqual(undefined, m_rsc:rid(ErrorUri, C)),
        {ok, ErrorLog} = m_import_feed_log:list(FeedId, 0, C),
        ?assert(lists:any(fun
            (#{uri := LogUri, event := <<"entry">>, status := <<"error">>}) -> LogUri =:= ErrorUri;
            (_) -> false
        end, maps:get(items, ErrorLog))),
        z_notifier:detach(import_resource, C),
        ok = m_rsc:delete(Id, C),
        ?assertEqual(skipped, z_import_feed_import:run(Entry#{title => <<"Do not resurrect">>}, F, C)),
        ?assertEqual(undefined, m_rsc:rid(maps:get(uri, Entry), C))
    after
        m_rsc:delete(ImportUser, C),
        m_acl_rule:delete_managed(?MODULE, C),
        z_notifier:detach(import_resource, C),
        z_db:q("delete from import_feed where id=$1", [FeedId], C),
        z_db:q("delete from import_feed_resource where uri like 'https://example.org/feed-test/%'", C)
    end.

observe(#import_resource{item = #{uri := <<"https://example.org/feed-test/error">>}}, _) ->
    {error, observer_test_error};
observe(#import_resource{item = #{edges := []}, related = [], source = #{importer := mod_import_feed, feed := #{id := _}},
        raw = {<<"item">>, [], []}}, _) ->
    {skip, observer_test}.

poll_and_media_test_() -> {timeout, 120, fun poll_and_media/0}.
poll_and_media() ->
    C = z_acl:logon(1, z_context:new(zotonic_site_testsandbox)),
    ok = m_import_feed:install(C),
    ImportUser = import_user(C),
    {ok, FeedId} = m_import_feed:save(undefined, #{
        <<"user_id">> => ImportUser,
        <<"url">> => <<"https://example.org/poll.xml">>, <<"title">> => <<"Poll test">>,
        <<"category_id">> => m_rsc:rid(article, C), <<"poll_minutes">> => 60,
        <<"media_mode">> => <<"dependent">>, <<"is_paused">> => true,
        <<"connections">> => <<"about=administrator">>}, C),
    MediaUrl = <<"https://example.org/import-feed-test.png">>,
    Xml = <<"<rss><channel><title>Poll</title><item><guid>https://example.org/poll-test/1</guid><title>Poll item</title><link>https://example.org/poll-test/1</link><description><![CDATA[Leading &amp; text<p>Body<img src='https://example.org/import-feed-test-small.png' srcset='https://example.org/import-feed-test-small.png 1x, https://example.org/import-feed-test.png 2x'></p>Trailing text]]></description><enclosure url='https://example.org/import-feed-test.png' type='image/png'/></item></channel></rss>">>,
    Png = base64:decode(<<"iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+ip1sAAAAASUVORK5CYII=">>),
    ok = meck:new(z_import_feed_http, [passthrough]),
    meck:expect(z_import_feed_http, get, fun(Url, _) -> {ok, {Url, [], byte_size(Xml), Xml}} end),
    meck:expect(z_import_feed_http, get, fun(Url, _, _) -> {ok, {Url, [], byte_size(Png), Png}} end),
    try
        ok = z_import_feed_poll:run(C),
        {ok, Before} = m_import_feed:get(FeedId, C),
        ?assertEqual(0, maps:get(polls, Before)),
        ok = z_mqtt:subscribe(<<"model/import_feed/event/log">>, C),
        ok = m_import_feed:pause(FeedId, false, C),
        ok = z_import_feed_poll:run(C),
        receive
            {mqtt_msg, #{message := #{payload := #{feed_id := FeedId, log_id := LiveId}}}} ->
                ?assertMatch({ok, #{feed_id := FeedId}}, m_import_feed_log:get(LiveId, C)),
                ?assertEqual({error, eacces}, m_import_feed_log:get(LiveId, z_acl:anondo(C)))
        after 1000 -> error(no_live_log_notification)
        end,
        z_mqtt:unsubscribe(<<"model/import_feed/event/log">>, C),
        {ok, After} = m_import_feed:get(FeedId, C),
        ?assertEqual(undefined, maps:get(last_error, After)),
        ?assertEqual(1, maps:get(polls, After)),
        ?assertEqual(1, maps:get(imported, After)),
        Id = m_rsc:rid(<<"https://example.org/poll-test/1">>, C),
        Mid = m_rsc:rid(MediaUrl, C),
        ?assert(is_integer(Id)), ?assert(is_integer(Mid)),
        {ok, Activity} = m_import_feed_log:list(FeedId, 0, C),
        LogRows = maps:get(items, Activity),
        ?assert(lists:all(fun(#{feed_id := LogFeed}) -> LogFeed =:= FeedId end, LogRows)),
        ?assert(lists:any(fun
            (#{event := <<"poll">>, status := <<"ok">>}) -> true;
            (_) -> false
        end, LogRows)),
        ?assert(lists:any(fun
            (#{event := <<"article">>, rsc_id := LogId, user_id := LogUser,
                    category_id := CategoryId}) ->
                LogId =:= Id andalso LogUser =:= ImportUser
                    andalso CategoryId =:= m_rsc:rid(article, C);
            (_) -> false
        end, LogRows)),
        ?assert(lists:any(fun
            (#{event := <<"media">>, rsc_id := LogId}) -> LogId =:= Mid;
            (_) -> false
        end, LogRows)),
        ?assertEqual({error, eacces}, m_import_feed_log:list(FeedId, 0, z_acl:anondo(C))),
        ?assertEqual({error, invalid_filter}, m_import_feed_log:list(<<"invalid">>, 0, C)),
        ?assertEqual({error, invalid_filter}, m_import_feed_log:list(FeedId, -1, C)),
        ?assertEqual(false, mod_import_feed:observe_acl_is_allowed(
            #acl_is_allowed{action = subscribe, object = #acl_mqtt{is_wildcard = false, packet = #{},
                topic = [<<"model">>, <<"import_feed">>, <<"event">>, <<"log">>]}}, z_acl:anondo(C))),
        {LogOutput, _} = z_template:render_to_iolist("_admin_import_feed_log.tpl",
            [{qfeed, FeedId}, {qoffset, 0}], C),
        LogHtml = iolist_to_binary(LogOutput),
        ?assertNotEqual(nomatch, binary:match(LogHtml, <<"Fetch now">>)),
        ?assertNotEqual(nomatch, binary:match(LogHtml, <<"poll-test/1">>)),
        PageContext = z_context:set_q(<<"feed">>, integer_to_binary(FeedId), C),
        PageContent = z_template:render_block(content, "admin_import_feed_log.tpl", [], PageContext),
        {PageOutput, _} = z_render:render_to_iolist(PageContent, PageContext),
        ?assertNotEqual(nomatch, binary:match(iolist_to_binary(PageOutput), <<"feed-log-entries">>)),
        %% Scrolling uses IDs rather than offsets, even if a newer event arrives.
        CursorTest = z_db:transaction(fun(Tx) ->
            z_db:q("insert into import_feed_log(feed_id,event,status)
                select $1,'test','ok' from generate_series(1,105)", [FeedId], Tx),
            {ok, FirstPage} = m_import_feed_log:list(FeedId, 0, Tx),
            ?assertEqual(100, length(maps:get(items, FirstPage))),
            ?assertEqual(true, maps:get(has_next, FirstPage)),
            NewId = z_db:q1("insert into import_feed_log(feed_id,event,status)
                values($1,'new','ok') returning id", [FeedId], Tx),
            {ok, OlderPage} = m_import_feed_log:list(FeedId, maps:get(next, FirstPage), Tx),
            LoadedIds = [maps:get(id, Row) || Row <- maps:get(items, FirstPage) ++ maps:get(items, OlderPage)],
            ?assertEqual(length(LoadedIds), length(lists:usort(LoadedIds))),
            ?assertNot(lists:member(NewId, LoadedIds)),
            Expected = z_db:q1("select count(*) from import_feed_log where feed_id=$1 and id<>$2", [FeedId, NewId], Tx),
            ?assertEqual(Expected, length(LoadedIds)),
            {Loader, _} = z_template:render_to_iolist("_admin_import_feed_log_loader.tpl",
                [{qfeed, FeedId}, {qbefore, 0}], Tx),
            ?assertNotEqual(nomatch, binary:match(iolist_to_binary(Loader), <<"z-lazy">>)),
            {rollback, cursor_checked}
        end, C),
        ?assertEqual(cursor_checked, CursorTest),
        ?assertEqual([Mid], m_edge:objects(Id, depiction, C)),
        ?assertEqual([1], m_edge:objects(Id, about, C)),
        ?assertEqual(true, m_rsc:p(Mid, is_dependent, C)),
        ?assertMatch(#{<<"mime">> := <<"image/png">>}, m_media:get(Mid, C)),
        ?assertEqual(2, length(m_import_feed:items(FeedId, 0, C))),
        ?assertEqual(nomatch, binary:match(m_rsc:p(Id, body, C), MediaUrl)),
        ?assertEqual(nomatch, binary:match(m_rsc:p(Id, body, C), <<"srcset">>)),
        ?assertNotEqual(nomatch, binary:match(m_rsc:p(Id, body, C),
            iolist_to_binary(["<!-- z-media ", integer_to_binary(Mid), " "]))),
        ?assertNotEqual(nomatch, binary:match(m_rsc:p(Id, body, C), <<"Leading &amp; text">>)),
        %% Attached mode puts the medium on the article itself.
        {ok, F} = m_import_feed:get(FeedId, C),
        {ok, Parsed} = z_import_feed_parse:parse(Xml, maps:get(url, F)),
        [Entry] = maps:get(entries, Parsed),
        ?assertEqual(updated, z_import_feed_import:run(Entry#{title => <<"Changed">>}, F, C)),
        ?assert(m_rsc:is_a(Mid, image, C)),
        ?assertEqual(updated, z_import_feed_import:run(Entry, F#{media_mode => <<"attached">>}, C)),
        ?assertMatch(#{<<"mime">> := <<"image/png">>}, m_media:get(Id, C)),
        ?assert(m_rsc:is_a(Id, article, C)),
        %% Media download failure leaves a valid article in either import mode.
        MissingUrl = <<"https://example.org/missing-feed-medium.png">>,
        meck:expect(z_import_feed_http, get, fun(_, _, _) -> {error, eacces} end),
        lists:foreach(fun(Mode) ->
            ArticleUri = <<"https://example.org/poll-test/missing-", Mode/binary>>,
            MissingEntry = Entry#{uri => ArticleUri, media => [MissingUrl],
                body => <<"<p>Article without media<img src='", MissingUrl/binary, "'></p>">>},
            ?assertEqual(imported, z_import_feed_import:run(MissingEntry, F#{media_mode => Mode}, C)),
            ArticleId = m_rsc:rid(ArticleUri, C),
            try
                ?assert(is_integer(ArticleId)),
                ?assertEqual(undefined, m_rsc:rid(MissingUrl, C)),
                ?assertEqual(undefined, m_media:get(ArticleId, C)),
                ?assertEqual([], m_edge:objects(ArticleId, depiction, C)),
                ?assertEqual(nomatch, binary:match(m_rsc:p(ArticleId, body, C), MissingUrl))
            after
                m_rsc:delete(ArticleId, C),
                z_db:q("delete from import_feed_resource where uri=$1", [ArticleUri], C)
            end
        end, [<<"dependent">>, <<"attached">>]),
        {ok, MediaLog} = m_import_feed_log:list(FeedId, 0, C),
        ?assert(lists:any(fun
            (#{event := <<"media">>, status := <<"error">>, uri := LogUri}) -> LogUri =:= MissingUrl;
            (_) -> false
        end, maps:get(items, MediaLog))),
        ok = m_rsc:delete(Id, C),
        case m_rsc:exists(Mid, C) of true -> m_rsc:delete(Mid, C); false -> ok end
    after
        m_rsc:delete(ImportUser, C),
        m_acl_rule:delete_managed(?MODULE, C),
        meck:unload(z_import_feed_http),
        z_db:q("delete from import_feed where id=$1", [FeedId], C),
        z_db:q("delete from import_feed_resource where uri in ($1,$2)",
            [<<"https://example.org/poll-test/1">>, MediaUrl], C)
    end.

%% Real restricted account: editor resource permissions plus feed-module access.
import_user(C) ->
    {ok, User} = m_rsc:insert(#{<<"category_id">> => person,
        <<"is_published">> => true, <<"title">> => <<"Feed test importer">>}, C),
    Username = <<"feed-test-", (integer_to_binary(User))/binary>>,
    ok = m_identity:set_username_pw(User, Username, z_ids:password(), C),
    {ok, _} = m_edge:insert(User, hasusergroup, acl_user_group_editors, C),
    m_acl_rule:replace_managed([{module, [
        {acl_user_group_id, acl_user_group_editors},
        {actions, [use]}, {module, mod_import_feed}
    ]}], ?MODULE, C),
    await_acl(erlang:monotonic_time(millisecond) + 5000, C),
    ?assertNot(m_import_feed:is_admin_user(User, C)),
    User.

await_acl(Deadline, C) ->
    {ok, Status} = mod_acl_user_groups:status(C),
    Pending = lists:any(fun(K) -> proplists:get_value(K, Status) =:= true end,
        [is_rebuilding, is_rebuild_publish, is_rebuild_edit]),
    case Pending of
        false -> ok;
        true ->
            case erlang:monotonic_time(millisecond) < Deadline of
                true -> timer:sleep(10), await_acl(Deadline, C);
                false -> error(acl_rebuild_timeout)
            end
    end.

native_depth_test_() -> {timeout, 60, fun native_depth/0}.
native_depth() ->
    C = z_acl:logon(1, z_context:new(zotonic_site_testsandbox)),
    ok = z_module_manager:activate_await(mod_import_feed, C),
    lists:foreach(fun(Depth) ->
        Result = z_db:transaction(fun(Tx) ->
            Base = <<"https://example.org/feed-native-depth/", (integer_to_binary(Depth))/binary, "/">>,
            A = <<Base/binary, "a">>, B = <<Base/binary, "b">>, D = <<Base/binary, "c">>,
            Cache = #{A => native_export(A, native_edge(B)),
                B => native_export(B, native_edge(D)), D => native_export(D, #{})},
            NativeC = z_context:set(import_feed_native, Cache, Tx),
            Options = z_import_feed_native:options(#{media_mode => <<"none">>, import_depth => Depth}, #{}),
            {ok, {Id, Imported}} = m_rsc_import:import_uri_recursive(A, Options, NativeC),
            ?assertEqual(Depth + 1, maps:size(Imported)),
            ?assertEqual(min(Depth, 1), length(m_edge:objects(Id, about, Tx))),
            ?assertEqual(Depth =:= 2, is_integer(m_rsc:rid(D, Tx))),
            {ok, Status} = m_rsc_import:get_import_status(Id, Tx),
            ?assertEqual(Depth, proplists:get_value(import_edges, maps:get(<<"options">>, Status))),
            {rollback, native_depth_ok}
        end, C),
        ?assertEqual(native_depth_ok, Result)
    end, [0, 1, 2]).

native_edge(Uri) ->
    #{<<"about">> => #{<<"predicate">> => #{<<"name">> => <<"about">>},
        <<"objects">> => [#{<<"object_id">> => #{<<"uri">> => Uri, <<"is_a">> => [<<"article">>]}}]}}.

native_export(Uri, Edges) ->
    #{<<"uri">> => Uri, <<"is_a">> => [<<"article">>],
        <<"resource">> => #{<<"title">> => Uri}, <<"edges">> => Edges}.
