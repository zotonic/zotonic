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
-include("../include/mod_import_feed.hrl").
-export([observe/2]).

import_test_() -> {timeout, 120, fun import_lifecycle/0}.
import_lifecycle() ->
    Context = z_context:set_tz(<<"Europe/Amsterdam">>, z_acl:logon(1, z_context:new(zotonic_site_testsandbox))),
    ok = z_module_manager:activate_await(mod_import_feed, Context),
    ok = m_import_feed:install(Context),
    ImportUser = import_user(Context),
    Input = #{<<"user_id">> => ImportUser, <<"url">> => <<"https://example.org/feed-test.xml">>, <<"title">> => <<"Test">>,
        <<"category_id">> => m_rsc:rid(article, Context), <<"poll_minutes">> => 60,
        <<"media_mode">> => <<"none">>, <<"is_paused">> => true},
    {ok, FeedId} = m_import_feed:save(undefined, Input, Context),
    {ok, Feed} = m_import_feed:get(FeedId, Context),
    Entry = #{uri => <<"https://example.org/feed-test/1">>, guid => <<"one">>,
        url => <<"https://example.org/article">>, title => <<"Test <script>bad</script>">>,
        body => <<"<p>Body</p><script>bad()</script>">>, summary => <<"Summary">>,
        author => <<"Author">>, published => {{2026,10,12},{8,0,0}}, modified => undefined,
        tags => [], media => [], raw => {<<"item">>, [], []}},
    try
        ?assertEqual({error, eacces}, m_import_feed:get(FeedId, z_acl:anondo(Context))),
        ?assertEqual(imported, z_import_feed_import:run(Entry, Feed, Context)),
        Id = m_rsc:rid(maps:get(uri, Entry), Context),
        ?assert(is_integer(Id)),
        ?assertEqual(false, m_rsc:p(Id, is_authoritative, Context)),
        ?assertEqual({{2026,10,12},{8,0,0}}, m_rsc:p(Id, publication_start, Context)),
        ?assertEqual(nomatch, binary:match(m_rsc:p(Id, body, Context), <<"<script">>)),
        ?assertEqual(skipped, z_import_feed_import:run(Entry, Feed, Context)),
        ?assertEqual(updated, z_import_feed_import:run(Entry#{title => <<"Updated">>}, Feed, Context)),
        ?assertEqual(Id, m_rsc:rid(maps:get(uri, Entry), Context)),
        ?assertEqual(updated, z_import_feed_import:run(Entry, Feed#{is_authoritative => true}, Context)),
        ?assertEqual(true, m_rsc:p(Id, is_authoritative, Context)),
        ?assertEqual(updated, z_import_feed_import:run(Entry#{title => <<"Local copy updated">>}, Feed#{is_authoritative => true}, Context)),
        ?assertEqual(1, length(m_import_feed:items(FeedId, 0, Context))),
        {Rendered, _} = z_template:render_to_iolist("_admin_import_feed_edit.tpl", [{feed, Feed}], Context),
        Html = iolist_to_binary(Rendered),
        ?assertNotEqual(nomatch, binary:match(Html, <<"feed-settings">>)),
        {Discovery, _} = z_template:render_to_iolist("_admin_import_feed_discovery.tpl",
            [{feeds, [#{title => <<"Example feed">>, url => <<"https://example.org/feed">>}]}], Context),
        ?assertNotEqual(nomatch, binary:match(iolist_to_binary(Discovery), <<"Example feed">>)),
        %% Observer suppresses default importing and sees raw data/edges/dependents.
        z_notifier:observe(import_feed_item, {?MODULE, observe}, 1, Context),
        ObservedEntry = Entry#{uri => <<"https://example.org/feed-test/2">>},
        ?assertEqual(skipped, z_import_feed_import:run(ObservedEntry, Feed, Context)),
        ?assertEqual(undefined, m_rsc:rid(maps:get(uri, ObservedEntry), Context)),
        z_notifier:detach(import_feed_item, Context),
        ok = m_rsc:delete(Id, Context),
        ?assertEqual(skipped, z_import_feed_import:run(Entry#{title => <<"Do not resurrect">>}, Feed, Context)),
        ?assertEqual(undefined, m_rsc:rid(maps:get(uri, Entry), Context))
    after
        m_rsc:delete(ImportUser, Context),
        m_acl_rule:delete_managed(?MODULE, Context),
        z_notifier:detach(import_feed_item, Context),
        z_db:q("delete from import_feed where id=$1", [FeedId], Context),
        z_db:q("delete from import_feed_resource where uri like 'https://example.org/feed-test/%'", Context)
    end.

observe(#import_feed_item{item = #{edges := []}, dependents = [], feed = #{id := _},
        raw = {<<"item">>, [], []}}, _) ->
    {skip, observer_test}.

poll_and_media_test_() -> {timeout, 120, fun poll_and_media/0}.
poll_and_media() ->
    Context = z_acl:logon(1, z_context:new(zotonic_site_testsandbox)),
    ok = m_import_feed:install(Context),
    ImportUser = import_user(Context),
    {ok, FeedId} = m_import_feed:save(undefined, #{
        <<"user_id">> => ImportUser,
        <<"url">> => <<"https://example.org/poll.xml">>, <<"title">> => <<"Poll test">>,
        <<"category_id">> => m_rsc:rid(article, Context), <<"poll_minutes">> => 60,
        <<"media_mode">> => <<"dependent">>, <<"is_paused">> => true,
        <<"connections">> => <<"about=administrator">>}, Context),
    MediaUrl = <<"https://example.org/import-feed-test.png">>,
    Xml = <<"<rss><channel><title>Poll</title><item><guid>https://example.org/poll-test/1</guid><title>Poll item</title><link>https://example.org/poll-test/1</link><description><![CDATA[Leading &amp; text<p>Body<img src='https://example.org/import-feed-test-small.png' srcset='https://example.org/import-feed-test-small.png 1x, https://example.org/import-feed-test.png 2x'></p>Trailing text]]></description><enclosure url='https://example.org/import-feed-test.png' type='image/png'/></item></channel></rss>">>,
    Png = base64:decode(<<"iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+jwWQAAAAASUVORK5CYII=">>),
    ok = meck:new(z_import_feed_http, [passthrough]),
    meck:expect(z_import_feed_http, get, fun(Url, _) -> {ok, {Url, [], byte_size(Xml), Xml}} end),
    meck:expect(z_import_feed_http, get, fun(Url, _, _) -> {ok, {Url, [], byte_size(Png), Png}} end),
    try
        ok = z_import_feed_poll:run(Context),
        {ok, Before} = m_import_feed:get(FeedId, Context),
        ?assertEqual(0, maps:get(polls, Before)),
        ok = m_import_feed:pause(FeedId, false, Context),
        ok = z_import_feed_poll:run(Context),
        {ok, After} = m_import_feed:get(FeedId, Context),
        ?assertEqual(undefined, maps:get(last_error, After)),
        ?assertEqual(1, maps:get(polls, After)),
        ?assertEqual(1, maps:get(imported, After)),
        Id = m_rsc:rid(<<"https://example.org/poll-test/1">>, Context),
        Mid = m_rsc:rid(MediaUrl, Context),
        ?assert(is_integer(Id)), ?assert(is_integer(Mid)),
        ?assertEqual([Mid], m_edge:objects(Id, depiction, Context)),
        ?assertEqual([1], m_edge:objects(Id, about, Context)),
        ?assertEqual(true, m_rsc:p(Mid, is_dependent, Context)),
        ?assertMatch(#{<<"mime">> := <<"image/png">>}, m_media:get(Mid, Context)),
        ?assertEqual(2, length(m_import_feed:items(FeedId, 0, Context))),
        ?assertEqual(nomatch, binary:match(m_rsc:p(Id, body, Context), MediaUrl)),
        ?assertEqual(nomatch, binary:match(m_rsc:p(Id, body, Context), <<"srcset">>)),
        ?assertNotEqual(nomatch, binary:match(m_rsc:p(Id, body, Context),
            iolist_to_binary(["<!-- z-media ", integer_to_binary(Mid), " "]))),
        ?assertNotEqual(nomatch, binary:match(m_rsc:p(Id, body, Context), <<"Leading &amp; text">>)),
        %% Attached mode puts the medium on the article itself.
        {ok, Feed} = m_import_feed:get(FeedId, Context),
        {ok, Parsed} = z_import_feed_parse:parse(Xml, maps:get(url, Feed)),
        [Entry] = maps:get(entries, Parsed),
        ?assertEqual(updated, z_import_feed_import:run(Entry#{title => <<"Changed">>}, Feed, Context)),
        ?assert(m_rsc:is_a(Mid, image, Context)),
        ?assertEqual(updated, z_import_feed_import:run(Entry, Feed#{media_mode => <<"attached">>}, Context)),
        ?assertMatch(#{<<"mime">> := <<"image/png">>}, m_media:get(Id, Context)),
        ?assert(m_rsc:is_a(Id, article, Context)),
        ok = m_rsc:delete(Id, Context),
        case m_rsc:exists(Mid, Context) of true -> m_rsc:delete(Mid, Context); false -> ok end
    after
        m_rsc:delete(ImportUser, Context),
        m_acl_rule:delete_managed(?MODULE, Context),
        meck:unload(z_import_feed_http),
        z_db:q("delete from import_feed where id=$1", [FeedId], Context),
        z_db:q("delete from import_feed_resource where uri in ($1,$2)",
            [<<"https://example.org/poll-test/1">>, MediaUrl], Context)
    end.

%% Real restricted account: editor resource permissions plus feed-module access.
import_user(Context) ->
    {ok, User} = m_rsc:insert(#{<<"category_id">> => person,
        <<"is_published">> => true, <<"title">> => <<"Feed test importer">>}, Context),
    Username = <<"feed-test-", (integer_to_binary(User))/binary>>,
    ok = m_identity:set_username_pw(User, Username, z_ids:password(), Context),
    {ok, _} = m_edge:insert(User, hasusergroup, acl_user_group_editors, Context),
    m_acl_rule:replace_managed([{module, [
        {acl_user_group_id, acl_user_group_editors},
        {actions, [use]}, {module, mod_import_feed}
    ]}], ?MODULE, Context),
    await_acl(erlang:monotonic_time(millisecond) + 5000, Context),
    ?assertNot(m_import_feed:is_admin_user(User, Context)),
    User.

await_acl(Deadline, Context) ->
    {ok, Status} = mod_acl_user_groups:status(Context),
    Pending = lists:any(fun(Key) -> proplists:get_value(Key, Status) =:= true end,
        [is_rebuilding, is_rebuild_publish, is_rebuild_edit]),
    case Pending of
        false -> ok;
        true ->
            case erlang:monotonic_time(millisecond) < Deadline of
                true -> timer:sleep(10), await_acl(Deadline, Context);
                false -> error(acl_rebuild_timeout)
            end
    end.

native_depth_test_() -> {timeout, 60, fun native_depth/0}.
native_depth() ->
    Context = z_acl:logon(1, z_context:new(zotonic_site_testsandbox)),
    ok = z_module_manager:activate_await(mod_import_feed, Context),
    lists:foreach(fun(Depth) ->
        Result = z_db:transaction(fun(TransactionContext) ->
            Base = <<"https://example.org/feed-native-depth/", (integer_to_binary(Depth))/binary, "/">>,
            RootUri = <<Base/binary, "a">>, ChildUri = <<Base/binary, "b">>, GrandchildUri = <<Base/binary, "c">>,
            Cache = #{RootUri => native_export(RootUri, native_edge(ChildUri)),
                ChildUri => native_export(ChildUri, native_edge(GrandchildUri)), GrandchildUri => native_export(GrandchildUri, #{})},
            NativeContext = z_context:set(import_feed_native, Cache, TransactionContext),
            Options = z_import_feed_native:options(#{media_mode => <<"none">>, import_depth => Depth}, #{}),
            {ok, {Id, Imported}} = m_rsc_import:import_uri_recursive(RootUri, Options, NativeContext),
            ?assertEqual(Depth + 1, maps:size(Imported)),
            ?assertEqual(min(Depth, 1), length(m_edge:objects(Id, about, TransactionContext))),
            ?assertEqual(Depth =:= 2, is_integer(m_rsc:rid(GrandchildUri, TransactionContext))),
            {ok, Status} = m_rsc_import:get_import_status(Id, TransactionContext),
            ?assertEqual(Depth, proplists:get_value(import_edges, maps:get(<<"options">>, Status))),
            {rollback, native_depth_ok}
        end, Context),
        ?assertEqual(native_depth_ok, Result)
    end, [0, 1, 2]).

native_edge(Uri) ->
    #{<<"about">> => #{<<"predicate">> => #{<<"name">> => <<"about">>},
        <<"objects">> => [#{<<"object_id">> => #{<<"uri">> => Uri, <<"is_a">> => [<<"article">>]}}]}}.

native_export(Uri, Edges) ->
    #{<<"uri">> => Uri, <<"is_a">> => [<<"article">>],
        <<"resource">> => #{<<"title">> => Uri}, <<"edges">> => Edges}.
