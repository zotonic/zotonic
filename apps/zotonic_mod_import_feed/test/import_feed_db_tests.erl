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
-export([observe/2, observe_tracking/3]).

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
        atomic_tracking(FeedId, Entry, F, C),
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
        ?assertNotEqual(nomatch, binary:match(Html, <<"name=\"refetch_deleted\"">>)),
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

%% Verify that bookkeeping is visible inside the resource transaction and that
%% an interrupted authoritative import can reuse its ID without a final checksum.
atomic_tracking(FeedId, Entry, Feed, Context) ->
    Uri = <<"https://example.org/feed-test/atomic">>,
    RejectedUri = <<"https://example.org/feed-test/rejected">>,
    Props = #{<<"category_id">> => article, <<"is_authoritative">> => true,
        <<"feed_source_uri">> => Uri, <<"title">> => <<"Atomic import">>},
    WriteContext = z_context:set(import_feed_identity, {FeedId, Uri, <<"article">>}, Context),
    z_notifier:observe(rsc_update, {?MODULE, observe_tracking}, 0, Context),
    try
        {ok, Id} = m_rsc:insert(Props, [is_import], WriteContext),
        ?assertEqual(<<"importing">>, z_db:q1("select status from import_feed_item where feed_id=$1 and uri=$2", [FeedId, Uri], Context)),
        ?assertEqual(updated, z_import_feed_import:run(Entry#{uri => Uri}, Feed#{is_authoritative => true}, Context)),
        ?assertEqual(Id, z_db:q1("select rsc_id from import_feed_resource where uri=$1", [Uri], Context)),
        ok = m_rsc:delete(Id, Context),
        RejectedContext = z_context:set(feed_tracking_reject, true,
            z_context:set(import_feed_identity, {FeedId, RejectedUri, <<"article">>}, Context)),
        ?assertEqual({error, tracking_rejected},
            m_rsc:insert(Props#{<<"feed_source_uri">> => RejectedUri}, [is_import], RejectedContext)),
        receive
            {tracking_rejected_id, RejectedId} -> ?assertNot(m_rsc:exists(RejectedId, Context))
        after 1000 -> error(no_transaction_observation)
        end,
        ?assertEqual(0, z_db:q1("select count(*) from import_feed_resource where uri=$1", [RejectedUri], Context)),
        ?assertEqual(0, z_db:q1("select count(*) from import_feed_item where feed_id=$1 and uri=$2", [FeedId, RejectedUri], Context))
    after
        z_notifier:detach(rsc_update, Context),
        %% Clean up even when demonstrating the rollback regression.
        Ids = z_db:q("select rsc_id from import_feed_resource where uri in ($1,$2) and rsc_id is not null", [Uri, RejectedUri], Context),
        lists:foreach(fun({ResourceId}) -> m_rsc:delete(ResourceId, Context) end, Ids),
        z_db:q("delete from import_feed_item where feed_id=$1 and uri in ($2,$3)", [FeedId, Uri, RejectedUri], Context),
        z_db:q("delete from import_feed_resource where uri in ($1,$2)", [Uri, RejectedUri], Context)
    end.

observe_tracking(#rsc_update{id = Id}, {ok, _} = Acc, Context) ->
    case z_context:get(import_feed_identity, Context) of
        {FeedId, Uri, _Kind} ->
            ?assert(is_pid(Context#context.dbc)),
            ?assertEqual(Id, z_db:q1("select rsc_id from import_feed_resource where uri=$1", [Uri], Context)),
            ?assertEqual(<<"importing">>, z_db:q1("select status from import_feed_item where feed_id=$1 and uri=$2", [FeedId, Uri], Context)),
            case z_context:get(feed_tracking_reject, Context) of
                true -> self() ! {tracking_rejected_id, Id}, {error, tracking_rejected};
                _ -> Acc
            end;
        _ -> Acc
    end;
observe_tracking(_, Acc, _) -> Acc.

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
    meck:expect(z_import_feed_http, get_feed, fun(Settings, FetchContext) ->
        z_import_feed_http:get(maps:get(url, Settings), FetchContext)
    end),
    %% Downloads must not retain a transaction connection, including attached
    %% media on an existing article and dependent media on a new article.
    meck:expect(z_import_feed_http, get, fun(Url, FetchContext) ->
        ?assertEqual(undefined, FetchContext#context.dbc),
        {ok, {Url, [], byte_size(Xml), Xml}}
    end),
    MediaFetch = fun(Url, _, FetchContext) ->
        ?assertEqual(undefined, FetchContext#context.dbc),
        ?assertEqual(ImportUser, z_acl:user(FetchContext)),
        {ok, {Url, [], byte_size(Png), Png}}
    end,
    meck:expect(z_import_feed_http, get, MediaFetch),
    %% Run queued downloads explicitly so the article-before-media boundary is deterministic.
    Parent = self(),
    ok = meck:new(z_pivot_rsc, [passthrough]),
    meck:expect(z_pivot_rsc, insert_task, fun
        (z_import_feed_media, download_task, _Key, Args, _) ->
            Parent ! {media_task, Args}, {ok, 1};
        (Module, Function, Key, Args, TaskContext) ->
            meck:passthrough([Module, Function, Key, Args, TaskContext])
    end),
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
        ?assertEqual(undefined, m_media:get(Mid, C)),
        ?assertNotEqual(nomatch, binary:match(m_rsc:p(Id, body, C), <<"<!-- z-media ">>)),
        ?assertEqual(0, meck:num_calls(z_import_feed_http, get, ['_', '_', '_'])),
        meck:expect(z_import_feed_http, get, fun(_, _, _) -> {error, timeout} end),
        {delay, 60, RetryArgs} = run_media_task(C),
        [TaskFeed, TaskId, TaskUrl, TaskMedium, 1] = RetryArgs,
        Task = fun(Retry) ->
            z_import_feed_media:download_task(TaskFeed, TaskId, TaskUrl, TaskMedium, Retry, C)
        end,
        Delays = [120, 240, 480, 960, 1920, 3840, 7680, 15360, 30720,
            43200, 43200, 43200, 43200, 43200, 43200],
        lists:foreach(fun({Retry, Delay}) ->
            ?assertEqual({delay, Delay, [TaskFeed, TaskId, TaskUrl, TaskMedium, Retry + 1]}, Task(Retry))
        end, lists:zip(lists:seq(1, 15), Delays)),
        ?assertEqual(ok, Task(16)),
        meck:expect(z_import_feed_http, get, fun(_, _, _) ->
            {error, {http_status, 503, {retry_after, 86400}}}
        end),
        ?assertMatch({delay, 86400, _}, Task(15)),
        ?assertEqual(ok, Task(16)),
        meck:expect(z_import_feed_http, get, fun(_, _, _) -> {error, eacces} end),
        ?assertEqual(ok, Task(1)),
        ?assertEqual(undefined, m_media:get(Mid, C)),
        meck:expect(z_import_feed_http, get, MediaFetch),
        ?assertEqual(ok, apply(z_import_feed_media, download_task, RetryArgs ++ [C])),
        %% An already completed/stale task must not download again.
        Downloads = meck:num_calls(z_import_feed_http, get, ['_', '_', '_']),
        ?assertEqual(ok, apply(z_import_feed_media, download_task, RetryArgs ++ [C])),
        ?assertEqual(Downloads, meck:num_calls(z_import_feed_http, get, ['_', '_', '_'])),
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
        ?assertEqual({rollback, cursor_checked}, CursorTest),
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
        ?assertEqual(undefined, m_media:get(Id, C)),
        ?assertEqual(ok, run_media_task(C)),
        ?assertMatch(#{<<"mime">> := <<"image/png">>}, m_media:get(Id, C)),
        ?assert(m_rsc:is_a(Id, article, C)),
        %% Media download failure leaves the imported article and placeholder intact.
        MissingUrl = <<"https://example.org/missing-feed-medium.png">>,
        meck:expect(z_import_feed_http, get, fun(_, _, _) -> {error, eacces} end),
        lists:foreach(fun(Mode) ->
            ArticleUri = <<"https://example.org/poll-test/missing-", Mode/binary>>,
            MissingEntry = Entry#{uri => ArticleUri, media => [MissingUrl],
                body => <<"<p>Article without media<img src='", MissingUrl/binary, "'></p>">>},
            ?assertEqual(imported, z_import_feed_import:run(MissingEntry, F#{media_mode => Mode}, C)),
            ArticleId = m_rsc:rid(ArticleUri, C),
            PlaceholderId = m_rsc:rid(MissingUrl, C),
            ?assertEqual(ok, run_media_task(C)),
            try
                ?assert(is_integer(ArticleId)),
                ?assertEqual(undefined, m_media:get(ArticleId, C)),
                ?assertEqual(nomatch, binary:match(m_rsc:p(ArticleId, body, C), MissingUrl)),
                ?assertNotEqual(nomatch, binary:match(m_rsc:p(ArticleId, body, C), <<"<!-- z-media ">>))
            after
                m_rsc:delete(ArticleId, C),
                case PlaceholderId of undefined -> ok; _ -> m_rsc:delete(PlaceholderId, C) end,
                z_db:q("delete from import_feed_resource where uri in ($1,$2)", [ArticleUri, MissingUrl], C)
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
        meck:unload(z_pivot_rsc),
        z_db:q("delete from import_feed where id=$1", [FeedId], C),
        z_db:q("delete from import_feed_resource where uri in ($1,$2)",
            [<<"https://example.org/poll-test/1">>, MediaUrl], C)
    end.

run_media_task(Context) ->
    receive
        {media_task, Args} -> apply(z_import_feed_media, download_task, Args ++ [Context])
    after 1000 -> error(no_media_task)
    end.

refetch_deleted_test_() -> {timeout, 120, fun refetch_deleted/0}.
refetch_deleted() ->
    Context = z_acl:logon(1, z_context:new(zotonic_site_testsandbox)),
    ok = m_import_feed:install(Context),
    %% Exercise the upgrade and its idempotency as well as the fresh schema.
    z_db:q("alter table import_feed drop column refetch_deleted", Context),
    ok = m_import_feed:install(Context),
    ok = m_import_feed:install(Context),
    User = import_user(Context),
    Present = <<"https://example.org/restore/present">>,
    Absent = <<"https://example.org/restore/absent">>,
    Shared = <<"https://example.org/restore/shared">>,
    Media = <<"https://example.org/restore/photo.png">>,
    Input = #{<<"user_id">> => User, <<"url">> => <<"https://example.org/restore/feed">>,
        <<"title">> => <<"Restore once">>, <<"category_id">> => m_rsc:rid(article, Context),
        <<"poll_minutes">> => 60, <<"media_mode">> => <<"dependent">>, <<"is_paused">> => true},
    {ok, FeedId} = m_import_feed:save(undefined, Input, Context),
    Xml = <<"<rss><channel><item><link>https://example.org/restore/present</link><title>Present</title><description><![CDATA[<img src='https://example.org/restore/photo.png'>]]></description></item><item><link>https://example.org/restore/shared</link><title>Shared</title><description><![CDATA[<img src='https://example.org/restore/photo.png'>]]></description></item></channel></rss>">>,
    {ok, Parsed} = z_import_feed_parse:parse(Xml, <<"https://example.org/restore/feed">>),
    [Entry, SharedEntry] = maps:get(entries, Parsed),
    meck:new(z_import_feed_http, [passthrough]),
    meck:expect(z_import_feed_http, get_feed, fun(Settings, FetchContext) ->
        z_import_feed_http:get(maps:get(url, Settings), FetchContext)
    end),
    Fetch = fun(Url, _) -> {ok, {Url, [], byte_size(Xml), Xml}} end,
    meck:expect(z_import_feed_http, get, Fetch),
    meck:new(z_import_feed_media, [passthrough]),
    meck:expect(z_import_feed_media, queue, fun(_, _, _, _) -> ok end),
    try
        {ok, Feed} = m_import_feed:get(FeedId, Context),
        ?assertEqual(imported, z_import_feed_import:run(Entry, Feed, Context)),
        ?assertEqual(imported, z_import_feed_import:run(Entry#{uri => Absent, media => [], body => <<>>}, Feed, Context)),
        ?assertEqual(imported, z_import_feed_import:run(SharedEntry, Feed, Context)),
        delete_restore_resources([Present, Absent, Media], Context),
        {ok, FeedId} = m_import_feed:save(FeedId, Input#{<<"refetch_deleted">> => true}, Context),
        %% An ordinary save must preserve an explicit request not yet claimed.
        {ok, FeedId} = m_import_feed:save(FeedId, Input, Context),
        ok = z_import_feed_poll:run(Context),
        {ok, After} = m_import_feed:get(FeedId, Context),
        ?assertEqual(undefined, maps:get(last_error, After)),
        ?assertEqual(false, maps:get(refetch_deleted, After)),
        ?assertEqual(true, maps:get(is_paused, After)),
        ?assertEqual(1, maps:get(polls, After)),
        ?assert(is_integer(m_rsc:rid(Present, Context))),
        ?assert(is_integer(m_rsc:rid(Media, Context))),
        ?assertEqual(undefined, m_rsc:rid(Absent, Context)),
        SharedId = m_rsc:rid(Shared, Context),
        RestoredMediaId = m_rsc:rid(Media, Context),
        ?assertNotEqual(nomatch, binary:match(m_rsc:p(SharedId, body, Context),
            iolist_to_binary(["<!-- z-media ", integer_to_binary(RestoredMediaId), " "]))),
        ?assertEqual(skipped, z_import_feed_import:run(Entry, After#{refetch_deleted_once => true}, Context)),
        %% A deleted dependent must be restored even if its article is unchanged.
        delete_restore_resources([Media], Context),
        ?assertEqual(updated, z_import_feed_import:run(Entry, After#{refetch_deleted_once => true}, Context)),
        ?assert(is_integer(m_rsc:rid(Media, Context))),
        delete_restore_resources([Present, Media], Context),
        %% A subsequent regular poll must preserve the new deletion.
        ok = m_import_feed:pause(FeedId, false, Context),
        ok = z_import_feed_poll:run(Context),
        ?assertEqual(undefined, m_rsc:rid(Present, Context)),
        ?assertEqual(undefined, m_rsc:rid(Media, Context)),
        %% Even a failed fetch consumes the one-fetch request without erasing tombstones.
        {ok, FeedId} = m_import_feed:save(FeedId, Input#{<<"refetch_deleted">> => true}, Context),
        meck:expect(z_import_feed_http, get, fun(_, _) -> {error, timeout} end),
        ok = z_import_feed_poll:run(Context),
        {ok, Failed} = m_import_feed:get(FeedId, Context),
        ?assertEqual(false, maps:get(refetch_deleted, Failed)),
        ?assertEqual(1, z_db:q1("select count(*) from import_feed_resource where uri=$1 and rsc_id is null", [Present], Context)),
        meck:expect(z_import_feed_http, get, Fetch),
        ok = m_import_feed:pause(FeedId, false, Context),
        ok = z_import_feed_poll:run(Context),
        ?assertEqual(undefined, m_rsc:rid(Present, Context))
    after
        meck:unload([z_import_feed_http, z_import_feed_media]),
        delete_restore_resources([Present, Absent, Shared, Media], Context),
        z_db:q("delete from import_feed where id=$1", [FeedId], Context),
        z_db:q("delete from import_feed_resource where uri in ($1,$2,$3,$4)", [Present, Absent, Shared, Media], Context),
        m_rsc:delete(User, Context),
        m_acl_rule:delete_managed(?MODULE, Context)
    end.

delete_restore_resources(Uris, Context) ->
    lists:foreach(fun(Uri) ->
        case m_rsc:rid(Uri, Context) of
            undefined -> ok;
            Id -> m_rsc:delete(Id, Context)
        end
    end, Uris).

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
        ?assertEqual({rollback, native_depth_ok}, Result)
    end, [0, 1, 2]).

native_edge(Uri) ->
    #{<<"about">> => #{<<"predicate">> => #{<<"name">> => <<"about">>},
        <<"objects">> => [#{<<"object_id">> => #{<<"uri">> => Uri, <<"is_a">> => [<<"article">>]}}]}}.

native_export(Uri, Edges) ->
    #{<<"uri">> => Uri, <<"is_a">> => [<<"article">>],
        <<"resource">> => #{<<"title">> => Uri}, <<"edges">> => Edges}.

%% Embedded media follow the same placeholder/task path, without HTTP requests.
data_media_test_() -> {timeout, 120, fun data_media/0}.
data_media() ->
    Context = z_acl:logon(1, z_context:new(zotonic_site_testsandbox)),
    ok = z_module_manager:activate_await(mod_import_feed, Context),
    ok = m_import_feed:install(Context),
    User = import_user(Context),
    {ok, FeedId} = m_import_feed:save(undefined, #{
        <<"user_id">> => User, <<"url">> => <<"https://example.org/data-feed.xml">>,
        <<"category_id">> => m_rsc:rid(article, Context),
        <<"media_mode">> => <<"dependent">>, <<"is_paused">> => true}, Context),
    DataUrl = <<"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+ip1sAAAAASUVORK5CYII=">>,
    MediaUri = z_import_feed_media:identity(DataUrl),
    Uri = <<"https://example.org/data-feed/article">>,
    Xml = <<"<rss><channel><item><guid>", Uri/binary, "</guid><link>", Uri/binary,
        "</link><title>Embedded image</title><description><![CDATA[<img src='", DataUrl/binary,
        "'>]]></description></item></channel></rss>">>,
    Parent = self(),
    meck:new(z_pivot_rsc, [passthrough]),
    meck:expect(z_pivot_rsc, insert_task, fun
        (z_import_feed_media, download_task, _Key, Args, _) ->
            Parent ! {media_task, Args}, {ok, 1};
        (Module, Function, Key, Args, TaskContext) ->
            meck:passthrough([Module, Function, Key, Args, TaskContext])
    end),
    try
        {ok, Parsed} = z_import_feed_parse:parse(Xml, <<"https://example.org/data-feed.xml">>),
        [Entry] = maps:get(entries, Parsed),
        {ok, Feed} = m_import_feed:get(FeedId, Context),
        ?assertEqual(imported, z_import_feed_import:run(Entry, Feed, Context)),
        Id = m_rsc:rid(Uri, Context),
        [MediaId] = m_edge:objects(Id, depiction, Context),
        ?assertEqual(MediaUri, m_rsc:p(MediaId, uri, Context)),
        ?assertEqual(undefined, m_media:get(MediaId, Context)),
        ?assertNotEqual(nomatch, binary:match(m_rsc:p(Id, body, Context), <<"<!-- z-media ">>)),
        ?assertEqual(ok, run_media_task(Context)),
        ?assertMatch(#{<<"mime">> := <<"image/png">>}, m_media:get(MediaId, Context)),
        ?assert(m_rsc:is_a(MediaId, image, Context)),
        ?assertEqual(skipped, z_import_feed_import:run(Entry, Feed, Context)),
        ?assertEqual([MediaId], m_edge:objects(Id, depiction, Context)),
        ok = m_rsc:delete(Id, Context),
        case m_rsc:exists(MediaId, Context) of true -> m_rsc:delete(MediaId, Context); false -> ok end
    after
        meck:unload(z_pivot_rsc),
        m_rsc:delete(User, Context),
        m_acl_rule:delete_managed(?MODULE, Context),
        z_db:q("delete from import_feed where id=$1", [FeedId], Context),
        z_db:q("delete from import_feed_resource where uri in ($1,$2)", [Uri, MediaUri], Context)
    end.

poll_schedule_test_() -> {timeout, 60, fun poll_schedule/0}.

poll_schedule() ->
    C = z_acl:logon(1, z_context:new(zotonic_site_testsandbox)),
    ok = m_import_feed:install(C),
    User = import_user(C),
    {ok, FeedId} = m_import_feed:save(undefined, #{
        <<"user_id">> => User, <<"url">> => <<"https://example.org/schedule.xml">>,
        <<"title">> => <<"Schedule">>, <<"category_id">> => m_rsc:rid(article, C),
        <<"poll_minutes">> => 60, <<"is_paused">> => true}, C),
    ok = meck:new(z_import_feed_http, [passthrough]),
    meck:expect(z_import_feed_http, get_feed, fun(Settings, FetchContext) ->
        z_import_feed_http:get(maps:get(url, Settings), FetchContext)
    end),
    Run = fun(Result, Failures, Seconds) ->
        meck:expect(z_import_feed_http, get, fun(_, _) -> Result end),
        z_db:q("update import_feed set refetch_deleted=true, next_poll='2000-01-01' where id=$1", [FeedId], C),
        ok = z_import_feed_poll:run(C),
        ?assertEqual({Failures, true}, z_db:q_row("select failure_count,
            next_poll > now() + ($2-2)*interval '1 second'
            and next_poll <= now() + $2*interval '1 second'
            from import_feed where id=$1", [FeedId, Seconds], C))
    end,
    Success = fun(Hint) ->
        Xml = <<"<rss><channel>", Hint/binary, "</channel></rss>">>,
        {ok, {<<"https://example.org/schedule.xml">>, [], byte_size(Xml), Xml}}
    end,
    try
        Run(Success(<<"<ttl>180</ttl>">>), 0, 10800),
        Run({error, timeout}, 1, 10800),
        Run({error, timeout}, 2, 14400),
        Run({error, {http_status, 429, {retry_after, 86400}}}, 3, 86400),
        Run({error, {http_status, 503}}, 4, 43200),
        Run(Success(<<"<ttl>5</ttl>">>), 0, 3600),
        Run({error, timeout}, 1, 7200),
        Run(Success(<<>>), 0, 3600),
        ?assertEqual(0, z_db:q1("select poll_hint_seconds from import_feed where id=$1", [FeedId], C)),
        % Upgrade old rows without losing their settings; rerunning is harmless.
        ok = z_db:transaction(fun(Ctx) ->
            z_db:q("alter table import_feed drop column failure_count, drop column poll_hint_seconds", Ctx),
            z_db:flush(Ctx),
            ok = m_import_feed:install(Ctx),
            ok = m_import_feed:install(Ctx),
            ?assertEqual({60, 0, 0}, z_db:q_row("select poll_minutes, failure_count, poll_hint_seconds
                from import_feed where id=$1", [FeedId], Ctx)),
            ok
        end, C)
    after
        meck:unload(z_import_feed_http),
        z_db:q("delete from import_feed where id=$1", [FeedId], C)
    end.

conditional_poll_test_() -> {timeout, 60, fun conditional_poll/0}.
conditional_poll() ->
    C = z_acl:logon(1, z_context:new(zotonic_site_testsandbox)),
    ok = m_import_feed:install(C),
    User = import_user(C),
    Url = <<"https://example.org/conditional.xml">>,
    {ok, FeedId} = m_import_feed:save(undefined, #{
        <<"user_id">> => User, <<"url">> => Url, <<"title">> => <<"Conditional">>,
        <<"category_id">> => m_rsc:rid(article, C), <<"is_paused">> => true}, C),
    meck:new(z_fetch, [passthrough]),
    Run = fun(Result, ExpectedHeaders) ->
        meck:expect(z_fetch, fetch, fun(U, Options, _) when U =:= Url ->
            ?assertEqual(ExpectedHeaders, proplists:get_value(headers, Options)),
            Result
        end),
        z_db:q("update import_feed set is_paused=false, next_poll='2000-01-01' where id=$1", [FeedId], C),
        ok = z_import_feed_poll:run(C)
    end,
    ETag = <<"\"version-1\"">>,
    Modified = <<"Wed, 01 Jan 2020 00:00:00 GMT">>,
    Conditional = [{<<"if-none-match">>, ETag}, {<<"if-modified-since">>, Modified}],
    Xml = <<"<rss><channel><ttl>180</ttl></channel></rss>">>,
    Response = {ok, {Url, [{"etag", ETag}, {"last-modified", Modified}], byte_size(Xml), Xml}},
    try
        Run(Response, []),
        ?assertEqual({ETag, Modified, Url, true}, z_db:q_row("select http_etag,
            http_last_modified, http_url, http_settings_version=modified::text
            from import_feed where id=$1", [FeedId], C)),
        z_db:q("update import_feed set imported=42 where id=$1", [FeedId], C),
        Run({error, {304, Url, [], 0, <<>>}}, Conditional),
        ?assertEqual({42, 0, 0, 10800, ETag}, z_db:q_row("select imported, errors,
            failure_count, poll_hint_seconds, http_etag from import_feed where id=$1", [FeedId], C)),
        Run({error, timeout}, Conditional),
        ?assertEqual(undefined, z_db:q1("select http_etag from import_feed where id=$1", [FeedId], C)),
        Run(Response, []),
        z_db:q("update import_feed set modified=modified+interval '1 second' where id=$1", [FeedId], C),
        Run(Response, []),
        z_db:q("update import_feed set refetch_deleted=true where id=$1", [FeedId], C),
        Run(Response, []),
        % Upgrade an old schema and repeat the upgrade in the same transaction.
        ok = z_db:transaction(fun(Ctx) ->
            z_db:q("alter table import_feed drop column http_etag, drop column http_last_modified,
                drop column http_url, drop column http_settings_version", Ctx),
            z_db:flush(Ctx),
            ok = m_import_feed:install(Ctx),
            ok = m_import_feed:install(Ctx),
            ?assertEqual({undefined, undefined, undefined, undefined}, z_db:q_row(
                "select http_etag,http_last_modified,http_url,http_settings_version
                from import_feed where id=$1", [FeedId], Ctx)),
            ok
        end, C)
    after
        meck:unload(z_fetch),
        z_db:q("delete from import_feed where id=$1", [FeedId], C),
        m_rsc:delete(User, C)
    end.

pagination_poll_test_() -> {timeout, 90, fun pagination_poll/0}.
pagination_poll() ->
    C = z_acl:logon(1, z_context:new(zotonic_site_testsandbox)),
    ok = m_import_feed:install(C),
    User = import_user(C),
    Url = <<"https://example.org/pagination">>,
    Next = <<Url/binary, "?page=2">>,
    Third = <<Url/binary, "?page=3">>,
    {ok, FeedId} = m_import_feed:save(undefined, #{
        <<"user_id">> => User, <<"url">> => Url,
        <<"category_id">> => m_rsc:rid(article, C), <<"is_paused">> => true}, C),
    meck:new(z_fetch, [passthrough]),
    meck:new(z_import_feed_import, [passthrough]),
    meck:expect(z_import_feed_import, run, fun(#{uri := Uri}, _, Context) ->
        z_db:q("insert into import_feed_item(feed_id,uri,status) values($1,$2,'ok')
            on conflict(feed_id,uri) do update set modified=now(),status='ok'",
            [FeedId, Uri], Context),
        imported
    end),
    Body = fun(Ids, NextUrl) -> z_json:encode(#{
        <<"version">> => <<"https://jsonfeed.org/version/1.1">>,
        <<"title">> => <<"Pagination">>, <<"next_url">> => NextUrl,
        <<"items">> => [#{<<"id">> => I, <<"content_text">> => I} || I <- Ids]}) end,
    Response = fun(U, Ids, N) ->
        B = Body(Ids, N), {ok, {U, [{"etag", "first-page"}], byte_size(B), B}}
    end,
    Run = fun() ->
        z_db:q("update import_feed set is_paused=false,next_poll='2000-01-01' where id=$1", [FeedId], C),
        ok = z_import_feed_poll:run(C)
    end,
    Reset = fun(All) ->
        z_db:q("delete from import_feed_item where feed_id=$1", [FeedId], C),
        z_db:q("update import_feed set last_success=null,next_url=null,pagination_started=null,
            http_etag=null,http_last_modified=null,http_settings_version=null,
            imported=0,errors=0,import_all_pages=$2 where id=$1", [FeedId, All], C)
    end,
    try
        %% Default initial import does not request the advertised second page.
        meck:expect(z_fetch, fetch, fun(U, _, _) when U =:= Url -> Response(U, [<<"a">>], Next) end),
        Run(),
        ?assertEqual(1, z_db:q1("select imported from import_feed where id=$1", [FeedId], C)),
        ?assertEqual(undefined, z_db:q1("select next_url from import_feed where id=$1", [FeedId], C)),
        %% Deleted entries remain seen; process the whole page, then stop.
        z_db:q("update import_feed_item set status='deleted' where feed_id=$1", [FeedId], C),
        meck:expect(z_fetch, fetch, fun(U, _, _) when U =:= Url -> Response(U, [<<"new">>, <<"a">>], Next) end),
        Run(),
        ?assertEqual(3, z_db:q1("select imported from import_feed where id=$1", [FeedId], C)),
        %% Initial opt-in follows all pages. A failed continuation is durable.
        Reset(true),
        meck:expect(z_fetch, fetch, fun
            (U, _, _) when U =:= Url -> Response(U, [<<"a">>], Next);
            (U, Options, _) when U =:= Next ->
                ?assertEqual(undefined, proplists:get_value(headers, Options)),
                {error, timeout}
        end),
        Run(),
        ?assertEqual({Next, undefined, undefined}, z_db:q_row(
            "select next_url,last_success,http_etag from import_feed where id=$1", [FeedId], C)),
        %% Retry starts at page two, never at the conditionally cached first page.
        meck:expect(z_fetch, fetch, fun
            (U, _, _) when U =:= Next -> Response(U, [<<"b">>], Third);
            (U, _, _) when U =:= Third -> Response(U, [<<"c">>], <<>>)
        end),
        Run(),
        ?assertEqual({3, undefined, true, undefined}, z_db:q_row(
            "select imported,next_url,last_success is not null,http_etag from import_feed where id=$1", [FeedId], C)),
        %% Catch-up traverses unseen pages, then processes the complete seen page.
        meck:expect(z_fetch, fetch, fun
            (U, _, _) when U =:= Url -> Response(U, [<<"d">>], Next);
            (U, _, _) when U =:= Next -> Response(U, [<<"a">>, <<"e">>], Third)
        end),
        Run(),
        ?assertEqual(6, z_db:q1("select imported from import_feed where id=$1", [FeedId], C)),
        %% A cyclic next link fails without spinning or committing validators.
        Reset(true),
        meck:expect(z_fetch, fetch, fun(U, _, _) when U =:= Url -> Response(U, [<<"a">>], Url) end),
        Run(),
        ?assertEqual({1, undefined}, z_db:q_row("select errors,http_etag from import_feed where id=$1", [FeedId], C)),
        %% Fresh installation already includes these fields; upgrades are repeatable.
        ok = z_db:transaction(fun(Ctx) ->
            z_db:q("alter table import_feed drop column import_all_pages, drop column next_url,
                drop column pagination_started", Ctx),
            z_db:flush(Ctx),
            ok = m_import_feed:install(Ctx),
            ok = m_import_feed:install(Ctx),
            ?assertEqual({false, undefined, undefined}, z_db:q_row(
                "select import_all_pages,next_url,pagination_started from import_feed where id=$1", [FeedId], Ctx)),
            ok
        end, C)
    after
        meck:unload([z_fetch, z_import_feed_import]),
        z_db:q("delete from import_feed where id=$1", [FeedId], C),
        m_rsc:delete(User, C)
    end.
