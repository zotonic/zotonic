%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Discover, preview and continuously import RSS, Atom and JSON Feed articles.
%% @end

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

-module(mod_import_feed).

-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Import feeds").
-mod_description("Import articles, events and media from RSS, Atom, JSON Feed and iCalendar sources.").
-mod_depends([admin, cron]).
-mod_schema(11).
-mod_config([]).

-moduledoc(#{
    zotonic_keywords => [
        "reference", "integrator", "module", "import_and_migration", "import", "schedule", "url"
    ]
}).
-moduledoc("
Discover RSS 2.0, RSS 1.0/RDF, Atom, JSON Feed 1/1.1 and iCalendar feeds and automatically import articles and media.

Administrator accounts cannot run imports. Select an enabled non-administrator account
with only the required import permissions. This is checked when saving settings and
before polling, including for existing feeds whose account is later promoted to admin.

## Setup and administration

Enable **Import feeds** in Modules and open **Content > Import feeds**. The module
requires `mod_admin` and `mod_cron`. Enter a website or feed URL, select one of the
discovered feeds, and review three example entries beside the import settings.
Discovery uses HTML alternate links and HTTP Link headers.

Managing feeds requires `use` permission for `mod_import_feed`. New feeds default to the current editor. Administrators and users with `use` permission
for `mod_admin_identity` can select another enabled account with **Select user**.
Saving records that account and its user groups for background imports; ordinary
settings edits preserve the selected account and saved groups. The worker checks that the
editor is still enabled and uses the editor's current, bounded permissions. Resource,
media and edge operations enforce their normal ACLs; imported HTML is sanitized.

## Per-feed settings

Settings are stored in the feed table. There are no `m_config` options.

| Setting | Behavior |
| --- | --- |
| Import user | Defaults to the creator. Only administrators or users with full account access can select or change it. |
| Default category | An existing non-meta category; initially Article. |
| Content group | Available when `mod_content_groups` is enabled. Empty uses the site default. |
| Guess category | Match tags against existing category names or translated titles, case-insensitively; otherwise use the default category. |
| Default connections | Use Add connection to select a predicate and page. Remove entries from the list as needed, then save. Existing article connections are preserved. |
| Media | Dependent resources with depiction edges, the most important medium attached directly, or no media import. |
| Fetch using | Anonymous by default, or registered OAuth2 credentials for the import user and destination host. |
| Polling interval | 5 to 10080 minutes; initially 60 minutes. |
| Authority | Non-authoritative by default; authoritative copies also retain source identities. |
| WordPress enrichment | Enabled initially in the admin form; use the anonymous public REST API for content, author, dates and featured media. Failures fall back to feed data. |
| Pause | Suspend automatic imports. New feeds are active unless saved paused. |

Imports create published resources and media placeholder resources. Inline images
become `z-media` embeds referring to these stable resource IDs. After saving the
article and its connections, the importer queues persistent `z_pivot_rsc` tasks
for the downloads. Attached media use the article itself as their placeholder.
The tasks use `m_media` to identify each downloaded file and select its media
category; downloads do not hold a database transaction.

Each task rechecks the feed's current import account and fetch credentials, resource
permissions, pending URL and existing medium. Deleted resources and locally replaced
media are not overwritten. Download failures retry up to sixteen times after the initial attempt. Delays
double from one minute up to a fixed twelve-hour cap: 1, 2, 4, 8, 16, 32, 64,
128, 256 and 512 minutes, followed by six twelve-hour waits (89 hours 3 minutes
total, excluding request execution and scheduling delays). A longer `Retry-After`
takes precedence. These limits are fixed, not module configuration. Permission failures stop immediately. The feed activity
log shows queued, started, retry, success and error events. A terminal failure leaves
an empty placeholder so the article remains imported and a later reimport can retry.
Inline images can use `data:` URLs, including base64 and percent-encoded data.
They follow the same deferred media import path and become `z-media` embeds.
A SHA-256 identity of the source URL keeps payloads out of resource URI indexes
and activity logs, and lets repeated imports reuse the same resource. Embedded
media have the same 20 MiB size limit as downloaded media.

Native Zotonic resource feeds already use the core importer's persistent media tasks.

For each inline image, the importer selects one variant, preferring the largest
`srcset` width or density, with `src` as fallback. WordPress featured-image variants
reuse the original image. In attached mode, other inline images retain their source URLs.

## Fetch authentication and development

Discovery can use the current user's registered OAuth2 consumer credentials.
Saved feeds use their selected import user's credentials for previews and polling.
Tokens are resolved per destination host and port, only over HTTPS, and are never
stored in feed settings. Missing credentials leave a request anonymous. Native
imports save their fetch preference for core reimports and medium downloads.

Development sites allow local destinations and self-signed TLS certificates.
Transport, redirects and certificate checks use the shared `z_fetch` routines;
browser input cannot change the server's environment. OAuth2 credentials are never
sent over HTTP. Anonymous fetching remains anonymous across redirects.

## Polling and identity

Polling stores the source's `ETag` and `Last-Modified` after a successful
import and sends both validators on subsequent requests. HTTP 304 is a
successful unchanged poll: it does not parse a body, import entries, or remove
existing content, and preserves the publisher's polling interval. Validator
updates are protected by the poll lease and settings version. Failed imports,
settings changes, manual reimport and restoring deleted resources require a
fresh response. Previews and individual-resource fetches remain unconditional.

Atom and RSS `rel=next` links and JSON Feed `next_url` are followed. The initial
import reads only the first page unless **Import all pages on the initial import**
is enabled (`import_all_pages`, default false). This option can import large
amounts of articles and media.

Later polls process each complete page and stop following links when that page
contains a previously successfully imported entry. Deleted tracked entries also
count as seen. This fetches new published entries, but is not a history or change
synchronization mechanism: older edits and backdated articles may be missed.

Continuation requests use the same URL, credential and response-size checks,
without first-page HTTP validators. Failed pages resume from their saved URL.
Each worker processes at most 100 pages, then schedules a continuation. Loops are
rejected within a traversal batch. Settings saves restart from the first page.
Validators are saved only for completed single-page polls; pending or failed
traversals cannot be hidden by a first-page HTTP 304.

Polling uses `poll_minutes` as a minimum interval (default 60 minutes; range
5–10080). RSS `ttl` and namespace-qualified syndication `updatePeriod` /
`updateFrequency` hints can lengthen it. When both are present, the longer hint
wins. Missing syndication frequency defaults to 1; a frequency without a period
uses daily. Monthly/yearly periods conservatively use 31/366 days, and division
rounds up to whole seconds. Invalid hints are ignored. Schedule alignment via
`updateBase`, RSS `skipHours`/`skipDays`, and HTTP cache freshness are not used.

Consecutive feed-fetch or parse failures double the configured interval on each
failure, with a fixed 12-hour backoff cap. Neither that cap nor backoff can shorten
`poll_minutes` or the last valid publisher interval. A successful fetch and parse
resets the failure counter and replaces the stored publisher hint, restoring the
normal frequency even if individual entries fail to import. Counters, publisher
hints, and next-poll times survive restarts. There is no automatic give-up deadline.

HTTP 429/503 responses honor `Retry-After` as seconds or an HTTP date. The next
poll waits for the longest of the normal interval, failure backoff, and that delay.
Past dates mean no additional delay; malformed, duplicate or out-of-range values
are ignored. Parsed delays must fit 0–2147483647 seconds. Entry-fetch errors can
also extend the next poll; media download retries honor the same header while
retaining their sixteen-retry limit. Headers and response bodies are not
saved in error messages. Explicit **Poll now** remains an immediate override.
No new module configuration is required; `poll_minutes` remains the per-feed control.


The minute tick starts up to five background workers per site and Erlang node.
Each worker polls up to five due feeds in sequence. Database leases prevent two
workers from polling the same feed concurrently. The overview shows article counts, cumulative import/update/skip/error
statistics and poll times. Each feed has settings, pause/resume, test/preview, poll-now,
reimport and paginated history for articles and dependent media.

Stable source URIs and checksums allow repeated polls without duplicates. Reimport
processes only entries currently present in the feed, preserving resource IDs. Deleted
resources remain deleted. Media already imported from the same URL is reused. Feeds
with more than 100 entries are rejected, rather than silently importing a partial feed.
WebSub hub links are recognized in feed data; feed fetching uses polling.

**Save & refetch deleted** saves the settings and requests one fetch that can
restore deleted articles and their media when present in the current feed. It
also fetches a paused feed once, without resuming its regular polling. The request
is consumed when a worker claims the feed, including if fetching fails; subsequent
polls preserve deleted resources as usual. Missing entries are never restored.

Imports lock their source URIs across connected Erlang nodes, so independent feeds
can progress concurrently while shared articles and media are serialized. Network
requests and the complete article import do not hold a database transaction;
resource and media APIs manage their own write transactions. The `rsc_update`
observer writes ordinary feed resources' source mappings and feed membership in
that same transaction, including authoritative copies and media placeholders.
Completion checksums are recorded only after the entry finishes. A failed import can
leave successfully imported resources in place for reuse on retry.

Deleting imported content checks each resource's delete permission and protection.
Dependent cleanup follows the normal resource deletion rules; a dependent does
not prevent deletion of its parent.

## iCalendar feeds

Discover `text/calendar` links or enter an `.ics` feed URL directly. VEVENTs
must have a UID, UTC DTSTAMP and valid DTSTART. Invalid events are skipped;
malformed calendar structure is rejected. Dates, durations and exclusive all-day
end dates are normalized to Zotonic event properties. UTC and known IANA TZIDs
are supported; floating times are interpreted as UTC. Custom VTIMEZONE rules
are not evaluated, and events with unknown TZIDs are skipped. Cancelled events
are unpublished. With category guessing enabled, calendar entries use `event`;
otherwise the configured category applies.

Recurrence rules and recurrence IDs are retained as `ical_rrule` and
`ical_recurrence_id`; occurrences are not expanded. Exceptions have separate
identities derived from UID and RECURRENCE-ID. Alarms are ignored.

Zotonic calendar exports advertise `X-ZOTONIC-RESOURCE`, using the same native
resource import and optional per-resource WebSub subscriptions as Atom/RSS/JSON
Feed. Generic calendar feeds continue to use polling; feed-level WebSub push
subscriptions are not provided. `X-ZOTONIC-NEXT` is followed using the existing
first-page/initial-import-all-pages and stop-at-seen-entry policies.

## Zotonic resource feeds

Atom and RSS entries from `mod_export` advertise a JSON resource link with
relation `https://zotonic.com/rel/resource`. JSON Feed items advertise the same
endpoint in `_zotonic.resource_url`. These use `m_rsc_import`, preserving native
properties, categories, media embeds and connections. Connection import depth
defaults to 1 and accepts 0–10. Content-group/copyright defaults and cleanup
patterns apply; category guessing and media-layout choices do not reshape native
resources. Choosing no media disables native medium downloads.

With `mod_websub` enabled, optional subscriptions cover imported articles and,
when selected, their connections within the import depth. Both options default
to false and require non-authoritative imports and a supporting source. Existing
subscriptions are managed separately through WebSub administration. Native
resources use the core Fetch new version flow and saved import options, without
requiring the entry to remain in the feed. These refreshes use the core import
notification with native resource source metadata.

## Fetch new version

For ordinary feed entries, the `rsc_import_fetch` observer refreshes known imported articles from their current
feed, using the saved settings and import account. The caller needs feed-management
and article-edit permission. Paused feeds can be refreshed manually without resuming
polling. This bypasses unchanged checksums and still runs the import notification.
An article no longer present in the feed returns `feed_entry_not_found`; unknown
URIs fall through to the standard resource importer.

## Import notification

Before writing an entry or its media, the importer dispatches the core
`#import_resource{item, related, source, options, raw}` notification. The source
is `#{importer => mod_import_feed, feed => Settings}`. Include
`zotonic_core/include/zotonic.hrl`, declare `zotonic_observer`, and implement
`observe_import_resource/2` or its optional PID-aware variant. See the core
observer documentation for the payload, permissions and result contract.

Unchanged and deleted entries are skipped before notifying. Native feed entries
notify once; their delegation to `m_rsc_import` preserves this scope, while
recursively imported resources may generate separate notifications.

").

-define(DEFAULT_POLL_MINUTES, 60).
-define(DEFAULT_IMPORT_DEPTH, 1).
-define(PREVIEW_ENTRIES, 3).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_wires/include/mod_wires.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").

-export([observe_rsc_update/3, observe_import_feed_log/2, observe_acl_is_allowed/2, observe_tick_24h/2, observe_admin_menu/3, observe_tick_1m/2, observe_rsc_import_fetch/2, manage_schema/2, event/2]).

%% @doc Persist feed identity and membership in the resource's own transaction.
%% Only the importer can set this scoped context marker. Ordinary editor updates
%% and unrelated resources created by other observers must not acquire membership.
-spec observe_rsc_update(#rsc_update{}, {ok, m_rsc:props()} | {error, term()}, z:context()) ->
    {ok, m_rsc:props()} | {error, term()}.
observe_rsc_update(#rsc_update{id = Id}, {ok, #{<<"feed_source_uri">> := Uri}} = Acc, Context) ->
    case z_context:get(import_feed_identity, Context) of
        {FeedId, Uri, Kind} ->
            case z_import_feed_import:track_resource(FeedId, Uri, Kind, Id, Context) of
                ok -> Acc;
                {error, _} = Error -> Error
            end;
        _ -> Acc
    end;
observe_rsc_update(_, Acc, _) -> Acc.

%% @doc Refresh known feed articles through the feed's settings and import account.
-spec observe_rsc_import_fetch(#rsc_import_fetch{}, z:context()) ->
    undefined | {ok, integer()} | {error, term()}.
observe_rsc_import_fetch(#rsc_import_fetch{uri = Uri}, Context) ->
    case z_context:get(import_feed_native, Context) of
        undefined -> m_import_feed:fetch_resource(Uri, Context);
        Native -> z_import_feed_native:fetch(Uri, Native, Context)
    end.

-spec observe_admin_menu(#admin_menu{}, list(), z:context()) -> list().
observe_admin_menu(#admin_menu{}, Acc, Context) ->
    [
        #menu_item{
            id = admin_import_feed,
            parent = admin_content,
            label = ?__("Import feeds", Context),
            url = {admin_import_feed, []},
            visiblecheck = {acl, use, mod_import_feed}
        }
        | Acc
    ].

-spec observe_tick_1m(tick_1m, z:context()) -> ok.
observe_tick_1m(tick_1m, Context) ->
    lists:foreach(fun(Name) ->
        _ = z_sidejob:start_site_unique(Name, z_import_feed_poll, run, [], Context)
    end, [mod_import_feed, mod_import_feed_2, mod_import_feed_3,
          mod_import_feed_4, mod_import_feed_5]),
    ok.

%% Internal notification, delayed by z_notifier until the transaction commits.
-spec observe_import_feed_log({import_feed_log, integer(), integer()}, z:context()) -> ok.
observe_import_feed_log({import_feed_log, FeedId, LogId}, Context) ->
    %% A savepoint rollback can discard a queued row without clearing the queue.
    case z_db:q1("select id from import_feed_log where id=$1 and feed_id=$2", [LogId, FeedId], Context) of
        LogId ->
            _ = z_mqtt:publish(<<"model/import_feed/event/log">>,
                #{feed_id => FeedId, log_id => LogId}, Context),
            ok;
        undefined -> ok
    end.

%% Live activity notices are restricted to feed managers, like the log model.
observe_acl_is_allowed(#acl_is_allowed{action = subscribe,
        object = #acl_mqtt{topic = [<<"model">>, <<"import_feed">>, <<"event">>, <<"log">>]}}, Context) ->
    m_import_feed:allowed(Context);
observe_acl_is_allowed(_, _) -> undefined.

observe_tick_24h(tick_24h, Context) ->
    m_import_feed_log:cleanup(Context).

-spec manage_schema(install | {upgrade, integer()}, z:context()) -> ok.
manage_schema(_, Context) ->
    m_import_feed:install(Context).

-spec event(#submit{} | #postback{}, z:context()) -> z:context().
event(Event, Context) ->
    %% These actions run when the response arrives, including permission errors.
    RenderContext = case Event of
        #submit{message = discover} ->
            fetch_complete("feed-discover-form", Context);
        #postback{message = {select, _}} ->
            fetch_complete("feed-discovery", Context);
        #postback{message = {test, _}} ->
            z_render:wire([
                {remove_class, [{target, "feed-preview"}, {class, "is-loading"}]},
                {enable, [{target, "feed-test"}]}
            ], Context);
        _ ->
            Context
    end,
    case m_import_feed:allowed(RenderContext) of
        true -> handle(Event, RenderContext);
        false -> z_render:growl_error(?__("You are not allowed to manage feeds.", RenderContext), RenderContext)
    end.

%% Restore controls on success, fetch errors and permission-denied responses.
fetch_complete(Target, Context) ->
    z_render:wire([
        {remove_class, [{target, Target}, {class, "is-loading"}]},
        {enable, [{selector, ".import-feed-fetch-button"}]}
    ], Context).

handle(#postback{message = add_connection}, Context) ->
    z_render:dialog(?__("Add default connection", Context),
        "_admin_import_feed_connection_dialog.tpl", [], Context);
handle(#submit{message = select_connection_predicate}, Context) ->
    Predicate = m_rsc:rid(z_context:get_q(<<"predicate">>, Context), Context),
    case m_rsc:is_a(Predicate, predicate, Context) andalso z_acl:rsc_visible(Predicate, Context) of
        true ->
            z_render:dialog(
                ?__("Select connected page", Context),
                "_action_dialog_connect.tpl",
                [
                    {intent, <<"select">>},
                    {id, Predicate},
                    {tabs_enabled, [<<"find">>]},
                    {delegate, mod_import_feed},
                    {width, <<"large">>}
                ],
                Context);
        false ->
            failure(invalid_connections, Context)
    end;
handle(#postback{message = {admin_connect_select, Args}}, Context) ->
    case m_import_feed:validate_connection(proplists:get_value(id, Args),
            z_context:get_q(<<"select_id">>, Context), Context) of
        {ok, {Predicate, Object}} ->
            UpdatedContext = z_render:insert_bottom(
                "feed-connections",
                #render{
                    template = "_admin_import_feed_connection.tpl",
                    vars = [{predicate, Predicate}, {object, Object}]
                },
                Context),
            z_render:wire({dialog_close, []}, UpdatedContext);
        {error, Reason} ->
            failure(Reason, Context)
    end;
handle(#postback{message={select_user, _Args}}, Context) ->
    case m_import_feed:can_select_user(Context) andalso not z_acl:is_read_only(Context) of
        true ->
            z_render:dialog(
                ?__("Select import user", Context),
                "_admin_import_feed_user_dialog.tpl",
                [], Context);
        false ->
            failure(eacces, Context)
    end;
handle(#submit{message = search_users}, Context) ->
    case m_import_feed:can_select_user(Context) of
        true -> z_render:update(
            "feed-user-results",
            #render{
                template = "_admin_import_feed_user_results.tpl",
                vars = [{search_text, z_context:get_q(<<"search_text">>, Context)}]
            },
            Context);
        false ->
            failure(eacces, Context)
    end;
handle(#postback{message = {choose_user, Args}}, Context) ->
    case m_import_feed:validate_user(proplists:get_value(id, Args), Context) of
        {ok, User} ->
            UpdatedContext = z_render:update("feed-import-user", #render{
                template = "_admin_import_feed_user.tpl", vars = [{user_id, User}]}, Context),
            z_render:wire({dialog_close, []}, UpdatedContext);
        {error, Reason} ->
            failure(Reason, Context)
    end;
handle(#submit{message = discover}, Context) ->
    UseCredentials = z_convert:to_bool(z_context:get_q(<<"use_credentials">>, Context)),
    FetchContext = z_import_feed_http:context(#{use_credentials => UseCredentials}, Context),
    case m_import_feed:discover(z_context:get_q(<<"url">>, Context), FetchContext) of
        {ok, Feeds} ->
            z_render:update(
                "feed-discovery",
                #render{
                    template = "_admin_import_feed_discovery.tpl",
                    vars = [
                        {feeds, Feeds},
                        {use_credentials, UseCredentials}
                    ]
                },
                Context);
        {error, Reason} ->
            failure(Reason, Context)
    end;
handle(#postback{message = {select, Args}}, Context) ->
    Url = proplists:get_value(url, Args),
    UseCredentials = z_convert:to_bool(proplists:get_value(use_credentials, Args)),
    case m_import_feed:preview(#{url => Url, use_credentials => UseCredentials}, ?PREVIEW_ENTRIES, Context) of
        {ok, Preview} ->
            Feed = #{
                is_zotonic_feed => maps:get(is_zotonic_feed, Preview),
                use_credentials => UseCredentials,
                url => maps:get(url, Preview),
                title => maps:get(title, Preview),
                category_id => m_rsc:rid(article, Context),
                poll_minutes => ?DEFAULT_POLL_MINUTES,
                import_depth => ?DEFAULT_IMPORT_DEPTH,
                media_mode => <<"dependent">>,
                use_wordpress => true,
                connections => []
            },
            z_render:update(
                "feed-discovery",
                #render{
                    template = "_admin_import_feed_edit.tpl",
                    vars = [
                        {feed, Feed},
                        {preview, Preview}
                    ]
                },
                Context);
        {error, Reason} ->
            failure(Reason, Context)
    end;
handle(#submit{message = {save, Args}}, Context) ->
    Input = (z_context:get_q_map_noz(Context))#{<<"connections">> => z_context:get_q_all(<<"connection">>, Context)},
    case m_import_feed:save(proplists:get_value(id, Args), Input, Context) of
        {ok, Id} ->
            case proplists:get_value(id, Args) of
                undefined -> m_import_feed:record_feed_type(
                    #{id => Id, url => maps:get(<<"url">>, Input)},
                    #{is_zotonic_feed => proplists:get_value(is_zotonic_feed, Args)}, Context);
                _ -> ok
            end,
            case z_convert:to_bool(maps:get(<<"refetch_deleted">>, Input, false)) of
                true -> observe_tick_1m(tick_1m, Context);
                false -> ok
            end,
            z_render:wire({redirect, [{dispatch, admin_import_feed_edit}, {id, Id}]}, Context);
        {error, Reason} ->
            failure(Reason, Context)
    end;
handle(#postback{message = {test, Args}}, Context) ->
    case m_import_feed:get(proplists:get_value(id, Args), Context) of
        {ok, Feed} ->
            case m_import_feed:preview(Feed, ?PREVIEW_ENTRIES, Context) of
                {ok, Preview} ->
                    m_import_feed:record_feed_type(Feed, Preview, Context),
                    UpdatedContext = z_render:update("feed-preview", #render{template = "_admin_import_feed_preview.tpl",
                        vars = [{preview, Preview}, {feed, Feed}]}, Context),
                    case maps:get(is_zotonic_feed, Preview) of
                        undefined -> UpdatedContext;
                        IsZotonic ->
                            Action = case IsZotonic of true -> remove_class; false -> add_class end,
                            WordpressAction = case IsZotonic of true -> disable; false -> enable end,
                            z_render:wire([
                                {Action, [{target, "feed-zotonic-options"}, {class, "hidden"}]},
                                {WordpressAction, [{target, "feed-use-wordpress"}]}
                            ], UpdatedContext)
                    end;
                {error, Reason} ->
                    failure(Reason, Context)
            end;
        {error, Reason} ->
            failure(Reason, Context)
    end;
handle(#postback{message = {pause, Args}}, Context) ->
    result(m_import_feed:pause(proplists:get_value(id, Args), proplists:get_value(paused, Args), Context), Context);
handle(#postback{message = {log_fetch, Args}}, Context) ->
    case m_import_feed:get(proplists:get_value(id, Args), Context) of
        {ok, #{is_paused := true}} -> failure(feed_paused, Context);
        {ok, #{is_running := true}} -> failure(feed_is_running, Context);
        {ok, Feed} ->
            case m_import_feed:queue(maps:get(id, Feed), false, Context) of
                ok -> z_render:growl(?__("Feed queued for fetching. Watch the activity log for results.", Context), Context);
                {error, Reason} -> failure(Reason, Context)
            end;
        {error, Reason} -> failure(Reason, Context)
    end;
handle(#postback{message = {poll, Args}}, Context) ->
    result(m_import_feed:queue(proplists:get_value(id, Args), proplists:get_value(reimport, Args, false), Context), Context);
handle(#postback{message = {delete_content, Args}}, Context) ->
    case m_import_feed:delete_content(proplists:get_value(id, Args), Context) of
        {ok, Counts} ->
            z_render:dialog(
                ?__("Imported content deleted", Context),
                "_admin_import_feed_delete_result.tpl",
                [{counts, Counts}],
                Context);
        {error, Reason} ->
            failure(Reason, Context)
    end;
handle(#postback{message = {delete_feed_dialog, Args}}, Context) ->
    case m_import_feed:get(proplists:get_value(id, Args), Context) of
        {ok, Feed} ->
            z_render:dialog(
                ?__("Delete feed?", Context),
                "_admin_import_feed_delete_dialog.tpl",
                [{feed, Feed}],
                Context);
        {error, Reason} ->
            failure(Reason, Context)
    end;
handle(#submit{message = {delete_feed, Args}}, Context) ->
    DeleteContent = z_context:get_q(<<"delete_content">>, Context) =:= <<"1">>,
    case m_import_feed:delete_feed(proplists:get_value(id, Args), DeleteContent, Context) of
        {ok, Counts} ->
            z_render:dialog(
                ?__("Feed deleted", Context),
                "_admin_import_feed_delete_result.tpl",
                [
                    {counts, Counts},
                    {feed_deleted, true},
                    {delete_content, DeleteContent}
                ],
                Context);
        {error, Reason} ->
            failure(Reason, Context)
    end;
handle(_, Context) -> Context.

result(ok, Context) ->
    z_render:wire({reload, []}, Context);
result({error, Reason}, Context) ->
    failure(Reason, Context).
failure(Reason, Context) ->
    Msg = m_import_feed:error_message(Reason, Context),
    z_render:growl_error(z_html:escape(Msg), Context).
