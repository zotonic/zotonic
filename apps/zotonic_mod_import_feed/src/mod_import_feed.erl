%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Discover, preview and continuously import RSS and Atom articles.
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
-define(DEFAULT_POLL_MINUTES, 60).
-define(DEFAULT_IMPORT_DEPTH, 1).
-define(PREVIEW_ENTRIES, 3).
-moduledoc(#{
    zotonic_keywords => [
        "reference", "integrator", "module", "import_and_migration", "import", "schedule", "url"
    ]
}).
-moduledoc("
Discover RSS 2.0, RSS 1.0/RDF and Atom feeds and automatically import articles and media.

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

Imports create published resources. For each inline image, the importer selects one
variant, preferring the largest declared `srcset` width or density, with `src` as
fallback. Imported variants share one local medium and inline references are rewritten
to its URL. WordPress featured-image variants reuse the original image. In attached
mode, other inline images retain their source URLs.

## Fetch authentication and development

Discovery can use the current user's registered OAuth2 consumer credentials.
Saved feeds use their selected import user's credentials for previews and polling.
Tokens are resolved per destination host and port, only over HTTPS, and are never
stored in feed settings. Missing credentials leave a request anonymous. Native
imports save their fetch preference for core reimports and medium downloads.

Development sites allow local destinations and self-signed TLS certificates.
Production keeps destination and certificate checks; browser input cannot change
the server's environment. OAuth2 credentials are never sent over HTTP.

## Polling and identity

The minute tick starts a bounded background job for due feeds. Database leases prevent
concurrent claims. The overview shows article counts, cumulative import/update/skip/error
statistics and poll times. Each feed has settings, pause/resume, test/preview, poll-now,
reimport and paginated history for articles and dependent media.

Stable source URIs and checksums allow repeated polls without duplicates. Reimport
processes only entries currently present in the feed, preserving resource IDs. Deleted
resources remain deleted. Media already imported from the same URL is reused. Feeds
with more than 100 entries are rejected, rather than silently importing a partial feed.
WebSub hub links are recognized in feed data; feed fetching uses polling.

## Zotonic resource feeds

Atom entries from `mod_export` advertise a JSON resource link with relation
`https://zotonic.com/rel/resource`. These use `m_rsc_import`, preserving native
properties, categories, media embeds and connections. Connection import depth
defaults to 1 and accepts 0–10. Content-group/copyright defaults and cleanup
patterns apply; category guessing and media-layout choices do not reshape native
resources. Choosing no media disables native medium downloads.

With `mod_websub` enabled, optional subscriptions cover imported articles and,
when selected, their connections within the import depth. Both options default
to false and require non-authoritative imports and a supporting source. Existing
subscriptions are managed separately through WebSub administration. Native
resources use the core Fetch new version flow and saved import options, without
requiring the entry to remain in the feed or repeating the feed notification.

## Fetch new version

For ordinary feed entries, the `rsc_import_fetch` observer refreshes known imported articles from their current
feed, using the saved settings and import account. The caller needs feed-management
and article-edit permission. Paused feeds can be refreshed manually without resuming
polling. This bypasses unchanged checksums and still runs the import notification.
An article no longer present in the feed returns `feed_entry_not_found`; unknown
URIs fall through to the standard resource importer.

## Import notification

Before writing an entry or its media, the importer calls `z_notifier:first/2` with
`#import_feed_item{item, dependents, feed, raw}`. Include the record from
`zotonic_mod_import_feed/include/mod_import_feed.hrl` and implement
`observe_import_feed_item/2` to customize importing.

- `item` contains `uri`, resource `props`, `edges` and an optional `medium` URL.
  Edges are `{PredicateId, ObjectIdOrUri}` pairs.
- `dependents` contains media import descriptions with `uri`, `props` and `medium`.
- `feed` contains settings and feed metadata.
- `raw` retains the parsed feed entry and, when available, WordPress response data.

Return `undefined` to use the default importer, `{ok, Id}` after importing the article,
or `{ok, Id, UriToIdMap}` to also register dependent resources. Return `{skip, Reason}`
to ignore the entry or `{error, Reason}` to record a failure for retry on a later poll.
Notification properties are still untrusted; custom observers must enforce appropriate
ACLs and sanitization. See `m_import_feed` for the administration API.
").
-author("Marc Worrell <marc@worrell.nl>").
-mod_title("Import feeds").
-mod_description("Discover RSS and Atom feeds and automatically import articles and media.").
-mod_depends([admin, cron]).
-mod_schema(6).
-mod_config([]).
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_wires/include/mod_wires.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").
-export([observe_admin_menu/3, observe_tick_1m/2, observe_rsc_import_fetch/2, manage_schema/2, event/2]).

%% @doc Refresh known feed articles through the feed's settings and import account.
-spec observe_rsc_import_fetch(#rsc_import_fetch{}, z:context()) ->
    undefined | {ok, integer()} | {error, term()}.
observe_rsc_import_fetch(#rsc_import_fetch{uri = Uri}, C) ->
    case z_context:get(import_feed_native, C) of
        undefined -> m_import_feed:fetch_resource(Uri, C);
        Native -> z_import_feed_native:fetch(Uri, Native, C)
    end.

-spec observe_admin_menu(#admin_menu{}, list(), z:context()) -> list().
observe_admin_menu(#admin_menu{}, Acc, C) ->
    [#menu_item{id = admin_import_feed, parent = admin_content,
        label = ?__("Import feeds", C), url = {admin_import_feed, []},
        visiblecheck = {acl, use, mod_import_feed}} | Acc].

-spec observe_tick_1m(tick_1m, z:context()) -> ok.
observe_tick_1m(tick_1m, C) ->
    _ = z_sidejob:start_site_unique(mod_import_feed, z_import_feed_poll, run, [], C), ok.

-spec manage_schema(install | {upgrade, integer()}, z:context()) -> ok.
manage_schema(_, C) -> m_import_feed:install(C).

-spec event(#submit{} | #postback{}, z:context()) -> z:context().
event(Event, Context) ->
    %% These actions run when the response arrives, including permission errors.
    C = case Event of
        #postback{message = {test, _}} -> z_render:wire([
            {remove_class, [{target, "feed-preview"}, {class, "is-loading"}]},
            {enable, [{target, "feed-test"}]}
        ], Context);
        _ -> Context
    end,
    case m_import_feed:allowed(C) of
        true -> handle(Event, C);
        false -> z_render:growl_error(?__("You are not allowed to manage feeds.", C), C)
    end.

handle(#postback{message = add_connection}, C) ->
    z_render:dialog(?__("Add default connection", C),
        "_admin_import_feed_connection_dialog.tpl", [], C);
handle(#submit{message = select_connection_predicate}, C) ->
    Predicate = m_rsc:rid(z_context:get_q(<<"predicate">>, C), C),
    case m_rsc:is_a(Predicate, predicate, C) andalso z_acl:rsc_visible(Predicate, C) of
        true -> z_render:dialog(?__("Select connected page", C), "_action_dialog_connect.tpl",
            [{intent, <<"select">>}, {id, Predicate}, {tabs_enabled, [<<"find">>]},
             {delegate, mod_import_feed}, {width, <<"large">>}], C);
        false -> failure(invalid_connections, C)
    end;
handle(#postback{message = {admin_connect_select, Args}}, C) ->
    case m_import_feed:validate_connection(proplists:get_value(id, Args),
            z_context:get_q(<<"select_id">>, C), C) of
        {ok, {Predicate, Object}} ->
            C1 = z_render:insert_bottom("feed-connections", #render{
                template = "_admin_import_feed_connection.tpl",
                vars = [{predicate, Predicate}, {object, Object}]}, C),
            z_render:wire({dialog_close, []}, C1);
        {error, Reason} -> failure(Reason, C)
    end;
handle(#postback{message = select_user}, C) ->
    case m_import_feed:can_select_user(C) andalso not z_acl:is_read_only(C) of
        true -> z_render:dialog(?__("Select import user", C), "_admin_import_feed_user_dialog.tpl", [], C);
        false -> failure(eacces, C)
    end;
handle(#submit{message = search_users}, C) ->
    case m_import_feed:can_select_user(C) of
        true -> z_render:update("feed-user-results", #render{
            template = "_admin_import_feed_user_results.tpl",
            vars = [{search_text, z_context:get_q(<<"search_text">>, C)}]}, C);
        false -> failure(eacces, C)
    end;
handle(#postback{message = {choose_user, Args}}, C) ->
    case m_import_feed:validate_user(proplists:get_value(id, Args), C) of
        {ok, User} ->
            C1 = z_render:update("feed-import-user", #render{
                template = "_admin_import_feed_user.tpl", vars = [{user_id, User}]}, C),
            z_render:wire({dialog_close, []}, C1);
        {error, Reason} -> failure(Reason, C)
    end;
handle(#submit{message = discover}, C) ->
    UseCredentials = z_convert:to_bool(z_context:get_q(<<"use_credentials">>, C)),
    FetchC = z_import_feed_http:context(#{use_credentials => UseCredentials}, C),
    case m_import_feed:discover(z_context:get_q(<<"url">>, C), FetchC) of
        {ok, Feeds} -> z_render:update("feed-discovery", #render{template = "_admin_import_feed_discovery.tpl", vars = [{feeds, Feeds}, {use_credentials, UseCredentials}]}, C);
        {error, Reason} -> failure(Reason, C)
    end;
handle(#postback{message = {select, Args}}, C) ->
    Url = proplists:get_value(url, Args),
    UseCredentials = z_convert:to_bool(proplists:get_value(use_credentials, Args)),
    case m_import_feed:preview(#{url => Url, use_credentials => UseCredentials}, ?PREVIEW_ENTRIES, C) of
        {ok, Preview} ->
            Feed = #{is_zotonic_feed => maps:get(is_zotonic_feed, Preview), use_credentials => UseCredentials, url => maps:get(url, Preview), title => maps:get(title, Preview),
                category_id => m_rsc:rid(article, C), poll_minutes => ?DEFAULT_POLL_MINUTES, import_depth => ?DEFAULT_IMPORT_DEPTH,
                media_mode => <<"dependent">>, use_wordpress => true, connections => []},
            z_render:update("feed-discovery", #render{template = "_admin_import_feed_edit.tpl", vars = [{feed, Feed}, {preview, Preview}]}, C);
        {error, Reason} -> failure(Reason, C)
    end;
handle(#submit{message = {save, Args}}, C) ->
    Input = (z_context:get_q_map_noz(C))#{<<"connections">> => z_context:get_q_all(<<"connection">>, C)},
    case m_import_feed:save(proplists:get_value(id, Args), Input, C) of
        {ok, Id} ->
            case proplists:get_value(id, Args) of
                undefined -> m_import_feed:record_feed_type(
                    #{id => Id, url => maps:get(<<"url">>, Input)},
                    #{is_zotonic_feed => proplists:get_value(is_zotonic_feed, Args)}, C);
                _ -> ok
            end,
            z_render:wire({redirect, [{dispatch, admin_import_feed_edit}, {id, Id}]}, C);
        {error, Reason} -> failure(Reason, C)
    end;
handle(#postback{message = {test, Args}}, C) ->
    case m_import_feed:get(proplists:get_value(id, Args), C) of
        {ok, Feed} ->
            case m_import_feed:preview(Feed, ?PREVIEW_ENTRIES, C) of
                {ok, Preview} ->
                    m_import_feed:record_feed_type(Feed, Preview, C),
                    C1 = z_render:update("feed-preview", #render{template = "_admin_import_feed_preview.tpl",
                        vars = [{preview, Preview}, {feed, Feed}]}, C),
                    case maps:get(is_zotonic_feed, Preview) of
                        undefined -> C1;
                        IsZotonic ->
                            Action = case IsZotonic of true -> remove_class; false -> add_class end,
                            WordpressAction = case IsZotonic of true -> disable; false -> enable end,
                            z_render:wire([
                                {Action, [{target, "feed-zotonic-options"}, {class, "hidden"}]},
                                {WordpressAction, [{target, "feed-use-wordpress"}]}
                            ], C1)
                    end;
                {error, Reason} -> failure(Reason, C)
            end;
        {error, Reason} -> failure(Reason, C)
    end;
handle(#postback{message = {pause, Args}}, C) ->
    result(m_import_feed:pause(proplists:get_value(id, Args), proplists:get_value(paused, Args), C), C);
handle(#postback{message = {poll, Args}}, C) ->
    result(m_import_feed:queue(proplists:get_value(id, Args), proplists:get_value(reimport, Args, false), C), C);
handle(#postback{message = {delete_content, Args}}, C) ->
    case m_import_feed:delete_content(proplists:get_value(id, Args), C) of
        {ok, Counts} ->
            z_render:dialog(?__("Imported content deleted", C),
                "_admin_import_feed_delete_result.tpl", [{counts, Counts}], C);
        {error, Reason} -> failure(Reason, C)
    end;
handle(#postback{message = {delete_feed_dialog, Args}}, C) ->
    case m_import_feed:get(proplists:get_value(id, Args), C) of
        {ok, Feed} -> z_render:dialog(?__("Delete feed?", C),
            "_admin_import_feed_delete_dialog.tpl", [{feed, Feed}], C);
        {error, Reason} -> failure(Reason, C)
    end;
handle(#submit{message = {delete_feed, Args}}, C) ->
    DeleteContent = z_context:get_q(<<"delete_content">>, C) =:= <<"1">>,
    case m_import_feed:delete_feed(proplists:get_value(id, Args), DeleteContent, C) of
        {ok, Counts} ->
            z_render:dialog(?__("Feed deleted", C), "_admin_import_feed_delete_result.tpl",
                [{counts, Counts}, {feed_deleted, true}, {delete_content, DeleteContent}], C);
        {error, Reason} -> failure(Reason, C)
    end;
handle(_, C) -> C.

result(ok, C) -> z_render:wire({reload, []}, C);
result({error, Reason}, C) -> failure(Reason, C).
failure(Reason, C) ->
    Msg = m_import_feed:error_message(Reason, C),
    z_render:growl_error(z_html:escape(Msg), C).
