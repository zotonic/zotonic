# mod_import_feed

Discover RSS 2.0, RSS 1.0/RDF and Atom feeds and continuously import their articles.
Enable **Import feeds** in Modules, then open **Content → Import feeds**. The module
requires `mod_admin` and `mod_cron`; it does not require `mod_websub`.

Administrator accounts cannot run imports. Select an enabled non-administrator account
with only the required import permissions. This is checked when saving settings and
before polling, including for existing feeds whose account is later promoted to admin.

## Editor workflow

1. Enter a website or feed URL. Discovery follows redirects and reads feed links
   from HTML (including relative links and `<base>`) and HTTP `Link` headers.
2. Select a feed. The next screen shows three example entries beside its settings.
3. Choose the default category, optional content group, category guessing,
   default connections, media mode, polling interval and authority mode, then save.
4. Use the overview to see imported article counts, cumulative import/update/skip/
   error counters, last poll, next poll and errors. Each feed has settings, pause/
   resume, a read-only test, poll-now and reimport controls, plus paginated history
   covering articles and dependent media, including deleted resources.

New feeds are active, poll hourly and import **non-authoritative** content by
default. They first become due immediately and are picked up by the minute tick.
The editor can save a feed paused. New feeds run as their creator. **Select user** lets administrators and users with
`use mod_admin_identity` choose another enabled account. Save the feed to apply the
selection. Ordinary settings edits preserve the selected account and its saved groups.

Settings are stored per feed, not in `m_config`:

| Setting | Behavior |
| --- | --- |
| Default category | An existing non-meta category. |
| Content group | Available when `mod_content_groups` is enabled; empty uses the site's default. |
| Guess category | Case-insensitive exact match of feed tags against existing category names or translated titles, in category-tree order. Meta categories and their descendants are never guessed. Falls back to the default, or article if the default has moved under meta; resource ACLs still apply. |
| Default connections | Use **Add connection** to choose a predicate and select a page in the standard resource dialog. The list shows each predicate and page, with a Remove button. Changes apply when the feed is saved; existing article connections are preserved. |
| Media | Import up to 20 media as dependent resources with `depiction` edges, attach the most important medium directly, or skip media. |
| Polling | Between 5 minutes and 7 days. Scheduling has one-minute resolution. |
| Copyright defaults | When `mod_copyright` is enabled: license, attribution and year. Nonempty values apply to articles and dependent media before the import notification. Empty values leave the standard copyright fallbacks in place. Stored defaults are preserved but not applied while the module is disabled. |
| Authority | Unchecked by default. Authoritative copies still retain their source identity in the importer's mapping and `feed_source_uri`. |
| Fetch using | Anonymous by default, or registered OAuth2 credentials for the selected import user and destination host. |
| WordPress enrichment | Enabled by default; REST failures fall back to feed data. |

Imported content is published. Resource insertion, editing, connection creation,
media upload and category/content-group access use normal Zotonic ACL checks.
Import HTML is sanitized by the resource APIs. Inline images that were imported
are rewritten to local media URLs; their remote `srcset` alternatives are removed.
For each image, only the largest declared `srcset` width or density is downloaded;
`src` is the fallback. All variants are aliases of that one medium, including
repeated images with overlapping candidate sets. WordPress featured-image size
variants reuse its original `source_url`. Media files pass through the regular media identification/sanitization pipeline.
Failed media downloads or insertions (including access denied) do not prevent
ordinary article imports. The importer removes references to the skipped medium
and omits its connections, while keeping the article and successfully imported media.
Use a forced refresh to retry skipped media.

The importance order is WordPress featured image, feed media/enclosures, then
inline images. Attaching one medium leaves other inline images at their source.

## Zotonic Atom feeds

Atom entries exported by `mod_export` advertise their structured resource with a
`link` whose `rel` is `https://zotonic.com/rel/resource` and type is
`application/json`. Its URL is the `rsc_export` model API. The importer checks
that the exported resource URI matches the Atom ID before using `m_rsc_import`.
An ordinary JSON alternate link does not activate native importing.

The Zotonic-specific settings are hidden for feeds detected as non-Zotonic.
Detection is remembered after previews and polling. Empty Atom feeds remain
unknown; mixed feeds keep these options available. Hidden settings retain their
saved values.

Native imports preserve source categories, multilingual properties, `z-media`
embeds and resource connections. Category guessing and the ordinary media-layout
choices do not reshape native resources; choosing no media disables downloads.
Content-group and copyright defaults, removal regexps and default connections
still apply. WordPress enrichment is skipped.

**Connection import depth** defaults to **1** (direct connections), accepts 0–10,
and uses the native importer's depth semantics. Zero skips edges; resource
references embedded in properties are still resolved by the native importer.
With `mod_websub` enabled, **Subscribe imported articles for updates** requests
resource subscriptions. **Also subscribe imported connections for updates**
extends those subscriptions within the selected depth. Both are off by default;
subscriptions require non-authoritative resources and a supporting source.
Unchecking these options stops requesting new subscriptions; manage existing
subscriptions in the WebSub administration screens.

Native resources use the core **Fetch new version** flow and its saved import
options, even after the article disappears from the feed. Such refreshes use the
current caller's permissions and do not run the feed-item notification. Polling
still runs under the feed's selected account and notifies before importing.

For native entries the notification item additionally includes `zotonic_export`
(the original decoded export) and `native_options`. Its proposed `props` include
cleanup and configured defaults; `edges` can contain predicate names as well as
local IDs. `dependents` lists direct edge targets with empty property maps; the
native importer resolves their full exports recursively. Imported non-meta
resources are recorded in the feed history alongside articles and media.

Feed and resource JSON requests use the bounded client and selected fetch mode described
below. Native medium downloads and update subscriptions use the core media and
WebSub routines and their respective limits.

## WordPress

WordPress post GUIDs such as `https://publisher.example/blog/?p=123` yield the
installation's `wp-json/wp/v2/posts/123?_embed=1` endpoint. If a GUID does not expose
a post ID, the importer checks the article's advertised JSON representation.
Enrichment supplies full rendered content, author name, UTC publication and
modification dates, featured media and additional inline images. It retains the
feed's identity rather than replacing it with the article slug or API endpoint.
REST requests use the selected fetch mode; tokens stay in Zotonic’s OAuth2 consumer registry.
Disable enrichment to avoid extra per-article requests on non-WordPress sources.

## Observer contract

Include the module's public record:

```erlang
-include_lib("zotonic_mod_import_feed/include/mod_import_feed.hrl").
-export([observe_import_feed_item/2]).

observe_import_feed_item(#import_feed_item{
    item = Item,
    dependents = Dependents,
    feed = Feed,
    raw = Raw
}, Context) ->
    %% Return undefined to use the default importer.
    undefined.
```

The `first` notification runs **before any resource or medium writes**, within the
entry's database transaction and with the saved editor's current permissions.
It is sent only for entries that need importing, not unchanged or deleted ones.

* `item`: `#{uri, props, edges, medium, media_aliases}`. The alias map relates
  image variant URLs to the selected largest/original URL. Properties use binary keys. `edges` is a
  list of `{PredicateId, ObjectId | SourceURI}` pairs; `medium` is a URL or
  `undefined`. Default connections and proposed depiction edges are included.
* `dependents`: a list of `#{uri, props, medium}` maps for proposed media resources.
* `feed`: saved configuration and identifiers, plus parsed feed metadata under
  `feed`. The worker's lease token and saved ACL group list are omitted.
* `raw`: the original parsed XML node. After successful WordPress enrichment it
  is `#{feed => XMLNode, wordpress => DecodedJSON}`.

The properties and raw data are **untrusted and not yet sanitized**. Observers
that import directly must use the normal resource/media APIs and respect ACLs.
The default importer sanitizes them through those APIs.

Return values:

| Return | Result |
| --- | --- |
| `undefined` | Import the item, dependent media and connections normally. |
| `{ok, ResourceId}` | Observer has imported the article; record its identity/history. |
| `{ok, ResourceId, ImportedMap}` | Also track dependent resources, where `ImportedMap` maps source URI binaries to local IDs. |
| `{skip, Reason}` | Record a deliberate skip; the observer can reconsider on the next poll. |
| `{error, Reason}` | Roll back this entry, record the error and retry on a later poll. |

Do not start asynchronous resource writes and return `{ok, Id}` prematurely.
Avoid external side effects inside the notification: the transaction can roll back.

## Identity, retries and scheduling

`import_feed` stores configuration, editor, due date, lease and statistics.
`import_feed_item` is the per-feed history, keyed by `(feed_id, uri)`.
`import_feed_resource` maps a source URI to a local resource across all feeds.
This allows overlapping feeds to reuse resources and authoritative copies to be
updated without relying on the local resource's `uri` property.

The source identity is an absolute RSS GUID/Atom ID when available, otherwise the
article URL. Opaque GUIDs become a SHA-256 URN scoped to the publisher origin;
missing both ID and URL is an error. Opaque identifiers must be unique within that
origin. Publishers with separate applications that reuse opaque IDs should emit
absolute GUIDs. A feed redirect does not replace an entry's absolute identity.

The resource map survives deletion with a null resource ID. These tombstones
prevent automatic resurrection, including on reimport. Changed entries and changed
settings are detected by a checksum of the proposed import. Reimport clears entry
checksums but retains identities. Reimport processes the **current contents of the
feed**, not an archive; entries that have fallen out of it need to be republished
by the source to be imported again. Media URLs are deduplicated; unchanged URLs
reuse existing downloads. An image replaced in place at the same URL is not
redownloaded automatically.

A per-site sidejob handles at most five due feeds per invocation. Database leases
prevent duplicate claims across nodes and recover after 15 minutes if a worker
crashes. Entries in each fetched feed are imported oldest first by publication
date, falling back to modification date. Undated entries come last; equal dates
and undated entries retain their order in the feed.
The lease is renewed before each entry. Pause and configuration changes
are checked between entries; an entry already being imported can finish.
Resource and identity writes share a transaction, serialized per site to prevent
races between overlapping feeds and shared media. A failed entry is rolled back
and does not stop the remaining entries. Feed failures wait for the configured
polling interval. Resource/media I/O occurs inside that transaction; slow publishers
can therefore delay other feed imports for the same site.

## Bounds and outbound requests

Fetches are anonymous by default. Choose **Registered OAuth2 credentials** during
discovery or in feed settings to use the selected import user's tokens from
`mod_oauth2`. Discovery uses the current account; saved-feed previews and polling
use the configured import user's credentials. The OAuth2 consumer must enable
use for imports. Missing matching credentials leave the request anonymous.
Tokens are looked up separately for each destination host (including its port),
only sent over HTTPS, and never copied between redirect destinations. The mode
applies to feed, WordPress, ordinary media and native resource JSON requests.
Native imports save their authentication preference in core fetch options;
native manual refreshes use the caller's registered credentials when enabled.
Feed settings never store tokens.

Only HTTP(S) URLs without embedded credentials are fetched. In production,
private/loopback/link-local destinations and HTTPS-to-HTTP redirects are rejected.
When `m_site:environment/1` is `development`, local destinations, self-signed TLS
certificates and HTTPS-to-HTTP redirects are allowed. This is derived server-side
from the site environment; a saved feed cannot enable it in production. OAuth2
tokens are still never sent over HTTP.
Every redirect is checked, with a maximum of five request hops. Requests have a
15-second timeout; feed/page/API responses are limited to 4 MiB and media to
20 MiB. Feeds exceeding 100 entries are rejected with an explicit error rather
than silently dropping entries. XML names remain binaries; DTDs and entity
declarations are rejected.

As with other Zotonic outbound clients, destination DNS checks happen before the
HTTP client connects. DNS rebinding protection ultimately requires address pinning
in the shared HTTP client; these checks alone do not provide that guarantee.

Feed fetching uses polling. It can read XML WebSub hub metadata, but does not subscribe to
RSS/Atom WebSub deliveries or attempt to crawl archive pagination.

## Tests

```sh
./rebar3 compile
bin/zotonic runtests import_feed_tests import_feed_db_tests
```

The database tests use Zotonic's disposable `testsandbox` schema. They cover schema
installation/reruns, access control, deduplication, authoritative copies, observer
short-circuiting, deletion tombstones, scheduling, both media modes, local inline
images, publication timezone preservation and runtime template rendering.

Imported inline images are stored as `z-media` embeds, so they display and remain
editable in Zotonic body editors. Lazy-loaded `data-src` and `data-srcset` images
use the same largest-variant selection as ordinary images. Captions are preserved.

Body links to image files are unwrapped. Other relative links are resolved against
the original article URL. Links matching an imported article’s source URI or
website use its local page URL when visible to the import user; fragments are
preserved. Other destinations, including email links, remain external.

### Removing repeated text

The feed settings accept one removal regexp per line (up to 50 lines / 16 KiB).
Patterns use Unicode, are case-sensitive, and run in order on decoded summary and
body text before the import notification. Body matches can span inline elements
such as links without matching HTML attributes. Surrounding markup is preserved.
Blank lines are ignored; invalid patterns prevent saving and report their line.
Matching has execution limits; exceeding them fails the item instead of importing
partially cleaned content. Use `(?i)` in a pattern for case-insensitive matching.

For the De Westkrant attribution footer, use:

```text
Het bericht .*? verscheen eerst op De Westkrant\.
```

Settings affect subsequent imports. Reimport current entries to apply changes to
articles already imported. Empty settings leave text unchanged.

### Deleting imported content

Feed management offers a confirmed **Delete imported content** action. It pauses
the feed and deletes its recorded articles and media using the current user's
permissions. Protected resources and resources the user cannot delete are kept.
Parents are also kept when removing them could trigger automatic deletion of an
unprotected dependent resource the user cannot delete. Shared imported resources
are included; the confirmation explicitly warns about this.

An active import must finish before deletion. Settings, history and identity
tombstones remain, preventing deleted content from being recreated by reimport.
The result dialog reports deleted, kept and failed counts.

**Delete feed** opens a confirmation with two choices: delete only the feed
(the default), or also delete imported content using the same protection and
permission checks. The feed and its per-feed history are removed; global source
identities remain for duplicate prevention and deletion tombstones. Any skipped
or failed content remains on the site, with counts shown in the result dialog.

### Fetching a new version

For ordinary feed articles, the standard **Fetch new version** action is handled
by `rsc_import_fetch`. It refreshes that article from the current feed using
its saved settings, WordPress enrichment and import account, including media,
cleanup and the `import_feed_item` notification. An unchanged checksum does not
prevent this explicit refresh. Paused feeds remain paused. With overlapping
feeds, the most recently recorded article history selects the configuration.

The requesting user must have feed-management and article-edit permission; the
saved import account must be enabled, non-administrator and allowed to edit the
article. Unknown URIs fall through to the usual resource importer. Missing feed
entries return `feed_entry_not_found`; this does not crawl historical articles.
The feed settings determine media and connections; generic resource-import dialog
options do not override them. Deleted resources are never resurrected.
