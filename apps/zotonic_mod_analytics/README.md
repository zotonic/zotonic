# Site Analytics

Lightweight analytics for site traffic, content, email, and custom events, using the site's PostgreSQL database. Enable `mod_analytics`
and open **Content → Site Analytics** (`admin_analytics`). There is no external
tracker, tracking cookie, visitor identifier or persisted event log.

The default Languages recording starts paused. Enable it with **Resume** in
the recording configuration when a language breakdown is useful. Existing
recordings retain their configured state.

The dashboard displays all non-paused recordings in their saved order, with one shared
period selector. Chart labels use the request context’s timezone, shown beside the
period selector; storage and bucket boundaries remain UTC. The first recording has a wide lead graph; remaining recordings
use a responsive graph grid. The site limit is 32 recordings.
Use **Configure recordings** (`admin_analytics_config`) for the separate
administrator-only page with creation, pause/resume and analytics status. Drag
recording handles to change their site-wide dashboard order; changes save
immediately. Reordering does not alter recording definitions or history.

## What is recorded

The built-in `page_request` event counts successful GET requests for public HTML
pages served by Zotonic, including template pages without a resource ID. It excludes private pages, admin/editor requests,
assets, redirects, DNT and GPC opt-outs. It includes automated traffic; the coarse
`traffic` dimension distinguishes known crawlers, other and unknown traffic. CDN
hits that never reach Zotonic are not counted. Counts are not unique visitors.

Initial recordings cover total requests, language, public content, device and
traffic classification. Optional dimensions are country (from an already enabled
local `mod_geoip` database) and referrer hostname. No IP, user-agent string, URL
path, query string or complete referrer is stored by this module. Other Zotonic
logging modules have their own policies.

Custom page controllers can mark a public resource with the returned context:

```erlang
Context1 = m_analytics:mark_page(ResourceId, Context).
```

The final response must still pass the request eligibility checks. To exclude a
request, set `analytics_exclude` to `true` in its context. Configuration
`mod_analytics.require_consent = true` requires `analytics_consent = true` in the
trusted request context. Sites must supply that flag from their own consent
integration; this module does not read browser local storage or install a beacon.

## Choosing recordings

Administrators select an event, zero or one breakdown, a group limit, and fixed
filters. Filter fields are ANDed; allowed values within one field are ORed. For
example, group requests by device while filtering language to `en`. This records
only device × time for that population, without storing device × language × time.
The form accepts two filters; the API accepts up to eight schema-declared fields.

Definitions are immutable. Create a new recording for a different population;
there is no historical backfill. Equivalent definitions cannot be duplicated.
Pause/resume preserves history and its storage reservation. Reports require
`use mod_analytics`; configuration requires a site administrator
(`use mod_admin_config`). The permissions are independent: analytics viewers
cannot read configuration/status/event schemas or create, pause, reorder or purge
recordings. Site administrators can manage configuration without analytics-view
permission. Configuration is also available under **System → Analytics configuration**. The **Delete** action on the configuration page first opens a confirmation
naming the recording and explaining that its history will be permanently removed.
Cancel leaves it unchanged. Confirming deletes the recording and releases its
reservation through the administrator-only `purge/2` API.

```erlang
{ok, Id} = m_analytics:create(#{
    <<"name">> => <<"English requests by device">>,
    <<"event">> => <<"page_request">>,
    <<"dimension">> => <<"device">>,
    <<"groups">> => 5,
    <<"filters">> => #{<<"language">> => [<<"en">>]}
}, <<"active">>, Context).

ok = m_analytics:set_state(Id, <<"paused">>, Context).
ok = m_analytics:set_state(Id, <<"active">>, Context).
```

Use `<<"draft">>` instead of `<<"active">>` to create without collecting. Empty
`dimension` means total only and reserves one group. Draft and paused recordings
also reserve capacity. History ages out on subsequent flushes; its definition
retains the reservation until purged.

## Module-defined events

The notification contract is documented in `src/behaviours/zotonic_analytics_observer.erl`,
including callback types and metadata for the Zotonic documentation importer.
An enabled module exports an `observe_analytics_events/3` fold observer:

```erlang
-include_lib("zotonic_mod_analytics/include/analytics.hrl").

observe_analytics_events(#analytics_events{}, Events, _Context) ->
    [#{name => <<"mod_newsletter:sent">>,
       version => 1,
       title => <<"Newsletter sent">>,
       dimensions => #{<<"kind">> => [<<"digest">>, <<"single">>],
                       <<"language">> => text}}
     | Events].
```

Names must be namespaced. Dimensions accept enums or UTF-8 text up to 255 bytes.
Each event has at most eight declared dimensions; undeclared properties are
rejected. Use coarse categories, never personal data, free-form user input,
addresses or identifiers. A few sensitive field names are rejected, but that is
not a substitute for reviewing a module's schema. Increment `version` when the
meaning or schema changes; old recordings do not silently collect a new version.

Record a background event after the domain operation succeeds:

```erlang
m_analytics:record_event(<<"mod_newsletter:sent">>,
    #{<<"kind">> => <<"digest">>, <<"language">> => <<"en">>}, Context).
```

Explicitly associate an event with the current request:

```erlang
m_analytics:record_event(<<"mod_newsletter:sent">>,
    #{<<"kind">> => <<"single">>}, z_context:get_reqdata(Context), Context).
```

The request must belong to that Context. Request exclusions apply, and declared
request dimensions (such as language) are derived on the server. Callers cannot
override these with properties. The three-argument API never implicitly uses a
request, even if the Context contains one. Background workflows must carry their
own permission decision; the collector cannot infer consent from a job.

These are trusted Erlang APIs, not public model POST routes. `ok` means best-effort
admission, including disabled/excluded collection, not durable acknowledgement.
Validation failures return `{error, Reason}`. Repeating a business operation's
record call counts again; database-flush deduplication does not deduplicate jobs.

## Bounded storage and aggregation

| Resolution | Slots per recording | Retention |
| --- | ---: | --- |
| 1 minute | 360 | 6 hours |
| 15 minutes | 672 | 7 days |
| 1 hour | 720 | 30 days |
| 1 day | 1,826 | About 5 years |

Each recording reserves `3,578 × groups` group/time slots. The default site budget
is 1,000,000 (`mod_analytics.row_budget`); there are at most 32 retained recordings
and 100 groups per recording. A non-page grouped recording reserves one group for Other;
at a limit of one, all grouped events go to Other. Minute admission retains the
first admitted groups for that minute. Rollups retain the largest surviving
groups, with stable tie-breaking, and fold the rest into Other. Counts are
conserved, but labels already folded into Other cannot be recovered.

A time bucket is one SQL row containing a bounded counter map. Its primary key is
collector, resolution and modulo slot. An absolute timestamp prevents stale data
from overwriting a reused slot. Therefore the budget describes group/time cells,
not physical SQL rows or an exact number of bytes. PostgreSQL indexes, MVCC dead
tuples and WAL need normal autovacuum and disk management.

Ingress uses a 4,096-entry ring with schema-validated dimensions only. The worker
drains at most 200 entries or approximately five milliseconds per turn, immediately
schedules another turn when the work or time budget is reached, and waits 100 ms only after finding the ring empty. It aggregates
in memory and sends an immutable batch to a separate writer every ten seconds. There are at most 10,000 pending
counter entries, plus one frozen batch; group admission is bounded by the recent
minute window. No database call is made on the normal ingestion path.

Each flush atomically commits its receipt, minute counters and recomputed parent
buckets. Its SQL queries share a ten-second budget, with 250 ms lock waits. Retrying that batch cannot increment counts twice. Receipts expire after
24 hours and have a hard 90,000-row limit. Retries stop after two minutes. Ring
loss, discarded work, startup and uncovered intervals produce incomplete data;
reports show gaps instead of inventing zero traffic. Nonzero partial buckets are
shown as lower bounds with an incompleteness notice. Reports choose a single tier
and return at most 240 chart samples, using Zotonic's `{% chart %}` scomp.

## Operational limits

Run one collecting Zotonic node per site. Distributed ownership/fencing is not
implemented. This is an audience counter, not a billing ledger or durable bus.
Shutdown or a worker crash can lose buffered and uncommitted work; restarts retain
committed history. Database outages and overload can lose counts deliberately to
keep memory bounded. Status reports dropped updates, writer failures and the last
successful flush. The global access-log ring can also lose events upstream.

Browser page-view collection, visitor/session estimates, the optional one-second
live tier and CSV export are later extensions described in the design document.
The current module uses only aggregate request and declared module-event counts.

## Source and validation

- [Research and proposal](../../doc/technotes/simple-analytics.md)
- [Implementation design](../../doc/technotes/simple-analytics-design.md)
- `z_analytics`: pure validation, registry, filters and count reduction.
- `z_analytics_request`: request eligibility and coarse dimensions.
- `z_analytics_worker` / `z_analytics_writer`: bounded ingestion and persistence.
- `m_analytics`: schema, reservations, transactional rollups and queries.
- `m_analytics`: trusted event APIs and ACL-checked reports/configuration.

Compile with `./rebar3 compile` from the umbrella root. Pure tests are in
`test/z_analytics_tests.erl`; shared graph tests include missing-sample behavior.
`analytics_integration_tests:run(Context)` explicitly requires a development site
and checks retry deduplication, bounded rollups, count conservation, slot reuse and
ACLs. `analytics_integration_tests:ordering(Context)` checks persisted ordering,
overview ordering, stale/duplicate orders and configuration authorization, restoring
the original order afterwards. The main integration test creates and cleans up only its own collector and receipts.

`analytics_integration_tests:access_control(Context)` checks controller and model
access for viewer-only, configuration-only, denied and administrator contexts.
It uses a scoped test observer and does not change stored ACL rules.

### HTTP API requests

Select **HTTP API requests** when creating a recording to count completed HTTP
requests handled by `controller_api` (`/api/...`). This is the `api_request` event,
separate from public page requests. All methods and response classes are counted,
including HEAD, OPTIONS, redirects and errors that reach the controller middleware.
Requests rejected before that middleware and MQTT/WebSocket model calls are not
included. The HTTP status describes the response, not necessarily the outcome of
an asynchronously published model call.

Choose one breakdown: `method`, `status` (1xx–5xx), `language`, `country` or
`traffic`. For example, record a total filtered by `status = 4xx, 5xx`, or break
down by method. The same bounded storage and fixed filters apply. Create the
recordings you need; existing installations do not get an extra recording automatically.

The existing request exclusions apply: admin-interface users, `analytics_exclude`,
DNT/GPC and required consent. API requests do not need to refer to a public page.
No topic/path, query string, body, token or user identifier is retained.

### Dispatch classification

Use `{analytics, Type}` in dispatch options to classify a request:

```erlang
{catalog, ["catalog", id], controller_page,
    [{template, <<"catalog.tpl">>}, {analytics, page}]},
{api, ["api", '*'], controller_api, [{analytics, api}]},
{download, ["download", id], controller_file, [{analytics, download}]}
```

Standard type names are `page`, `api`, `download` and `asset`. Custom names such
as `webhook` are also supported: atoms or binaries matching `[a-z][a-z0-9_]*`,
up to 64 bytes. These are fixed route labels, never values derived from request
paths, resource names or user input. `{analytics, false}` disables a route.
Without an option, `controller_page` and `controller_template` default to `page` and `controller_api`
defaults to `api`; other controllers are not tracked. Assets and downloads are
therefore opt-in. Explicit options always override those defaults.

Select **Requests by dispatch type** (`request`) and break down by `type`, or
use `type` as a fixed filter with another breakdown such as `status` or `method`.
Configuration discovers type names from the site's effective dispatch rules on
each page load, including controller defaults, and lists them as an inline hint below the filters.
Custom labels do not require an event-schema registration or collector restart.
Historical labels remain valid filters even after a dispatch rule is removed.

The `page` type retains the public HTML GET/200 policy and requires anonymous access; a resource ID, when present, must be publicly visible. Other types count completed responses of any method/status that
reach the middleware. All types retain the existing privacy/consent exclusions.
`page_request` and `api_request` remain available for existing recordings; the
new `request` event provides the cross-type view. Each event updates only its
own recordings, so an individual recording does not count a request twice.
MQTT traffic and requests rejected before controller middleware remain excluded.

For **HTTP API requests** and **Requests by dispatch type**, `http_status` is
also available as a breakdown or fixed filter. It contains the exact final HTTP
response code (100–599), for example `200`, `404` or `500`. The existing `status`
dimension continues to contain classes such as `2xx` and `5xx`. For example,
filter `http_status = 401, 403` and break down by `type` to compare access-denied
responses across route types. New recordings start with new requests; existing
aggregate history cannot be reclassified by exact status.


Template pages and resource presence
------------------------------------

`controller_template` defaults to the `page` analytics type, like
`controller_page`. Successful HTML GET responses without a resource ID are
eligible when the route allows anonymous access. An invalid configured resource
is not treated as an ID-less page; resources with IDs must be publicly visible.
The existing privacy exclusions and `{analytics, false}` override still apply.

The `has_page_id` dimension is available for page, API and dispatch-type request
recordings. Use the fixed filter `has_page_id = true` for requests with a resolved
resource ID, or `false` for requests without one; omit the filter to include both.
It is also available as a breakdown. For `controller_template`, only its configured
resource ID (or explicit `mark_page/2` ID) counts, matching what the controller
renders. A query-string ID does not attach a resource to an unrelated template.
Requests without an ID omit the `page` dimension and appear as unknown when
breaking down by page. Existing aggregate history is not retroactively classified.


## Performance verification

See `doc/technotes/simple-analytics-performance-audit.md` for the performance audit,
measured aggregation benchmark, overload tests and remaining operational limits.
The request path skips metadata enrichment when no active recording consumes its
event stream. Event-local ETS schemas avoid copying the complete registry during
ingestion. Admission expiry runs once per minute, and events visit only matching
active recording definitions. Database flushes avoid unchanged bucket rewrites.


## Popular content

Page breakdowns use a bounded, mergeable Misra–Gries summary instead of retaining
the first pages seen. Newly popular pages can enter the ranking even after the
counter capacity is reached. The dashboard shows up to 20 leading candidates
for the selected moving period (one hour, six hours, 24 hours, week, month, year
or five years). Counts are ranges when approximation is necessary; overlapping
ranges do not establish an exact rank. Totals remain exact for recorded events.

The default 100 counters retain every page exceeding roughly 1% of recorded
traffic, but cannot guarantee an exact top 20 among thousands of near-equal pages.
Old traffic stops contributing when its bucket leaves the selected window.
Windows advance at the selected tier resolution, include the current partial
bucket, and can be up to one bucket shorter than their nominal duration. Refresh
the dashboard to see newly flushed data. A horizontal bar chart compares the leading pages, with the ranked list and
count ranges beneath it. Bars show the lower bounds. No total-traffic time graph
is generated for popular content.

Existing history is preserved. Old anonymous Other counts widen the uncertainty
bounds until that history expires. Storage remains capped at the configured
number of counters per retained bucket plus constant summary metadata. No
visitor identity or raw request log is added.

See the [research and architecture decision](../../doc/technotes/simple-analytics.md#popular-content-bounded-rankings-over-moving-periods-implemented-november-2026)
for alternatives, references, error bounds and implementation details.

## SMTP (Email)

A standard **SMTP (Email)** recording counts outgoing email transport notifications
and breaks them down by `outcome`: `sent`, `retry`, `failed`, or `bounced`. Its
line graph shows notification activity over time; the breakdown shows outcomes
within the selected period. It is appended to the standard recordings and shown with all other recordings.
Drag it higher in configuration to change its dashboard position.

`email_sent` with `is_final=false` counts as sent (accepted by the transport).
The later `is_final=true` confirmation is ignored to avoid counting a normal send
twice. Non-final failures count as retries, final failures as failed, and bounce
notifications or failures with reason `bounce` as bounced. A sent message can
later bounce, and retries can repeat: these are notifications, not unique emails,
inbox deliveries or opens. Repeated provider notifications are not deduplicated;
providers that only report final success do not contribute to the sent count.
Inbound email is not counted.

Only the outcome is retained. No recipient/sender addresses, message identifiers,
SMTP response text, subjects or bodies are recorded. Email activity is operational
and independent of browser consent/editor exclusions. It uses the same bounded
queue, rollups, reporting ACL and configuration permissions as other recordings.

Fresh installations include this recording. Schema upgrade 3 appends it to
existing installations if the budget permits; an equivalent existing recording
is preserved. Its five groups reserve 17,890 group/time slots. If the budget is
full, free capacity and create an SMTP recording from configuration. Deleting the
recording after upgrade does not cause it to reappear on restart.

## Adding a recording

Use **Add recording** on the configuration page. The dialog first asks for a
name, an optional short description and an event type. The second step offers
only that event's breakdown and filter dimensions. Enumerated filters show their
allowed values. Returning to the first step preserves the settings; changing the
event clears the old breakdown and filters. Total recordings do not need a group
limit. All submitted dimensions and values are validated again on the server.

Descriptions are plain text, displayed with the recording on the dashboard and
configuration page. The form allows 250 characters; the API accepts at most
1,000 UTF-8 bytes. Descriptions are optional for existing recordings and stored
in the definition payload, so no table migration is needed. Names and descriptions
do not affect whether two recordings have an equivalent measurement definition.

## Recording information

Each recording on the configuration page has an **Info** button. Its read-only,
site-administrator-only dialog shows the description, event/schema, state,
breakdown, filters, group limit, creation/start timestamps and storage usage.
Per-resolution details include bucket counts, available slots, aggregate payload
bytes and oldest/latest bucket start times. Dates use the context's timezone.

Stored payload is measured with PostgreSQL `pg_column_size` for the counts and
recording definition; it excludes row headers, indexes, shared allocation and
unsaved in-memory events. Reserved group/time slots are shown separately as the
budget commitment, not physical disk usage. Statistics are queried only when the
dialog opens, for that recording, with no aggregate payload decoding or scans of
other recordings. Missing/deleted recordings show an unavailable message.
