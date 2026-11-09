Site Analytics
=================

Goal: make it possible to skip usage of trackers and have the most essential information inside the site itself.

What do site owners want to know:

- Visitors (new / returning)
- Bots / real people
- Phone / Tablet / Desktop
- Per country
- Per language
- Which content is (currently) popular
- Pages per session
- Referrers?

Idea:

Make some combinations, start collecting them in a statistics overview.
If site-admin wants to know a new combination, then we can configure new collectors.
Make overview page of all collectors.

Collectors: 
- graph for last minute / week / month / year / years
- this could be a generic thing

Similar to the reporters of iStat 3 by Bjango.


----------------------------------------
Add design below this line
----------------------------------------

## Proposed design

Research date: 6 November 2026. This is a proposal; names and interfaces below are
new unless explicitly identified as existing Zotonic functionality.

The companion [implementation and architecture design](simple-analytics-design.md)
works out the processes, APIs, schema, rollup algorithms and delivery stages.

Implement `mod_analytics` as a per-site module, with local PostgreSQL
storage and an admin overview. Start with aggregate counts of public page
requests. Offer browser page views and short-lived visitor estimates as explicit
extensions. No external analytics service, separate database server, or mandatory
JavaScript is needed for the baseline.

The useful abstraction is a **collector**: a named counter, a selection of events,
zero or one grouping dimension, fixed filters, and time buckets. Reporters display these
collectors at different resolutions, much like the iStat idea above. Keep the
collector engine inside this module until another module needs it.

### What the numbers mean

| Question | Proposed measurement | Limitation |
| --- | --- | --- |
| How much traffic? | Eligible public page requests; optionally browser page views as a separate series | Requests are not proof that somebody saw a page |
| How many visitors? | Optional estimated distinct visitors within a UTC day | A browser/network estimate, not a count of people |
| New or returning? | Unavailable in the default mode | Requires an identifier that survives between visits/days |
| Bots or people? | Known crawler, other, unknown | “Other” must not be labelled “human”; bots can impersonate browsers |
| Phone/tablet/desktop? | Coarse user-agent classification, including unknown | User-agent reduction and spoofing limit accuracy |
| Country? | Optional local IP-to-country lookup | Network location, not nationality or a person's location |
| Language? | Language of the served content | Not browser preference or the visitor's native language |
| Popular content? | Requests/views by resource ID, optionally language | Keep request and browser rankings separate |
| Pages per session? | Optional completed-session page count divided by completed sessions | Depends on the identity and session rules below |
| Referrers? | External hostname; internal and direct/unknown separately | Missing referrer information cannot be reconstructed |

Do not include bounce rate, time on page, funnels, advertising attribution or
individual visitor histories in the first version. A server request cannot
measure reading time, and the final page in a session has no following request
from which to infer duration.

### Use the existing request pipeline

The current code already provides most of the server-side input:

- [`zotonic_listen_http_metrics`](../../apps/zotonic_listen_http/src/zotonic_listen_http_metrics.erl)
  puts completed request metrics into bounded ring buffers. Entries include
  status, method, path, query string, user agent, referrer and request metadata.
- [`z_stats`](../../apps/zotonic_core/src/support/z_stats.erl) drains these buffers
  and publishes `$SYS/site/<site>/log/access`. Subscribe only to the current
  site's topic, following [`mod_syslog`](../../apps/zotonic_mod_syslog/src/mod_syslog.erl).
- [`z_sites_dispatcher`](../../apps/zotonic_core/src/support/z_sites_dispatcher.erl)
  supplies the dispatch rule, controller and resolved peer IP.
  [`controller_page`](../../apps/zotonic_mod_base/src/controllers/controller_page.erl)
  adds `rsc_id`; [`z_cowmachine_middleware`](../../apps/zotonic_core/src/support/z_cowmachine_middleware.erl)
  adds language, timezone, user ID and session ID after handling.
- [`z_user_agent:is_crawler/1`](../../apps/zotonic_core/src/support/z_user_agent.erl)
  is an existing starting point for crawler classification. Its matching and
  coverage need tests before treating it as the analytics classifier.
- [`mod_admin_statistics`](../../apps/zotonic_mod_admin_statistics/src/mod_admin_statistics.erl)
  already covers operational statistics. Keep audience analytics in a separate
  admin screen; reuse layout conventions, not its operational metric semantics.

Subscribe on module startup and unsubscribe on shutdown. Immediately select and
normalize permitted fields; never persist the incoming access-log map. In
particular, discard query strings, user IDs and Zotonic session IDs. Analytics
must not silently repurpose authentication or Cotonic sessions.

For the baseline, count completed `GET` requests with successful `200` responses
on explicitly eligible public page dispatches. Exclude redirects, errors,
`HEAD`, assets, media, APIs, admin, previews, private content and the collection
endpoint. Keep `304` revalidations separate from successful document responses.
Custom page controllers opt in through a small module API that adds an eligibility
flag and approved page key using `z_context:set_req_metrics/2`.

Add eligibility, negotiated HTML content type and an exclusion flag while the
request context is available, using the existing handled middleware notification.
The current log drops response headers, so an `Accept` header or a resource ID
alone is insufficient to identify a public HTML response. Default to exclusion
when eligibility is uncertain. Exclude admin/editor activity by checking ACLs in
that context and emitting only a boolean; the later consumer must not perform a
resource or user database lookup for every event.

Origin metrics miss CDN/cache hits that never reach Zotonic, and requests include
prefetches and automation. State this beside the request chart. Application
caching that still serves requests through Zotonic does not have this limitation.

### Optional browser measurement

A small plain JavaScript file, included through the standard `_html_body.tpl`
all-include, sends a same-origin POST to a dedicated dispatch/controller. Use HTTP
rather than requiring an MQTT connection solely for analytics. The endpoint must
not create a Zotonic session and must return a small non-cacheable response.

Send one event when an eligible document first becomes visible. Treat a visible
back/forward-cache restoration as another view; switching tabs alone is not
another view. SPA sites explicitly call the same function after a route change.
Generate an in-memory random event ID for each view and reuse it for any bounded
retry. Never embed per-visitor IDs in cacheable HTML. Browser lifecycle handling
must account for background loading and prerendering: `pageshow` alone is not a
visibility signal. See [MDN's pageshow documentation](https://developer.mozilla.org/en-US/docs/Web/API/Window/pageshow_event).

Send only an approved page key, content language, event ID and sanitized referrer
hostname. Supply page metadata from the template; validate it on receipt against
public resources or a configured dispatch/page-key allowlist. For cached pages,
recheck current public eligibility. Do not accept client-supplied country, IP,
visitor identity or arbitrary event properties. Do not send query strings,
fragments, titles, form contents or full referrer URLs.

Validate same-origin requests where Origin/Fetch Metadata are available, limit
body size and field lengths, rate-limit ingestion and cap event-ID deduplication
state. Origin checks do not authenticate a human and cannot prevent determined
metric poisoning. Browser events remain estimates: blockers and disabled
JavaScript cause undercounting, while capable bots can send events.

Store browser views and origin requests as different event types. Never add them
together or infer bot counts by subtracting one from the other. Enabling browser
measurement starts a new series, rather than changing the meaning of history.

### Privacy and visitor/session options

The default is **aggregate-only**: no identifier, cookie, local storage or visitor
history. Derive optional country and coarse device/bot classifications in memory
and discard IP and raw user agent before aggregation. Use the resolved peer IP
only with a correctly configured trusted proxy chain; never trust an arbitrary
forwarded header from the client. Country lookup uses an optional downloaded
country database, with an update procedure and license review; no per-request
external lookup. Missing or stale data is visible as unknown. IP geolocation is
inherently approximate; see [MaxMind's accuracy explanation](https://support.maxmind.com/knowledge-base/articles/maxmind-geolocation-accuracy).

For sites that need visitors and sessions, offer a separate **daily-estimate**
mode. Derive an in-memory key using HMAC-SHA-256 with an independent random daily
secret over an unambiguously encoded tuple of site, resolved IP and user agent.
Keep the key only in bounded, site-local memory; do not store it in aggregate
rows or logs. Destroy the secret and identity state at UTC midnight; never derive
past secrets from a permanent master key. A restart also resets this state and
must mark affected visitor/session statistics as approximate/incomplete.

Plausible uses a daily rotating site/IP/user-agent hash for cookie-free visitor
estimates; this is a useful precedent, not a guarantee of unique people or a legal
exemption. Shared networks can merge visitors and network/browser changes can
split them. See [Plausible's data policy](https://plausible.io/data-policy).

In this mode:

- A visitor is a key seen within one UTC day. Maintain separate daily distinct
  sets for each supported collector group, with explicit memory limits. Persist
  their counts, not the keys. Daily counts are not additive across dimensions;
  weekly/monthly sums must be labelled **visitor-days**, not unique visitors.
- A session ends after 30 minutes without an eligible event, or at UTC midnight.
  Use one selected source (requests or browser views) for a session collector.
  Keep only last-seen time, page count and entry dimensions in memory, not a path
  history. Attribute completed sessions and their page totals to the completion
  bucket and entry dimensions. The ratio uses these matched totals; do not divide
  all page views in a period by sessions that merely started in that period.
- Expire inactive sessions independently of new traffic. Mark unfinished sessions
  lost on restart as incomplete; do not invent completion records. Memory
  overflow disables affected identity measurements with a visible quality flag,
  rather than repeatedly evicting identities and counting them as new visitors.

Different products define “visit” differently: GoatCounter, for example,
deduplicates paths within its session scheme. Document our own definitions rather
than assuming comparable totals. See [GoatCounter's session documentation](https://www.goatcounter.com/help/sessions).

Cross-day **new/returning** counts are deferred. If added later, require an
explicit persistent-identifier mode with a separate first-party random cookie,
fixed retention, consent/withdrawal handling and no account association. Label it
as new/returning *identified browsers*, within the retention window; cookie
clearing, refusal and multiple devices prevent a person-level measurement.

Provide collection exclusions and an opt-out. Where consent is required, integrate
with `mod_cookie_consent`'s existing `stats` category, including withdrawal; apply
the same policy to server collection, not just script loading. An explicit
request-context exclusion flag is needed because consent headers/cookies are not
in the current metrics payload. Cookie-free does not automatically mean
consent-exempt: [CNIL's audience measurement guidance](https://www.cnil.fr/en/sheet-ndeg16-use-analytics-your-websites-and-applications)
explains that exemptions have conditions and national differences. Ship clear
data-handling documentation without claiming universal compliance. Existing
access logs and other subscribers have their own retention; this module does not
sanitize or erase those logs.

### Collectors and dimensions

Represent each collector as validated data, not executable SQL or Erlang:

```erlang
#{name => <<"popular_dutch_content">>,
  event => page_request,
  filters => #{traffic_class => [other, unknown], language => [<<"nl">>]},
  group_by => [page_key],
  measures => [count]}.
```

Use a validated registry of built-in and module-defined event types, dimensions,
filters and measures. Parse
external configuration into known values without creating atoms from user input.
Allow zero or one grouping dimension per collector, in addition to time. Use
`group_by => []` for a total. Enforce this in the model as well as the admin form;
defer multidimensional collectors until a concrete need justifies them. Cap enabled collectors and
distinct groups per bucket. Reject configurations beyond the site's budget.

Fixed filters select the population before aggregation and do not add grouping
dimensions. For example, filter to one page and group by country, or filter to
Dutch requests and group by content. Support bounded lists of typed values with
OR within each list and AND between fields. Apply filters after normalization
and privacy/eligibility checks, but before assigning an event to a retained group
or `other`. Missing values match only an explicit unknown selection. Keep filters
fixed for a collector version; changing them starts a new recording version.

Do not automatically create a collector for every filter value. Separate
collectors for every page or country would recreate combinatorial growth, so
filtered collectors and their retained versions share the same site-wide budget.
Even a single dimension can have many values: content and referrer collectors
still need group caps, while small finite dimensions can usually retain every
value, including unknown. Independent language and country recordings cannot
reconstruct language-by-country history; a country-filtered language recording
can answer that focused question only from its activation onwards.

Default collectors: total eligible requests by traffic class; non-crawler
requests by content; language; device class; country when configured; external
referrer hostname. Offer fixed-filter variants for focused questions.
“Non-crawler” here includes unknown traffic; always offer its breakdown.

Use resource IDs as content keys so translated URLs and later renames remain one
piece of content. Pages without a resource use an explicitly registered stable
key, not an arbitrary path. Resolve titles only when displaying the report, with
normal resource ACLs; deleted resources retain a neutral historical label. Never
cascade-delete aggregate history when a resource disappears.

Normalize referrer hostnames, discard user information, ports, paths, queries and
fragments, and classify configured site aliases as internal. Missing or stripped
referrers are direct/unknown, not proof of a direct visit. Unknown device,
country, language and page values have explicit buckets. Campaign parameters and
search terms are out of scope by default.

For unbounded dimensions such as referrer hosts, admit a bounded number of groups
per bucket and send excess values to `other`. Describe this as a cardinality cap,
not an exact top-N algorithm; retain admitted groups for the bucket's lifetime.
Separate caps for collectors and retained storage also bound the number of
filtered recordings.

A new combination collects from its activation time onwards. It cannot be
reconstructed from independent totals or discarded events. Changing filters,
dimensions, identity mode or measurement semantics creates a new collector
version with a visible start time. Renaming a label does not. The dashboard can
only filter on dimensions actually stored in that collector.

### Module-defined events

Allow enabled modules to declare their own events through a proposed
`#analytics_events{}` fold notification. Each declaration supplies a
module-qualified binary name, schema version, description and allowed dimensions
with types and value/cardinality limits. For example, `mod_newsletter:sent` could
declare a bounded `delivery_kind` dimension. Reject conflicting declarations and
unknown fields; never infer a schema from incoming events. Initially support the
additive `count` measure, with each accepted event contributing one.

Expose a trusted Erlang API through `m_analytics`:

```erlang
% Background job: Context selects the site; no request enrichment.
m_analytics:record_event(
    <<"mod_newsletter:sent">>,
    #{<<"delivery_kind">> => <<"digest">>},
    Context).

% Request handler: Req is the current Cowboy request map.
m_analytics:record_event(
    <<"mod_catalog:download">>,
    #{<<"format">> => <<"pdf">>},
    Req,
    Context).
```

These are proposed interfaces. The three-argument form is equivalent to the
four-argument form with `Req = undefined`, even if Context happens to contain a
request. A caller can explicitly pass `z_context:get_reqdata(Context)` to opt in
to request enrichment. Context always selects the site; a supplied request must
belong to that same site and context. No live HTTP request is needed for scheduled
jobs, imports or other asynchronous module work.

With a request, apply the same opt-out, consent, admin exclusion and sanitization
policy as other request-associated collection. Extract only approved dimensions
such as language, device, country and traffic class, then immediately discard
the request and sensitive fields. Module properties cannot override reserved
dimensions or exclusion decisions. Custom events do not require a successful
public HTML response: the caller records the domain action at the appropriate
point, after success/commit where applicable. Resource-related events still need
an explicit eligibility check to avoid exposing private content.

Without a request, record only the declared properties and server timestamp;
request-derived dimensions are unavailable. Do not invent a visitor, infer one
from Context's user/session, or accept IP addresses and identities as substitute
properties. Use this form for aggregate domain events, not to bypass collection
restrictions for user activity moved into a background job. Such jobs must carry
the already evaluated collection permission and approved dimensions, never a
saved raw request. Dimensions intended for that use must be explicitly declared.

Custom events feed the same bounded queue, collectors, versioning and retention
as built-in events. They remain separate from page requests/views and do not
increase page counts or extend browsing sessions. A collector can select a
registered custom event, apply fixed filters, and select at most one of its
declared or available request dimensions for grouping. The admin editor lists
these definitions and explains which
dimensions require a request. Schema changes create new event/collector versions;
disabling the declaring module stops acceptance but preserves historical reports.

Return `ok` for accepted events and when analytics are disabled, so modules can
use this optional facility without making their operation depend on it. Return
`{error, Reason}` for invalid declarations/payloads, with bounded diagnostics;
callers must not fail their primary operation or retry indefinitely. The existing
Zotonic ringbuffer overwrites old entries when full and reports skipped entries
to its reader, so it cannot provide a reliable `queue_full` return to producers.
Use that best-effort behaviour and surface loss through collection quality flags.
Acceptance is not a durability guarantee. Repeated calls count separately: batch
retry deduplication does not deduplicate a module job retried by its caller.
Do not expose this API as an unrestricted browser/model POST endpoint; any future
client event API needs a separate explicit allowlist and ingestion controls.

### Aggregation, storage and failure behaviour

Use one supervised worker per active site, owned by the module lifecycle. Keep
module hooks in `mod_analytics`, query/configuration and ACL boundaries
in `m_analytics`, and normalization/aggregation in support modules.
Implement schema creation/upgrades through `manage_schema/2`.

The ingest callback performs bounded normalization and inserts into a bounded
queue; a plain unbounded `gen_server:cast` mailbox is insufficient. The worker
updates ETS counters and flushes batches every 10 seconds or at a size threshold.
Do not make database writes, GeoIP downloads or reporting queries part of serving
a page. Use local cached classifiers and prepared, bounded report queries.

Proposed site-local tables:

| Table | Contents |
| --- | --- |
| `analytics_collector` | ID, version, definition, active interval and enabled state |
| `analytics_bucket` | Collector version, resolution, UTC bucket start, canonical dimension value (or total sentinel), additive counters and quality flags |
| `analytics_daily` | Optional daily distinct counts by collector/group; kept separate from additive rollups |
| `analytics_batch` | Random worker epoch and batch sequence for idempotent flush retries |
| `analytics_checkpoint` | Bounded per-version/tier rollup progress and coverage state |

Use a unique key over collector/version/resolution/slot/dimensions, with the
absolute bucket timestamp checked on every update as described below. Store
non-null canonical values for missing dimensions so uniqueness works reliably.
Keep database indexes on collector, resolution and bucket time; do not create a
table or index for every user-defined collector. Start with ordinary PostgreSQL
tables; partition only if measured retention cleanup or query costs justify it.

Flush additive deltas and the batch receipt in one transaction. Retry the same
batch identifier after an ambiguous commit; an already recorded batch must not
increment counters again. Keep receipts longer than the maximum retry window;
never retry a batch after its receipt can have expired. This prevents retry
inflation, not loss of unflushed memory on process/node failure.

#### Capped multi-resolution history

The reference implementation is
[Bjango's iStat Server for Linux](https://github.com/bjango/istatserverlinux),
inspected at commit `11a79fefed90d8c83b56bc6bd1a5f84995fb881b`:

- [`System.h`](https://github.com/bjango/istatserverlinux/blob/11a79fefed90d8c83b56bc6bd1a5f84995fb881b/src/System.h)
  defines `HISTORY_SIZE` as 600. This keeps in-memory histories small at every
  resolution, rather than retaining all samples since startup.
- [`StatBase.cpp`](https://github.com/bjango/istatserverlinux/blob/11a79fefed90d8c83b56bc6bd1a5f84995fb881b/src/stats/StatBase.cpp)
  configures eight active intervals: 1, 6, 144, 1,008, 4,320, 12,960, 25,920 and
  52,560 seconds. At 600 samples, the seven coarser tiers span approximately an
  hour, day, week, month, three months, six months and year. It selects a finer
  source history for each tier and queues deletion of old persisted sample IDs.
  Persistence is periodic pruning, not a literal fixed-slot database ring.
- [`StatsLoad.cpp`](https://github.com/bjango/istatserverlinux/blob/11a79fefed90d8c83b56bc6bd1a5f84995fb881b/src/stats/StatsLoad.cpp)
  illustrates bounded deques, time-window aggregation, restoration of previous
  history and explicit empty samples. Its load measurements use averaging;
  analytics event counts instead need additive totals.
- [`Stats.cpp`](https://github.com/bjango/istatserverlinux/blob/11a79fefed90d8c83b56bc6bd1a5f84995fb881b/src/Stats.cpp)
  batches queued history writes and pruning in a transaction, with subsequent
  flushes scheduled five minutes apart.

Adopt the bounded histories, scheduled aggregation and batched persistence.
Use calendar-friendly minute/quarter-hour/hour/day boundaries for analytics reports, and
explicit dimension budgets. The PostgreSQL slot scheme below is our proposed
adaptation, not a description of iStat's SQLite implementation. There is no need
to copy its unusual sampling intervals or introduce SQLite into Zotonic.

Use a fixed-size history at each resolution, following the iStat-style idea:
recent activity has fine detail, older activity is progressively coarser. Store
aggregate buckets only, never individual events. More traffic increments existing
counters rather than creating one row per event. Both the number of time slots
and the number of dimension groups per slot must be bounded; rollups alone do
not cap storage if new URLs, referrers or collector versions can grow forever.

Initial retention defaults, configurable downwards or within an explicit budget:

| Resolution | Maximum slots | Approximate history | Purpose |
| --- | ---: | --- | --- |
| 1 second, memory only | 120 | 2 minutes | Last-minute activity |
| 1 minute | 360 | 6 hours | Live and recent detail |
| 15 minutes | 672 | 7 days | Day/week graphs |
| 1 hour | 720 | 30 days | Month graphs |
| 1 UTC day | 1,826 | 5 years | Year and multi-year graphs |

This profile reserves 3,578 persistent time slots per dimension group, about 48%
fewer than the earlier 6,866-slot proposal. It trades long retention of minute
detail for smaller storage while preserving exact UTC daily totals and useful
week/month graphs. Each tier divides evenly into the next; chart rendering can
combine these buckets further without adopting iStat's irregular intervals.

Slot counts include the current partial bucket. The retention contract is a
fixed number of buckets, not an exact calendar anniversary. The second-level
history is an optional live display; persistent minute counters are updated
directly from accepted events, so loss of that in-memory display cannot erase a
minute awaiting aggregation.

The persistent pipeline is:

```text
events -> minute -> quarter-hour -> hour -> day -> expire
          360 slots   672 slots    720 slots  1,826 slots
```

Each quarter-hour is the sum of 15 minute buckets, an hour is the sum of four
quarter-hour buckets, and a UTC day is the sum of 24 hour buckets. For example,
3,600 views spread across an hour eventually occupy one
hour bucket per retained dimension group, irrespective of the number of views.
Keep the minute buckets until their own retention expires, allowing recent
detail and long-term trends to coexist. The oldest daily bucket is discarded;
there is no unlimited archive. A future monthly tier must also have a fixed slot
count and an explicit budget, rather than extending retention indefinitely.

Implement each tier as a logical ring in PostgreSQL. For resolution `R` seconds
and `N` slots, compute `bucket_start = floor(timestamp / R) * R` and
`slot = floor(timestamp / R) rem N`. Store both slot and absolute bucket start.
Use uniqueness on collector version, resolution, slot and canonical dimensions;
when reusing a slot, transactionally delete its old groups and write the new
bucket. Check the absolute timestamp on every update to prevent a late event
from overwriting a newer generation of the same slot. A regular bounded cleanup
also expires inactive collectors, whose slots would otherwise never be reused.

Keep a small per-collector/tier checkpoint recording the last completed rollup
and collection coverage, even for empty buckets. A scheduled worker advances it
without requiring new traffic. In one transaction, replace the parent bucket's
complete group set with freshly computed totals and advance the checkpoint.
Replacement makes retrying a rollup idempotent; never add the same child totals
to a parent twice. Serialize flush, rollup and slot reuse through the site worker.

Allow a short, fixed lateness window, initially two minutes. Recompute affected
open parent buckets after a late flush and finalize them only when the window
has closed. Live charts may calculate the current partial quarter-hour/hour/day from retained
children. Do not mix a partial parent with those same children in a total. After
finalization, reject older events with a quality counter rather than reopening
arbitrarily old history.

Normally, reuse a child slot only after its parent is durable. A failed rollup
must not cause retention to expand: retry within the existing capacity, and if
the slot must be reused before recovery, drop the unrolled data and mark the
affected parent interval incomplete. Prefer a visible gap over unbounded storage
or fabricated zero counts. Coverage metadata distinguishes a measured empty
bucket from a missing one, including after restarts and long outages.

#### Bound dimensions and total storage

Give each collector a maximum group count `G` **at every resolution**, including
an `other` group. A quarter-hour union of 15 bounded minute buckets can still exceed
`G`; reapply the cap during each rollup. Choose retained groups deterministically
from the child totals, with stable tie-breaking, and merge the rest into `other`.
Existing child `other` values remain `other`; their original identities cannot
be recovered. Totals stay correct, but coarser rankings have less detail and must
not be described as exact top-N rankings. No permanent, unbounded dictionary of
all previously seen dimension values is needed.

The maximum persistent aggregate row count is calculable before activation:

```text
rows <= sum over retained collector versions (
    minute_slots * minute_group_cap
  + quarter_hour_slots * quarter_hour_group_cap
  + hour_slots   * hour_group_cap
  + day_slots    * day_group_cap
)
```

For example, eight retained collector versions with at most 16 groups at each
tier need at most `8 * 16 * (360 + 672 + 720 + 1826) = 457,984` bucket rows. Their
optional second-resolution rings need at most another 15,360 in-memory entries.
This upper bound stays the same whether the site receives thousands or millions
of events per day. Low-cardinality or ungrouped collectors use much smaller caps;
do not reserve 16 groups for a total that only needs one.

Set a site-wide row budget as well as per-collector limits. Count retained old
versions and disabled collectors against it; a configuration change cannot
allocate unlimited new histories. Reject activation or offer smaller group caps,
shorter retention, or an explicit purge when the reservation does not fit.
Reserve separate bounded allowances for daily visitor counts, batch receipts,
checkpoint/quality metadata, pending batches and identity state. Cap flush rate
and receipt lifetime together so retry bookkeeping is also bounded. Daily
visitor rows use the daily slot/group limits and expire with that tier.

Bound dimension value lengths and the number of measures so row size is bounded
as well. Estimate bytes from measured table/index sizes and show both reserved
rows and actual disk usage in the admin screen. Fixed row counts are not an exact
disk quota: PostgreSQL indexes, WAL, dead tuples and backups also use space.
Configure autovacuum for the bucket tables and monitor bloat; do not run frequent
blocking `VACUUM FULL` operations. Database/backup retention and disk monitoring
remain part of the deployment budget.

#### Reading and combining resolutions

Choose the coarsest available resolution that still supplies useful detail for
the selected period and chart width, with a bounded response such as 300 points.
Group additional adjacent buckets for display when necessary. If a range spans
retention boundaries, either use one common coarser tier or stitch disjoint
intervals and label the resolution change. Never sum overlapping minute, quarter-hour, hour
and daily representations of the same events. Round old query boundaries to
available buckets and show that rounding; do not imply sub-day accuracy from
daily data.

Counts and sums roll up by addition. Derive averages and ratios from their stored
sum/count or numerator/denominator, never by averaging averages. If future module
events add numeric measures, define their merge rule in the measure registry:
min/max merge with min/max, while gauges need last-value timestamps and different
semantics from event counts. Do not add these types merely to support page counts.

Distinct visitors cannot be rolled up by summing groups or time buckets. Keep
the optional daily distinct results separate; longer periods show visitor-days.
Do not fold distinct counts of discarded dimension groups into an alleged unique
`other` count: that requires deduplication before identities are discarded, or
the result must be unavailable. Calendar month/year views can sum daily additive
data; expired fine detail cannot be reconstructed.

Use server receipt time for browser events and request-start wall time for origin
events, with a bounded late-arrival window. Store UTC boundaries; display timezone
and resolution explicitly. Old daily data represents UTC days, even if labels
are localized; it cannot be regrouped exactly into a different timezone's days.

Expose buffer drops, upstream metrics-buffer loss where observable, rejected
late events, failed flushes, collection gaps and last successful flush. The
upstream ring buffer already permits loss, so this is best-effort analytics, not
billing-grade accounting. During database outages retain only a bounded backlog
and drop with counters when full. Module shutdown attempts a bounded flush;
page delivery continues if analytics fail.

The first version supports one collecting node per site. Do not silently sum
per-node daily visitors or sessions. A multi-node deployment needs explicit
ownership/routing to one site aggregator and deduplication before identity-based
metrics are enabled. Add that only when deployment requirements justify it.

### Admin reporting and access

Add a “Site Analytics” admin page with overview cards, a period selector,
small collector graphs and tables of popular content/referrers. Show collection
start, source, timezone, resolution, missing data and approximate labels. Empty
periods before activation or during outages are gaps, not zeros. Live reporting
means recent requests/views, not “people currently online”.

Use normal Zotonic templates and a shared chart component. Start with polling
aggregated model results every 10 seconds while visible; MQTT invalidation can be
added later if useful. Never publish raw access events or identity keys to the
browser. Export the same authorized aggregates as CSV.

Check `z_acl:is_allowed(use, mod_analytics, Context)` in the model,
controller, exports and any future MQTT subscription path. Configuration and
purge actions require administrative rights and normal CSRF protection. Escape
all displayed dimension values. Reports must not reveal private resource titles
through historical statistics. No public counts endpoint by default.

The collectors page supports enable/disable, definition/version inspection,
retention, collection start and storage estimates. Preview configuration cost
before activating another combination. Disabling stops collection; purging is a
separate explicit operation. Cleanup runs in bounded batches and never blocks
the request pipeline.

### Choosing what to record

Give site owners a **Record analytics** screen built around questions they want
answered. Use “recording” in the interface for a configured collector and keep
the internal collector terminology in technical documentation. Separate selecting
what to record from arranging charts: a new chart can reuse an existing recording
without collecting or storing the same data again.

Start with a small gallery of editable presets:

| Question/preset | Event source | Breakdown |
| --- | --- | --- |
| Which content is popular? | Public page requests | Content |
| Which languages are requested for this page? | Public page requests, filtered to a selected page | Language |
| Which devices are used in this country? | Public page requests, filtered to a selected country | Device |
| Where do readers of this page come from? | Public page requests, filtered to a selected page | Referrer hostname |
| Which downloads are used? | A module's registered download event | Its declared format dimension |

Use source-specific wording: a browser-view preset is offered separately from a
request preset. Module presets appear only when their event definitions are
available. Country and visitor/session options explain their prerequisites and
remain unavailable until those features are configured; selecting a preset must
not silently enable identification or change the site's collection policy.

For a custom recording, use a short guided form:

1. **What happened?** Choose one event, such as “Public page requested”, “Page
   viewed in browser” or a module's human-readable event name. Show its definition,
   producer and available measurements. Start with event count; offer daily
   visitor estimates or completed-session measures only for compatible sources.
2. **Which events?** Optionally narrow the population with typed filters, such as
   selected content, content language or traffic class. Use resource pickers and
   declared value lists instead of SQL or expressions. Start with AND between
   filters and OR within a selected list of values. Show missing/unknown values
   explicitly. Available filters come from the selected event schema.
3. **Break down by what?** Choose no breakdown (a total) or one dimension.
   Show compatible choices only. A filter on language
   selects which events count; a language breakdown produces separate counts for
   each language. Explain this distinction with a live sentence and example rows.
4. **How much detail and history?** Offer a recommended storage profile and
   smaller alternatives, with advanced controls for retained groups and tier
   lengths within the site's budget. Explain that older data becomes coarser and
   excess dimension values are combined into “Other”. The group limit includes
   this overflow bucket and any retained unknown value.
5. **Review and start.** Suggest an editable title and show the population,
   breakdown, privacy mode, history profile and remaining storage capacity.
   “Start recording” activates it and shows its actual collection start time.

For example, the review could say: “Count Dutch-language public page requests,
excluding known crawlers, grouped by content. Keep up to 99 content values plus
Other per time bucket; additional values appear as Other. Keep minute detail for six hours,
quarter-hour detail for seven days, hourly detail for 30 days and daily totals
for about five years. Recording starts
now.” Explain that excluding known crawlers still includes unknown automation.
The 100-group example would reserve up to 357,800 aggregate rows under the
default tier lengths, so it must fit the site's remaining budget before starting.

Show a small illustrative table while configuring, clearly marked as an example.
Use real previews only when an existing compatible recording supplies the data;
do not collect temporary raw events or claim to preview unavailable history.
For declared finite dimensions, show the possible group count (for example,
4 configured content languages, plus unknown and any reserved Other bucket). For content
and referrers, explain that cardinality is variable and use the configured cap
for the storage reservation. The normal interface shows “Fits within your
budget” and the remaining allowance; advanced details can show rows and estimated
bytes. Recheck the budget atomically when saving, including concurrent edits.

Provide “Save as draft” so configuration can be prepared without recording.
On activation, check for an equivalent existing recording and offer to use it.
For additive counts, a content recording can supply a total by summing all its
groups, including Other, for the same source, fixed filters and coverage. It can
also report a selected content value if that identity was retained. It cannot
apply a new language filter retrospectively, or recover a page folded into Other.
Do not derive distinct visitor totals by adding per-group distinct counts. When reuse
is not valid, offer a separate recording with its own cost and start date.

The recordings overview shows active, draft, paused and unavailable recordings,
their questions, start dates, history limits, storage reservations and data-quality
indicators. “Paused” stops collection and leaves a gap while existing history
continues to expire. A missing event-producing module marks its recording
unavailable without losing history or repeatedly trying to activate it.

Allow users to duplicate a recording and adjust it. Changes to its event,
filters, breakdown or measurement semantics start a new version; explain that
old data cannot be regrouped retroactively. Labels and chart styles can change
without a new version. Shortening retention or lowering group limits explains
which detail will be lost before applying the change; extending retention only
preserves future data and never restores expired detail. Historical versions
continue to count against the budget until expired or explicitly deleted.

Dashboard filters operate only on the recorded grouping dimension; show the
recording's fixed population filters separately. If a user asks for another
population, offer “Start a filtered recording” with the form pre-filled and a
clear “from now on” start. Keep its breakdown to at most one dimension; do not
offer historical cross-filtering between independent recordings. Viewing and
arranging charts uses the normal analytics access permission; starting, changing
or deleting recordings requires the administrative rights described above.

### Implementation sequence and acceptance checks

1. Build aggregate request collection, public-page eligibility, normalization,
   bounded buffering, schema, retry-safe batches and minute/quarter-hour/hour/day rollups.
   Deliver the overview and default collectors before a general collector editor.
2. Add configuration/versioning, retention, quality indicators, CSV export and
   optional local country lookup. Add module event declarations and recording
   with and without a request. Benchmark representative site traffic and
   dimension cardinality before choosing final limits.
3. Add the optional browser source and its lifecycle/deduplication tests.
4. Add daily visitors and completed-session collectors only after their memory
   bounds, reset behaviour and consent integration are implemented. Defer
   persistent identity and multi-node aggregation.

Acceptance coverage should include public/private pages, alternate representations,
redirects/errors/304s, admin exclusion, bot classification, trusted proxies,
missing dimensions, sanitization and isolation between sites. Cover module events
with/without requests, declaration conflicts, schema changes,
disabled modules, invalid properties and background-job collection permissions.
Verify fresh install
and schema upgrade, ambiguous database commits, process restarts, buffer overflow,
retention/rollup boundaries, UTC midnight and collector version changes. Verify
slot wraparound, late events targeting an old slot generation, deterministic
dimension overflow at every tier, and recovery after rollup failures. Assert
the reserved row bound under sustained traffic, changing dimension values and
repeated collector edits, including disabled histories and receipt cleanup. Check
the recording form's source/dimension compatibility, zero/one-dimension limit,
fixed-filter matching before overflow, equivalent-recording reuse,
concurrent budget reservations, draft activation and version-change explanations.
Check
that ratios use matched populations and that distinct visitors are never summed
across dimensions or presented as cross-day uniques. For browser mode test
background tabs, prerendering, back/forward cache, SPA navigation, retries,
cache-served pages and consent withdrawal. Measure ingestion overhead and storage
at the configured limits, with no per-page database write and no unbounded queue.

## Popular content: bounded rankings over moving periods (implemented November 2026)

The former first-seen admission policy is unsuitable for popular content: a page
arriving after the candidate limit can never enter that minute's ranking, even
when it becomes the busiest page. Page breakdowns now use a **weighted,
mergeable Misra–Gries summary**. Other breakdowns retain their existing policy.

### Alternatives and research

| Method | Advantages | Reason for selection or rejection |
|---|---|---|
| Exact per-page counts in time buckets | Exact ranking within bucket boundaries | State grows with distinct pages; conflicts with a fixed storage budget. |
| First-seen admission / independently truncated top lists | Simple | Permanently loses late candidates; merging truncated lists cannot recover a page that is moderately frequent in many buckets. |
| Space-Saving | Fixed candidate count; replaces weak candidates; useful frequency bounds | A strong alternative, but correct merging needs more than summing retained counters. Lower-bound summaries make our rollup error accounting simpler. |
| Count-Min sketch plus candidate tracking | Compact, mergeable frequency estimates | The sketch alone cannot enumerate page identities; needs a separate candidate algorithm and probabilistic error accounting. |
| Mergeable Misra–Gries | Fixed number of identities, deterministic bounds, weighted updates and merges | Selected: small implementation, no dependencies, directly composes with the existing bucket hierarchy. |
| Exponential decay | Smooth recency score | Does not answer “requests in the last week”; old traffic never expires at a definite boundary. |
| Specialized sliding-window sketches, e.g. ECM | Support time/count windows with probabilistic guarantees | More machinery than needed for this module's fixed reporting periods and existing expiring bucket storage. |

Research sources:

- Cormode, [Misra-Gries Summaries](https://people.csail.mit.edu/rrw/6.045-2020/encalgs-mg.pdf): lower-bound counters, weighted updates, amortized update cost and mergeability. The summary explains the distinction from Space-Saving's upper-bound representation.
- Agarwal et al., [Mergeable Summaries](https://doi.org/10.1145/2500128): bounded summaries which retain their guarantees through repeated merges.
- Metwally, Agrawal and El Abbadi, [Efficient Computation of Frequent and Top-k Elements in Data Streams](https://cs.ucsb.edu/index.php/research/tech-reports/2005-23): Space-Saving and its frequency/top-k guarantees.
- Cormode and Muthukrishnan, [Count-Min Sketch](https://dimacs.rutgers.edu/~graham/pubs/papers/cm-full.pdf): compact frequency point queries; a ranking also needs candidate identities.
- Papapetrou, Garofalakis and Deligiannakis, [Sketch-based Querying of Distributed Sliding-Window Data Streams](https://arxiv.org/abs/1207.0139): ECM sketches for time/count windows and distributed composition.
- Apache [Frequent Items overview](https://datasketches.apache.org/docs/Frequency/FrequentItemsOverview.html): useful precedent for displaying frequency bounds and acknowledging ambiguous ordering. We implement deterministic weighted Misra–Gries, not Apache's optimized algorithm.

### Implemented representation and guarantees

`z_analytics_frequency` stores `#{format => mg1, counters => Map, total => N,
error => E}`. There are at most `groups` candidate counters, including an unknown
page when requests lack an ID. Unlike ordinary breakdowns, no counter is reserved
for Other. Three fixed metadata fields add constant overhead per bucket; no
per-page rows, raw requests or unbounded candidate directory are introduced.

A counter is a lower bound `L`; the actual frequency of that key among recorded
events is in `[L, min(N,L+E)]`. Any absent key has frequency at most `E`. The
summary total remains exact for accepted events, independently of pruning.
Dropped or unrecorded requests remain outside these guarantees and are reported
through the existing coverage/overload status.

With capacity `K` and no legacy overflow, `E <= floor(N/(K+1))`, including after
rollups and report merges. At the default capacity 100, any page exceeding about
one percent of recorded traffic is retained. This does **not** promise the exact
top 20 when thousands of pages have similar frequencies. The dashboard shows up
to 20 candidates ordered by lower bound, with count ranges and a warning that
overlapping ranges cannot certify relative order. Increasing capacity improves
accuracy but must remain within the existing recording/storage budget.

An update increments an existing key or admits a key if there is room. Otherwise
it cancels the minimum of the incoming weight and the smallest counter from all
counters and the incoming key, removes zeros and increments the error offset.
Unit updates use a map lookup except for an overflow decrement scan. Capacity is
at most 100; the existing worker time slice and ring limit still apply.

Merging adds counters by identity, adds totals and error offsets, then (if needed)
subtracts the `(K+1)`th-largest count from every counter, clamps at zero, and adds
the subtracted amount to the error offset. Merge inputs are reduced each step,
so rollups/reports hold at most `2*K` candidate entries transiently. Never sum
candidate counters to obtain event totals. Never subtract summaries to expire
traffic: a report merges only the buckets in its selected window.

### Moving periods and retention

The report selects recent buckets on each request; it is not an all-time count.
One hour and six hours use minute buckets, 24 hours and seven days use 15-minute
buckets, 30 days use hourly buckets, and 365/1,826 days use daily buckets. The
current partial bucket is included. Window starts advance at the selected
resolution: the effective duration is up to one bucket shorter than the nominal
period. No proportional interpolation pretends to reconstruct discarded event
timestamps. The UI states the step size. Counts update on refresh after the
normal writer flush; no second-resolution live sliding window is promised.

Once an old bucket leaves the selected interval, its traffic no longer affects
the ranking. Changing the period recomputes both the ranked list and graph from
the corresponding retained tier. The existing retention slots and reservation
budget are unchanged; no additional time-series tier is needed.

Legacy maps remain readable without rewriting or deleting history. Retained
counts become lower bounds, the old Other count becomes additional uncertainty,
and the full old total is preserved. Unknown identities cannot be recovered.
New data uses summaries automatically for every page-dimension recording.
Normal expiry gradually replaces legacy history. Existing batch receipts,
transaction deadlines, cascade deletion and permission checks are unchanged.
