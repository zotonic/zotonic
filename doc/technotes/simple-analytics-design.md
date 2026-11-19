# Site Analytics: implementation design

Status: implementation design with initial implementation, 6 November 2026. Companion to the
[research and product proposal](simple-analytics.md). This document defines the
module boundaries, data contracts and algorithms for implementing that proposal.
The design below includes later stages. The implementation status section distinguishes
the working module from those extensions; the module README defines the current API.

## 1. Decisions and scope

Implement `zotonic_mod_analytics`, with `mod_analytics` as its module
entry point. Use the site's PostgreSQL database, ETS and the existing ringbuffer
library. Require no external analytics service or additional database.

The first usable release collects public page requests and declared module events.
Every collector selects one event schema, fixed filters, zero or one grouping
dimension, and additive measures. The initial measure is `count`. The same engine
handles request-associated and background module events. Browser page views,
country lookup and daily visitor/session estimates are separately enabled stages.

The following are invariants, not just UI defaults:

- No persisted individual events, raw requests, IPs, user agents or identity keys.
- Zero or one grouping dimension per collector, in addition to time.
- Filters run before grouping; grouping never changes the population selected.
- Every retained collector version reserves storage, including paused histories.
- Time slots, groups, field lengths, queues and metadata all have explicit limits.
- A failed analytics operation must not fail the site's primary operation.
- Only complete, covered periods can be reported as zero. Missing data is a gap.
- A persisted counter is never incremented twice by retrying the same flush.
- Request counts, browser views and custom events are separate populations.

The supported deployment is one collecting node per site. The module must reject
an unsupported multi-writer configuration rather than silently merge independently
collected visitor/session data. This is not a durable event bus or billing ledger.

## 2. Application and process structure

Proposed application layout:

```text
apps/zotonic_mod_analytics/
  rebar.config
  include/analytics.hrl
  src/zotonic_mod_analytics.app.src
  src/mod_analytics.erl
  src/models/m_analytics.erl
  src/controllers/controller_admin_analytics.erl
  src/controllers/controller_analytics_collect.erl  # browser stage
  src/support/z_analytics_worker.erl
  src/support/z_analytics_writer.erl
  src/support/z_analytics_registry.erl
  src/support/z_analytics_normalize.erl
  src/support/z_analytics_collectors.erl
  src/support/z_analytics_rollup.erl
  src/support/z_analytics_query.erl
  priv/dispatch/dispatch
  priv/templates/admin_analytics.tpl
  priv/templates/_analytics_recording_form.tpl
  priv/templates/_analytics_chart.tpl
  priv/templates/_html_body.tpl                          # browser stage
  priv/lib/js/analytics.js                        # browser stage
  test/
```

Keep pure validation, normalization, filtering and rollup functions separate from
processes and database access. The public model owns site selection, configuration
validation and ACL boundaries. Do not expose support-process internals to templates.

Make `mod_analytics` a small OTP supervisor plus module hooks. Zotonic's
[module manager](../../apps/zotonic_core/src/support/z_module_manager.erl) explicitly
supports modules implementing the supervisor behaviour and passes startup arguments
containing `{context, Context}`. Its two permanent children, under `one_for_all`,
are the aggregation worker and the database writer. Restarting either resets their
shared in-flight protocol and marks a possible collection gap. Use bounded restart
intensity; repeated failure leaves collection unavailable instead of a restart loop.

| Component | Responsibility |
| --- | --- |
| Module entry point | Child specs, schema hook, admin menu and notification glue |
| Aggregation worker | Registry snapshot, collector matching, private ETS counters, timers, ingest draining and bounded flush scheduling |
| Database writer | One serialized database operation at a time: flush, rollup, configuration or cleanup |
| Ingest ring | Bounded normalized event transfer from request/module callbacks to the worker |
| Query support | Read-only, ACL-filtered queries; never send dashboard queries through the ingestion worker |

The worker owns its ETS tables. Only a small read-only ingestion configuration
snapshot is published for producer-side validation; replace snapshots atomically
with a generation number. Use site-scoped names derived from the configured site,
not atoms derived from event names or request values.

The existing ringbuffer has its own supervised owner. Create a per-site buffer at
startup and explicitly discard/recreate leftover contents on a new worker epoch.
Unsubscribe before shutdown and delete the buffer after a bounded final flush.
Treat a producer racing shutdown as a harmless disabled/unavailable result.

## 3. Existing integration points

| Existing code | Intended use |
| --- | --- |
| [`z_stats`](../../apps/zotonic_core/src/support/z_stats.erl) | Subscribe to the site's `$SYS/site/<site>/log/access` stream |
| [`mod_syslog`](../../apps/zotonic_mod_syslog/src/mod_syslog.erl) | Pattern for subscription callback and site-specific topic |
| [`z_cowmachine_middleware`](../../apps/zotonic_core/src/support/z_cowmachine_middleware.erl) | `#middleware{on = handled}` fold while final request context is available |
| [`z_context`](../../apps/zotonic_core/src/support/z_context.erl) | `set_req_metrics/2`, `get_reqdata/1`, response headers and request context helpers |
| [`controller_page`](../../apps/zotonic_mod_base/src/controllers/controller_page.erl) | Existing `rsc_id` metric for resource pages |
| [`mod_logging`](../../apps/zotonic_mod_logging/src/mod_logging.erl) | Existing per-site ringbuffer creation and bounded draining pattern |
| [`controller_admin_statistics`](../../apps/zotonic_mod_admin_statistics/src/controllers/controller_admin_statistics.erl) | Admin authorization, no-cache and no-index response pattern |

At handled time, compute a small nested `analytics` metrics map:

```erlang
#{eligible => true,
  excluded => false,
  page_key => {rsc, 123},
  content_type => <<"text/html">>,
  policy_generation => 7}.
```

The observer returns the context unchanged after adding metadata. It resolves
public eligibility in an anonymous context, not merely the current user's right
to view the resource. Determine editor/admin exclusion and consent before dropping
user/session information. Use the negotiated response type, not request `Accept`.
If the response type is unavailable at this hook for a controller, require an
explicit controller integration and default to exclusion. Verify this timing with
an actual `controller_page` response in the first implementation milestone.

Provide proposed `mark_page(PageKey, Context)` for custom public HTML controllers;
it adds the marker but cannot override exclusion policy. A dispatch allowlist and
public-resource validation determine which standard/custom pages are eligible.
No raw path becomes a page identity automatically. Resource visibility checks
belong here, not in the later metrics consumer, and can use Zotonic's normal caches.

The metrics callback counts only eligible, non-excluded completed GET/200 HTML
responses. Normalize the log's `timestamp`, language, peer IP, user agent and
referrer immediately. Never enqueue the complete access-log map. Requests without
the marker do not count. Thus assets, redirects, private pages and the browser
collection endpoint cannot accidentally count as page views.

The upstream metrics buffers can lose entries, and MQTT delivery behaviour also
needs load verification. The module's bounded ring limits its own retained data;
it does not establish a new memory bound for Zotonic's entire metrics subsystem.
Keep the callback short, avoid per-message spawned workers, and measure callback
mailbox growth under overload before release.

## 4. Event registry and public Erlang API

Declare a new notification in the module's include file:

```erlang
-record(analytics_events, {}).
```

Build the registry through a fold notification after module observers are ready.
Rebuild after module activation/deactivation using a coalesced refresh message.
A missing producer makes affected collectors unavailable; it must not reinterpret
their old schema. A declaration is bounded, typed data:

```erlang
#{name => <<"mod_newsletter:sent">>,
  version => 1,
  title => <<"Newsletter sent">>,
  request_mode => optional,
  dimensions => #{
      <<"delivery_kind">> => #{
          type => enum,
          values => [<<"digest">>, <<"individual">>],
          title => <<"Delivery kind">>
      }
  },
  measures => [count]}.
```

Namespace names by producing module. Validate all declarations before publishing
a new registry snapshot: reject duplicate name/version pairs, reserved dimension
names, oversized schemas, arbitrary code and unsupported types. Offer only enum,
bounded text and resource/page-key types initially. Resource dimensions must
specify public eligibility and display handling; arbitrary resource IDs are not
an automatic permission to record private content. Built-in request dimensions
are server-derived and cannot be overridden by properties.

Proposed APIs, exported from `m_analytics`:

```erlang
-spec record_event(Name, Properties, Context) -> Result
    when
        Name :: binary(),
        Properties :: map(),
        Context :: z:context(),
        Result :: ok | {error, term()}.

-spec record_event(Name, Properties, Req, Context) -> Result
    when
        Name :: binary(),
        Properties :: map(),
        Req :: map() | undefined,
        Context :: z:context(),
        Result :: ok | {error, term()}.
```

The three-argument form always supplies `undefined` for Req. The four-argument
form accepts the request from the same Context and site, explicitly selected by
the caller. Perform permission/exclusion checks and enrichment synchronously,
then discard Req. Background events carry only declared properties; never derive
an identity from an authentication context. A job originating from user activity
must preserve the already evaluated collection permission in its domain workflow
and call only when permitted. The API cannot infer consent from a background job.

Use `ok` for disabled/excluded events and for best-effort admission. Return bounded
validation errors such as `unknown_event`, `invalid_properties` or
`request_required`. Do not map this function to an unrestricted `m_post` path.
The module emitting a domain event calls after its transaction succeeds, accepting
that a crash between commit and recording can lose that statistic. A retried job
may count twice; adding a durable outbox is outside this module's initial scope.

The existing `ringbuffer:write/2` overwrites old entries on overflow; it does not
return `queue_full`. Retain that simple best-effort behaviour and observe skipped
entries while reading. This refines the earlier generic API sketch: `ok` means
accepted for attempted collection, not retained or durably stored. Do not add an
unbounded retry queue to make the API appear lossless.

## 5. Normalized event and collector contracts

Only the following form enters the aggregate-only ingest ring:

```erlang
#{event => <<"page_request">>,
  schema_version => 1,
  event_time => 1793966400,       % UTC Unix seconds
  received_time => 1793966401,
  registry_generation => 4,
  config_generation => 12,
  dimensions => #{
      <<"page_key">> => {rsc, 123},
      <<"language">> => <<"nl">>,
      <<"traffic_class">> => other,
      <<"country">> => unknown
  }}.
```

Normalize only dimensions used by active collectors or policy checks. Unknown
and unavailable values are explicit tagged values. Limit the serialized event
size, number of fields and Unicode text length. Remove query strings, fragments,
full referrers, cookies, authentication IDs and raw network/device data before
this boundary. No incoming event can choose a site, collector ID or bucket slot.

Use a canonical internal collector definition:

```erlang
#{event => <<"page_request">>,
  schema_version => 1,
  filters => #{
      <<"language">> => [<<"nl">>],
      <<"traffic_class">> => [other, unknown]
  },
  group_by => [<<"page_key">>],
  measures => [count],
  group_limit => 100,
  tiers => [{60, 360}, {900, 672}, {3600, 720}, {86400, 1826}]}.
```

External JSON uses binary names; resolve enums through a fixed lookup rather than
creating atoms. Sort/deduplicate filter values for canonicalization. Reject empty
filter lists and unknown fields, more than one grouping field, unsupported
measures and incompatible request dimensions. Omitted filter means any value;
unknown matches only an explicit unknown choice when that field is filtered.

Compile validated collectors into an event-name/schema-version lookup. Per event:

1. Validate timestamps, schema/generation and collector active interval.
2. Evaluate AND between filter fields and membership within each value list.
3. Resolve `total`, `unknown` or the selected single dimension value.
4. Update the current minute's admitted group, or its overflow group.
5. Update the separate optional second-resolution live ring.

Cap collector fan-out per event through the site collector limit. No arbitrary
expressions, SQL, resource joins or dynamically generated collectors run here.

For a grouped minute with limit G, reserve one slot for `other`; admit at most
G-1 distinct values in first-observed order, including `unknown`. Admission stays
fixed until the minute expires. Persist admitted values with the counts so a
restart can reload still-open minute admissions before processing new events.
Undefinable values are `unknown`, never the overflow value `other`. An ungrouped
collector has one `total` group and no overflow. Namespace sentinels separately
from textual values so a real hostname named “other” cannot collide.

## 6. Storage schema and reservations

Use `manage_schema/2` for complete fresh-install schema and incremental upgrades.
The following is a logical schema; migrations should express these constraints
explicitly rather than relying only on application validation.

| Table | Key and essential fields |
| --- | --- |
| `analytics_recording` | `id`; title, desired state, current version, edit revision, created time |
| `analytics_collector` | `id`; recording ID, version, immutable canonical definition, schema fingerprint, start/end time, reservation; unique recording/version |
| `analytics_bucket` | PK `(collector_id, resolution, slot, group_key)`; absolute bucket start, nonnegative `count bigint`, optional matched session totals |
| `analytics_slot` | PK `(collector_id, resolution, slot)`; absolute bucket start, coverage, final/provisional state, quality flags, retained-group metadata if needed |
| `analytics_checkpoint` | PK `(collector_id, resolution)`; finalized-through time, maintenance progress |
| `analytics_batch` | PK `(epoch, sequence)`; payload checksum, committed time |
| `analytics_budget` | Singleton site row; hard limits, reserved aggregate/metadata rows, configuration revision |
| `analytics_daily` | Optional later-stage daily estimate rows; separate from additive buckets |

The slot header is a deliberate addition to the research schema: it records
coverage even when a bucket has no count rows. Quality flags cannot live only
on populated groups, because silence and collection failure must differ.
Use a fixed-size coverage representation per tier: 60 one-second coverage bits
for minute headers; 15, 4 and 24 child-completeness bits for coarser headers.
Derive completeness from durable child coverage and propagate quality flags.
Coverage means the pipeline was operating, not proof that upstream delivered
every request. Known upstream losses make coverage incomplete.

Store a bounded canonical binary/text encoding for `group_key`, with type tags
for total, unknown, other, resource ID and declared text. A collector's grouping
field lives in its immutable definition, so it need not be repeated per row.
Keep foreign keys from slots/buckets to the collector; do not foreign-key page
keys to `rsc`, since resource deletion must not delete historical totals.
Index bucket queries by collector/resolution/bucket_start, and receipts by commit
time. Do not add one table or index per recording.

For every retained version reserve:

```text
aggregate_rows = sum(tier.slots * tier.group_limit)
slot_headers   = sum(tier.slots)
checkpoints    = number_of_tiers
```

With default tiers, that is 3,578 * G aggregate rows and 3,578 slot headers.
Eight retained versions at G=16 reserve 457,984 aggregate rows, plus 28,624 slot
headers and 32 checkpoints. These are separate budgets, not hidden additions to
the aggregate-row cap. Retain the independent caps for drafts, event declarations,
receipts, identity state and process memory. Actual disk includes indexes, MVCC,
WAL and backups; row reservations do not promise a precise filesystem quota.

Creation/activation locks the singleton budget row in a transaction, recalculates
available reservations and inserts the immutable version. Equivalent canonical
definitions can reuse an existing recording; a fingerprint is an index aid,
not a substitute for comparing definitions. A semantic edit closes the old
version and reserves a new one. Paused or superseded histories retain their
reservation until expired/purged. Labels and chart presentation do not create
versions. Retention/group-limit changes create versions initially, avoiding
in-place reinterpretation of a ring; offer explicit purge of old detail when
immediate space recovery is required.

## 7. Ingestion, batching and writer protocol

Starting limits, to validate with load tests rather than present as performance
claims:

| Resource | Initial bound |
| --- | ---: |
| Normalized ingest ring | 4,096 events, each at most 2 KiB encoded |
| Drain work per worker turn | 200 events or a 5 ms cooperative budget; yield between chunks, wait 100 ms when empty |
| Normal flush cadence | Every 10 seconds |
| In-flight write batches | 1 |
| Unsent aggregate keys | 10,000, additionally constrained by a memory budget |
| Active/retained collector versions | Site-configured; default budget reserves for at most 32 |
| Filter fields / values per field | 8 / 100 |
| Report points / returned groups | 300 / 100 |

The producer writes directly to the ring and never sends one Erlang mailbox
message per event. The worker drains on a short self-scheduled timer, with at
most one pending drain tick. It owns hot counters and polls the ring's skipped
count. Unknown loss timing marks all potentially affected active recordings
incomplete; do not guess which visitor or group was lost.

Every flush freezes an immutable bounded map of deltas and coverage, assigns a
random worker epoch plus increasing sequence, and sends one command to the writer.
New events accumulate in a separate bounded map. The writer replies committed or
retryable failure. The worker retains the frozen batch until the result is known;
it never folds it back into a newer batch. At most one write command is outstanding,
so a slow database cannot grow the writer's mailbox without bound.

On size pressure, flush early but enforce a minimum interval (initially one
second). If the in-flight batch prevents progress, reject new aggregate updates
at the configured memory bound and mark loss. Do not spawn more writers. Database
transactions have statement/lock timeouts and bounded retries with backoff.

A flush transaction:

1. Insert the batch receipt with checksum; on conflict verify the checksum and
   acknowledge the already committed batch without applying its payload again.
2. Validate each target slot's generation and the finalized watermark.
3. Reuse expired slots only through the generation-safe procedure below.
4. Upsert additive group deltas, slot coverage and bounded quality metadata.
5. Commit all changes, including the receipt, together.

Drop the frozen batch only after acknowledgment. If the connection fails after
commit, resend exactly the same batch ID and payload. For a process restart,
unknown unflushed data is lost and coverage is conservative; do not replay a
partially reconstructed batch under a new ID.

Keep receipts for 24 hours and forbid retries older than one hour. Enforce the
one-batch-per-second ceiling, including process restarts through a persisted
admission/receipt limit, and a hard receipt-row cap. If receipts cannot be safely
expired, stop accepting flushes rather than deleting deduplication evidence.
The writer also handles rollups and cleanup, scheduled fairly between flushes.
Queries use separate bounded database calls and cannot starve ingestion by
occupying its command queue.

Configuration changes are low-frequency writer operations. Pause worker draining,
finish the old batch, persist the new version/generation, then publish its snapshot
and resume. Tag queued events with their producer generation; discard mismatches
with a quality indication rather than applying new filters to old queued events.
Record the cutover interval. Do not flush on every individual event merely to
make configuration boundaries exact.

## 8. Multi-resolution rollups

| Resolution R (seconds) | Slots N | Retained history including open bucket |
| ---: | ---: | --- |
| 1, memory only | 120 | 2 minutes |
| 60 | 360 | 6 hours |
| 900 | 672 | 7 days |
| 3,600 | 720 | 30 days |
| 86,400 | 1,826 | About 5 years |

Persistent flow: minute → quarter-hour → hour → UTC day. The live second ring is
not a prerequisite for persisted minute counts. Higher tiers merge 15, 4 and 24
children respectively. All buckets are half-open intervals `[start, end)`.

```erlang
bucket_start(Timestamp, Resolution) ->
    (Timestamp div Resolution) * Resolution.

bucket_slot(Timestamp, Resolution, Slots) ->
    (Timestamp div Resolution) rem Slots.
```

Inputs are validated nonnegative Unix seconds. Use monotonic time for process
scheduling and elapsed timeout calculations, UTC wall time for bucket identities.
A backwards clock jump must not reopen a finalized bucket; flag and discard
invalid events. A forward jump creates explicit missing coverage, with bounded
catch-up work rather than iterating over every second of a long outage.

### Finalization and late arrival

Allow event-time lateness of two minutes. Origin events use request-start time;
browser and background events use server receipt time. Very long origin requests
can therefore be rejected as late even if just completed; count that rejection.
Admission-time lateness and database flush delay are different: an event admitted
on time must not become late solely because its batch waited for the database.

Advance the durable minute watermark only after all accepted batches through that
boundary have committed or been explicitly discarded. Stop advancing it while
an earlier batch is unresolved. Drain queued events before advancing, applying
lateness against the worker's current watermark; events still delayed upstream
can be rejected and must mark quality loss. Do not promise that a producer's
best-effort `ok` prevents such rejection. A parent is final only when all expected children
are final or explicitly incomplete. A timer finalizes empty minutes too. A rollup
transaction writes the parent's complete replacement and its checkpoint together;
repeating it cannot double-count. Optional provisional charts sum disjoint current
children and never add a stored parent covering those same children.

### Group reduction

At each rollup, first sum child counts by canonical group key. Preserve child
`other` as `other`; its members cannot be reconstructed. If known groups exceed
G-1, keep the G-1 largest totals, sorting ties by canonical key, and add all other
counts to `other`. This differs from minute admission's first-seen policy, but
both preserve additive totals and keep at most G groups. For G=1 in a grouped
collector everything is Other; the UI should recommend an ungrouped total instead.

Run this reduction as a pure function. Its key invariant is:

```text
sum(parent.counts) == sum(all child.counts)
```

Missing child coverage propagates to the parent even if some counts exist.
Rankings are approximate after overflow; no parent can recover content identities
already merged into a child's Other bucket. Distinct visitors do not use this
algorithm.

### Slot reuse and maintenance

Within the single writer transaction, lock the slot header and compare absolute
bucket starts. Equal generation means update; an older incoming generation is
rejected; a newer generation requires clearing the old groups/header first.
Before clearing, roll up required old children if possible. A failure cannot
allocate an extra ring slot: mark the relevant parent incomplete before dropping
unrolled data. The timestamp fence is mandatory even when modulo slots match.

Expire paused/deactivated histories on the same schedule. Catch-up is bounded by
retained slot counts and a per-turn maintenance limit. Never generate years of
empty intermediate buckets after downtime. Preserve gaps within retained tiers;
anything older than the retention horizon can be removed directly. Clean child
rows before releasing a version's reservation, and use bounded purge batches
with a persisted cursor so an interrupted purge can resume.

## 9. Reporting API and admin workflow

Proposed model paths; every path performs its own authorization:

| Operation | Permission | Behaviour |
| --- | --- | --- |
| GET registry, recordings, status, series | `use mod_analytics` | Return only validated definitions/aggregates and bounded query results |
| GET storage details | Site administrator | Reservations and measured usage |
| POST preview/draft/activate/pause/resume | Site administrator | Validate full payload, optimistic edit revision and budget |
| POST change-version/purge | Site administrator | Explicit semantic change or destructive history removal |
| CSV export | `use mod_analytics` | Same query and resource visibility rules as chart data |

Use `z_acl:is_admin/1` for configuration, following existing admin configuration
models; use `z_acl:is_allowed(use, mod_analytics, Context)` for reports.
HTTP/postback mutations use normal Zotonic CSRF protections. Model callbacks are
also reachable through MQTT, so they recheck ACLs and payloads independently.
No public aggregate API or arbitrary custom-event POST path is included.

The chart query accepts recording/version, from/to, supported measure and an
optional selection on the one stored grouping dimension. It returns effective
bounds, resolution, points, quality, collection start and source. Reject filters
on dimensions absent from the recording. Fixed population filters are returned
as read-only metadata, not reinterpreted at query time.

Choose a tier covering the requested range and sufficient chart detail, then
combine adjacent buckets to at most 300 output points. Prefer one common tier;
if stitching is needed, split on aligned boundaries and use disjoint intervals.
Old daily buckets cannot answer arbitrary local-time boundaries exactly. Expose
UTC alignment and any rounding. Query-side top groups sum only retained identities;
return omitted displayed groups in a presentation Other total separately from
stored overflow, with a clear approximation flag.

Never turn an absent count row into zero without a complete slot header. Preserve
incomplete intervals even when values are nonzero. Render titles at query time
with normal resource visibility checks; use neutral labels for deleted/invisible
resources without exposing their identity or title. Escape all dimension labels
and protect CSV cells against spreadsheet formula interpretation.

The admin UI offers presets and a guided form: event → fixed filters → optional
single breakdown → retention/group budget → review/start. Preview uses schema
examples or compatible existing aggregates, never temporary raw-event recording.
Use a single-select breakdown control. Display the storage impact of filters as
another collector reservation, not as free extra analysis. Detect equivalent
recordings before activation, but also enforce equivalence/budget checks server-side.

Draft → active → paused → active is an operational state flow. Module absence
adds an unavailable status without changing the user's desired state. Semantic
edits create a new version; old versions remain readable. A resumed version keeps
its gap, and expired detail remains expired. Purging is a separate explicit action.
Poll visible charts every ten seconds; stop polling hidden views. Use Zotonic
translation tags, dispatched URLs and existing admin template conventions.

## 10. Optional extensions and boundaries

### Browser source

Add a dedicated same-origin POST controller with a 2 KiB payload bound, content-type
validation, Origin/Fetch Metadata checks where available and bounded rate limiting.
It must not create a session. Derive site and network metadata on the server;
validate page key and current public eligibility. Accept only declared fields for
the built-in browser-view event, never arbitrary custom schemas.

The script sends one event on first visible document activation, another on visible
back/forward-cache restoration, and explicit SPA route notifications. It generates
a per-view random ID in memory and reuses it for a bounded retry. Deduplicate by
that ID in bounded TTL memory; on capacity pressure drop new events rather than
evicting live IDs and claiming retry-safe counts. Restart clears this state and
may permit duplicate retries; mark the affected interval accordingly. No visitor
ID is embedded in cacheable HTML, cookies or local storage.

Include through the usual `_html_body.tpl` all-include and asset helpers. Recheck
consent on activation and withdrawal. Browser events feed the same normalized
pipeline but never merge with request counts or extend sessions across sources.

### Country lookup

An optional local database reader returns only a country code. Refresh databases
outside request handling and atomically swap the reader. Missing/stale lookup is
unknown. Use Zotonic's resolved peer IP and documented trusted-proxy configuration,
not a client-supplied forwarded value. No network lookup per event.

### Daily visitors and sessions

Keep this behind a separate collection mode and implementation milestone. Derive
the site-scoped daily HMAC key before discarding IP/user agent. Only this mode may
carry an ephemeral identity key into bounded private memory; never put it in SQL,
logs or report payloads. Destroy secrets and keys at UTC midnight.

For distinct counts, assign the event to its daily collector group before adding
the key to that group's bounded set. Deduplicate within the daily Other set too;
never compute its distinct total by summing discarded groups. Persist absolute
cardinalities with a day/epoch fence, not additive minute deltas. After an uncertain
restart or identity-memory overflow, stop that day's estimate and mark it incomplete
rather than summing newly recreated identities. Resume normally next UTC day.
Weekly/monthly reports can show visitor-days, not cross-day unique visitors.

Session state is scoped to one selected source and daily identity: last seen,
page count and entry dimensions only. Complete after 30 minutes idle or at midnight;
persist matched `completed_sessions` and `completed_session_pages` through the
same retry-safe batching path. Attribute to completion time and entry dimensions.
Session filters select entry attributes; they do not selectively remove pages
inside a session and still call the result pages/session. Display this definition.
Custom module events do not count as pages or extend sessions. A crash loses
unfinished sessions and creates incomplete coverage, not synthetic completions.

Long-lived new/returning identity, general numeric gauges, durable job deduplication,
cross-site reports and multi-node aggregation remain out of scope.

## 11. Validation and delivery

Implement in reviewable stages:

1. **Core contracts:** module skeleton, registry, pure normalization/filtering,
   one-dimension validation, schema and storage reservation. Cover fresh install,
   upgrades and idempotent schema management.
2. **Request/custom ingest:** eligibility hook, bounded ring, worker/writer protocol,
   minute persistence and basic admin totals. Verify actual content-type/metrics
   timing and ensure analytics failure cannot fail a page response.
3. **History:** pure reduction, tier rollups, slot headers, watermark/fencing,
   receipt retry handling, retention and purge. Deliver charts before a general
   recording editor.
4. **Recording UI:** presets, drafts, filters, budget checks, versions, exports and
   quality explanations. Add country lookup independently.
5. **Optional measurement:** browser lifecycle and consent handling, followed by
   daily identity/session support only when its bounded-state and reset rules pass.

Test the important invariants rather than mirroring functions:

- Property tests: count conservation across rollups/overflow; filter order does not
  change the population; normalization never leaves sensitive fields; row counts
  stay within reservations as dimension values change.
- Database tests: ambiguous commit retry, stale slot generations, slot wraparound,
  rollup retry, empty versus missing coverage, concurrent activation budgets,
  version changes and restart during purge.
- Integration tests: GET/HEAD/200/304/redirect/error, public/private HTML and alternate
  representations, admin exclusion, module events with/without Req, producer module
  removal, cross-site isolation and model ACLs outside the admin page.
- Failure tests: full ingest ring, slow/disconnected database, worker/writer crash,
  exhausted hot counters, wall-clock jumps and long downtime. Verify bounded memory,
  visible quality loss and no duplicate persisted batches.
- Optional-stage tests: browser prerender/background/bfcache/SPA/retries, consent
  withdrawal, daily midnight/reset, unknown geolocation and session denominator
  alignment.

Load-test at the configured collector/group limits with high-cardinality input and
slow SQL. Record request overhead, CPU, peak ETS/mailbox memory, flush/rollup time,
row growth and disk/index size. Tune numeric defaults from those measurements;
do not weaken fixed limits to hide overload. No raw event archive is needed for
verification: use synthetic deterministic event streams and expected aggregates.

## 16. Initial implementation status

The initial implementation is in [`apps/zotonic_mod_analytics`](../../apps/zotonic_mod_analytics/README.md),
with module `mod_analytics` and model `m_analytics`. It implements request counts,
declared background/request events, fixed filters, one optional dimension, bounded
rollups, an administrator recording form, pause/resume and aggregate reports.
Optional local country lookup uses an already enabled `mod_geoip`. Reports use
Zotonic's existing `{% chart %}` scomp; its line renderer supports missing samples.

Implementation refinements relative to the detailed design above:

- Pure validation, registry and reduction share `z_analytics`; request normalization
  is `z_analytics_request`. SQL, schema installation and persistence live in `m_analytics`, alongside its
  ACL-checked public APIs. The worker and writer call its trusted `store_*` functions.
- A bucket stores a bounded group map in one PostgreSQL row, keyed by collector,
  resolution and modulo slot, with an absolute timestamp protecting slot reuse.
  Reservations still count group/time slots, including Other. This reduces row and
  index overhead without removing group or time limits.
- Immutable flush receipts and all changed buckets commit in one transaction.
  Parents are recomputed from their bounded children, including partial current
  intervals. Minute coverage and quality flags distinguish gaps from zero.
- Recordings are immutable definitions. Changed selections create a new recording;
  paused definitions retain their budget until explicitly purged through the API.
- Custom schemas support bounded text and enums. Schema authors must bump their
  version when semantics change and must exclude personal data themselves.
- The deployment contract is one collecting node per site; distributed ownership
  and failover fencing are not implemented.

Browser page-view ingestion, visitor/session estimates, the optional one-second
live tier, CSV export, a guided schema-dependent form and a browser-consent bridge
remain later stages. The initial request collector has a fail-closed opt-in policy
when `require_consent` is enabled, using a trusted request-context flag. It does
not infer consent from the browser's local storage. See the README for exact
retention, shutdown-loss behavior, configuration and integration examples.

### Separate dashboard and configuration (7 November 2026)

`admin_analytics` shows all retained recordings in saved order in a graph-focused
dashboard, bounded by the 32-recording site limit. A shared
period selector applies to all panels. The first panel spans the grid; the others
use two columns on wider screens and one on mobile. Breakdown details are secondary
to graphs and period totals.

`admin_analytics_config` is a separate administrator-only page. It holds the
recording form, pause/resume controls, storage and collection status, and a
Zotonic sorter/sortable list. A signed sort event calls `m_analytics:reorder/2`;
server-side admin authorization remains mandatory. Schema version 2 adds
`analytics_collector.sort_order`, independent of immutable collection definitions.
Existing installations retain ID order, new recordings append, and a reorder is
committed atomically under the budget lock after checking a complete permutation
of current IDs. Missing, duplicate and stale IDs are rejected. The dashboard and
configuration read the same site-wide order, with ID as a stable tie-breaker.

### Access control

The dashboard, recordings and report model paths require `use mod_analytics`.
The configuration page, configuration/status/event-registry model paths, and all
recording mutations require site administrator access (`use mod_admin_config`).
These are independent permissions, enforced in controllers and models as well as
navigation/templates. Configuration has a separate System menu entry so an
administrator can reach it without passing through the dashboard. Trusted Erlang
event-ingestion APIs remain independent of report-viewing permissions.

### Dispatch request classification

Dispatch rules can specify `{analytics, Type}` with standard types `page`, `api`,
`download` and `asset`, or a fixed custom atom/binary label. `{analytics, false}`
disables tracking. For compatibility, an absent option defaults to `page` for
`controller_page`/`controller_template` and `api` for `controller_api`; other controllers are opt-in.
The configuration model inspects `z_dispatcher:dispatchinfo/1` on each page load
and lists the effective types for breakdown/filter configuration.

The `request` event includes `type`, HTTP `method`, response `status` class,
`language`, `country` and `traffic`. Existing `page_request` and `api_request`
recordings remain supported. Labels are bounded to 64 bytes and restricted to
lowercase identifiers; no paths or request payloads become dimensions. The
`page` classification retains the public HTML GET/200 policy; other classified
routes include error responses. Privacy exclusions apply to every type.

The `request` and `api_request` events also expose `http_status`, the exact final
HTTP response code as a decimal string (100–599), for breakdowns and fixed
filters. `status` retains its existing response-class meaning for compatibility.
The new dimension does not change existing recording definitions or historical
counts; new exact-status recordings start without historical data.


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

## Implemented popular-content summary

Page-dimension recordings supersede the first-seen/Other policy described above.
See [the research decision and guarantees](simple-analytics.md#popular-content-bounded-rankings-over-moving-periods-implemented-november-2026).

The pure `z_analytics_frequency` module owns versioned summaries, weighted updates,
merge/reduction, exact totals, and ranked lower/upper bounds. The worker reserves
`groups` hot entries when starting a page-minute summary, keeps no separate
admission set for it, and passes the immutable summary in the existing batch.
The writer merges it into the minute bucket under the existing idempotent receipt.
Every parent rollup merges/reduces child summaries; the model does the same for
the selected reporting interval. The page panel shows a horizontal bar chart of candidate lower bounds and a
top-20 list with uncertainty ranges. It skips time-series chart preparation
(indexing, point/gap expansion and timestamp formatting). Ordinary dimension
panels retain their time graphs. Summary totals and time buckets remain necessary
for frequency bounds, moving periods and rollups; no graph-only stored data exists.
No database schema migration or new dependency is needed: the existing bytea
payload holds the versioned summary. Legacy payload conversion is lazy and
conservative, retaining anonymous overflow as an error bound.

## Standard SMTP recording

Schema version 3 adds an active `smtp` recording with the single enum dimension
`outcome` (`sent`, `retry`, `failed`, `bounced`). Fresh installs include it;
upgrades append it without altering existing recording order and respect the
storage budget. The standard limit is five groups (four outcomes plus overflow).
The normal graph/report path and permissions apply.

Module observers map `email_sent` (non-final only), `email_failed`, and
`email_bounced` to an outcome-only event through the bounded worker ingestion API.
Final success notifications are ignored because they normally confirm an earlier
send. Bounce-reason failures map to bounced, non-final failures to retry, and
other final failures to failed. No request enrichment, recipient identity,
message identifier, response text or email contents enter the analytics buffer.
This measures outgoing transport notifications, including repeated retries, not
unique messages or guaranteed delivery. The module README defines the semantics
and provider limitations in detail.
