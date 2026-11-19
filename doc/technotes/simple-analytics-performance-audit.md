# Site Analytics performance audit

Audited 2026-11-07: module observers and HTTP request enrichment, model APIs,
worker/ring ingestion, persistence and rollups, report generation, templates and
assets. Also inspected the local ringbuffer and MQTT callback implementations to
check where queueing occurs. This is a code audit and targeted local verification,
not a production HTTP capacity test.

## Findings and changes

| Area | Finding | Implemented change |
| --- | --- | --- |
| Drain scheduling | 200 events followed by a mandatory 100 ms sleep imposed a ceiling below 2,000 events/s, irrespective of available CPU. | Process at most 200 events per turn. Full chunks immediately schedule another turn; empty buffers wait 100 ms. Returning to the server loop keeps flush/status messages serviceable. |
| Ingestion metadata | Every event copied the entire schema registry from ETS; exact HTTP status validation included a 500-element list. | Event-local ETS rows, with generation and active-recording interest. Validate exact HTTP status with three byte/range checks. Registry snapshots remain available for configuration APIs. |
| Unused events | Both legacy and typed request events entered the ring even when no recordings used one of them. Metadata enrichment also ran without consumers. | Skip queue insertion for inactive event streams. Skip request ACL/resource/UA/GeoIP enrichment when neither applicable request event has an active recording. Custom event input validation remains intact. |
| Recording matching | Every event scanned all recordings, including unrelated and paused ones. | Index active recordings by event and schema version when configuration changes. Fixed filters still run for the matching candidates. |
| Admission bookkeeping | Every accepted recording update filtered the entire recent admission map and updated admission counts that were only used for membership. | Prune once per minute. Reuse admission maps for known groups; count events only in the aggregate map. |
| SQL flush preparation | Each recording scanned the entire batch and queried its continued existence individually. | Partition deltas once and fetch existing recording IDs once under the existing transaction lock. Deleted recordings still cannot be recreated by a frozen batch. |
| Write amplification | Every flush rewrote parent buckets even when their counts/completeness/quality were unchanged. | Add an `IS DISTINCT FROM` condition to bucket upserts. Unchanged parents avoid new tuple versions and associated WAL churn. Minute coverage changes still persist. |
| Retention work | Four expiry DELETE statements per recording ran every ten seconds even when thresholds had not moved. | Run a tier's expiry deletion only when its time boundary advances. Ring slot uniqueness continues to cap physical row count. |

## Measurements and verification

A standalone Erlang microbenchmark wrote and drained 20,000 events, each matching
32 recordings, with 100 group values. It performed 640,000 recording updates,
retained 3,200 counter entries and dropped no events:

| Version | Elapsed |
| --- | ---: |
| Before changes | 1.086286 s |
| After final changes | 0.552312 s |

This local sample uses the actual ringbuffer and worker aggregation code, but
invokes drain turns directly. It excludes intentional timer waits, HTTP, MQTT,
request enrichment, database latency and graph rendering. It indicates roughly
49% less aggregation time in this workload, not a promised requests-per-second
capacity. Run `z_analytics_bench:run/0` from `test/z_analytics_bench.erl` to repeat
it; measurements depend on hardware and competing load.

Targeted tests cover:

- A 10,000-event burst into the 4,096-entry ring: 4,096 accepted events, 5,904
  reported overwritten events, bounded ETS storage and conserved surviving counts.
- Inactive streams avoid the ring; invalid dimensions remain rejected.
- Stale timestamps/generations are dropped and unrelated events do no recording work.
- Exact HTTP status validation preserves the accepted range 100–599.
- Database retry idempotency, count conservation, tier rollups, slot reuse,
  reservation release, deleted-recording non-resurrection and access control.
- PostgreSQL `xmin` stays unchanged for bucket rows after a new, empty batch for
  an already-covered interval: a direct check of avoiding unnecessary rewrites.

Local persistence and ACL integration tests ran on `zotonicwww2`, using temporary
recordings that the tests remove. No live stress traffic was generated.

## Reviewed bounds and remaining costs

- HTTP response hooks still perform visibility/consent checks and coarse metadata
  extraction when there is an active consumer. Resource/configuration lookups use
  Zotonic's existing caches; they are not a guarantee of zero SQL on cache misses.
  Interest is at event level, so a typed-request recording filtered to one type
  can still cause enrichment for another type before the worker applies filters.
- The access-log MQTT subscriber uses a callback, not per-request messages to the
  analytics worker mailbox. MQTT's sidejob limits and the upstream access-log ring
  can discard work before analytics sees it. Analytics drop counters cannot count
  those upstream losses.
- Ring reads synchronize through the ringbuffer process. This and a single worker
  remain throughput limits. The bounded ring deliberately drops oldest events
  instead of backpressuring HTTP. Continuous overload can keep the worker busy;
  scheduling now also targets a five-millisecond drain turn (see the second pass below).
- Hot counters stay capped at 10,000 entries, plus one frozen writer batch. The
  writer is single-flight, including retries; database stalls cannot accumulate
  unbounded batches. Configuration reloads still read recent admission state from
  SQL and run in the worker; they are administrative operations, not per-event work.
- SQL work scales with recordings and affected time buckets, not request volume.
  A typical one-minute flush needs one minute read/write and three parent
  read/write pairs per active recording (eight statements), plus shared transaction
  bookkeeping and occasional expiry work. Busy rollups with changing counts must
  still be written. Transactions share the budget lock with configuration changes,
  so slow storage can delay configuration operations. Flush queries now share a ten-second budget and use short lock waits
  (see the second pass below); pool checkout and transaction cleanup remain external.
- Receipt cleanup/counting and status-page bucket counts are bounded but not free.
  Keep PostgreSQL autovacuum enabled; logical slot limits do not cap WAL or MVCC
  dead-tuple storage. Empty/fully paused sites now skip periodic flushes when no deltas remain.
- Reports use an indexed recording/resolution/time query, at most 1,826 rows per
  recording and 240 graph samples. Five recordings load initially; explicit Load
  more is capped at 32. Ranking may merge up to 182,600 group values for one
  five-year, 100-group recording before reducing to 100 labels. This is bounded
  but remains a relatively expensive administrator read; no shared result cache
  was added because labels and visibility are permission-sensitive. Test concurrent
  long-range dashboard reads before treating this module as a high-volume API.
- The one-writer-node-per-site constraint remains. These counters are best-effort
  analytics, not a durable event ledger. Benchmark realistic traffic and database
  latency before choosing operational capacity targets.


## Second pass: node contention and delayed work

A second audit on 2026-11-07 focused on scheduler contention, concurrent producers,
blocked SQL, control-message backlogs and delayed timers. Additional fixes:

- Drain turns now stop after 200 events **or a five-millisecond elapsed budget**,
  checked between ring reads. This keeps a slow batch from monopolizing the worker
  callback until all 200 events have been processed.
- All queries in a flush share one monotonic ten-second budget. Each driver query
  receives the remaining time, rather than a fresh ten seconds. PostgreSQL lock
  waits are capped at 250 ms; deadlock retries are delegated to the existing
  single-flight writer instead of nesting automatic transaction retries.
- Empty/fully paused sites skip flushes when no deltas remain. Active recordings
  still write coverage even without traffic, so valid zeroes remain distinguishable
  from gaps. Frozen batches still finish before the idle shortcut can apply.
- Up to 64 queued reload notifications are coalesced before refreshing configuration.
  Paused recordings no longer trigger recent-history queries during that refresh.
- Retry age is checked again when the retry timer is delivered. A timer delayed by
  a starved node cannot start a new write attempt after the two-minute window.

### Concurrent load experiment

`test/z_analytics_load_bench.erl` runs in a **separate Erlang VM with `+S 2:2`**,
with one CPU-bound competing process and eight concurrent producers. Each producer
writes 100 batches of 100 events, pausing ten milliseconds between batches. All
events match 32 recordings. A real analytics `gen_server` processes the ring while
300 status probes sample latency, memory and mailbox length. No HTTP traffic or
SQL is part of this experiment, and the live Zotonic node is not stress-tested.

Observed in one run:

| Measurement | Result |
| --- | ---: |
| Produced events | 80,000 |
| Consumed events | 34,821 |
| Reported overwritten events | 45,179 |
| Aggregate counters retained | 3,200 |
| Maximum sampled worker mailbox length | 0 |
| Maximum sampled worker memory | 4,720,560 bytes |
| Status-call p95 | 33 µs |
| Maximum observed status-call latency | 6,259 µs |
| Maximum sampled ring ETS rows | 4,101 |

Consumed plus overwritten events exactly equals produced events. The deliberate
input overload sheds events rather than accumulating an analytics mailbox. The
ring's nominal capacity is 4,096 payloads; metadata and concurrent insert/delete
operations can temporarily make the physical ETS row count slightly larger.
Samples are observations, not guaranteed upper bounds. This test does not establish
an acceptable loss rate or production requests-per-second capacity.

A development-database test briefly held the analytics budget row lock. A competing
flush rolled back in **264,958 µs**, wrote no receipt, and succeeded with exactly one
receipt after the lock was released. Persistence/ACL tests and the unchanged-row
check also pass with the shared query budget. Unit tests additionally cover the
idle-flush shortcut, single-flight behavior and expired retries.

### Limits that remain

The five-millisecond drain budget is cooperative, not preemptive. An individual
ring read synchronizes with the ringbuffer process; its implementation can wait
for an in-flight writer and uses a synchronous call. Scheduler starvation can
therefore exceed the target before the next budget check. Replacing that dependency
with a different queue is outside this module-only change.

The SQL budget includes elapsed time since entering `flush/5`, but it cannot
interrupt database-pool checkout, BEGIN/COMMIT/ROLLBACK handling, or scheduler
starvation outside a query. If checkout consumes the budget, no further flush
query starts. Already-started writes still rely on the database driver's cancellation
and rollback behavior; this is not a hard ten-second end-to-end latency guarantee.
A rollback keeps the frozen batch for the normal bounded retry policy.

Configuration refresh still performs SQL in the worker. Coalescing reduces
administrative bursts, but slow refreshes can delay status calls and ring draining.
Long-range reports and upstream access-log/MQTT pressure remain the costs described
above. They require a realistic end-to-end load test before assigning a capacity SLO.

## Popular-content summary validation (2026-11-07)

Page-dimension recordings now use bounded weighted Misra–Gries summaries (see
`z_analytics_frequency`). The worker reserves the full candidate capacity for
each active minute on first use, avoiding admission-set copies and ensuring
candidate replacement still works when the global entry reservation is full.
Known keys use a map update; only overflow scans the bounded counters. Rollup
queries order children by start for deterministic repeated merges. Reports
reduce after each bucket merge and resolve titles/visibility only for the 20
candidates displayed. No unbounded per-page query or candidate table is added.

Checks completed:

- 25 EUnit tests passed, including 10,000 distinct pages followed by a late
  winner through the real worker, weighted randomized summary trees checked
  against exact counts, capacity one, old overflow conversion and expiry.
- Development-site integration passed: all retention tiers remain bounded,
  totals survive rollups, duplicate batch receipts prevent double counting,
  a two-hour-old leader disappears from the last-hour report but remains in the
  last-six-hours report, and legacy overflow widens the uncertainty bound.
  The existing general storage integration suite also passed. Test recordings
  and batch receipts were removed after the checks.
- Safari verified the ranked list, resource titles, one-hour/24-hour controls,
  and the corresponding graph and bucket-resolution explanation.
- Manual benchmark in a separate VM with `+S 2:2`: 20,000 input events matched
  32 recordings, yielding 640,000 updates. Ordinary breakdowns (100 recurring
  values) took 571,166 microseconds; page summaries (10,000 distinct recurring
  pages) took 591,654 microseconds. Both reserved 3,200 entries and dropped zero
  events. This is a single local ring/aggregation measurement, not an HTTP,
  database or production throughput guarantee.
