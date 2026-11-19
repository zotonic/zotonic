Mediarunner reliability plan
===========================

Investigation and implementation: 2026-11-04. The R1–R4 fixes below are implemented
in the Zotonic checkout and the separate `apps_user/mediarunner` checkout. The
original findings and design follow the implementation notes for context.

Update from the 2026-11-10 [CAFS audit revisit](cafs-audit.md): the client now
waits up to **60 seconds** for HTTP file lookup, following `7440a0d32`. This gives
cold image downloads/conversions time to finish because browsers do not themselves
retry failed image requests. The original five-second choice below is historical;
bounded waiting, shared generation and retryable error responses remain in place.

Implemented behavior
--------------------

- Video tasks retain their original on temporary errors and on failed medium
  commits, for both v2 queue files and v3 CAFS inputs. The v4 wrapper persists retry
  metadata. Automatic backoff starts at 30 seconds, caps at one hour (plus 1–10
  seconds jitter), and pauses after 24 hours since the first recorded failure.
  Operator/integrity errors pause immediately. Paused tasks reconcile hourly.
  `mod_video:retry_task(Task, AdminContext)` explicitly resumes a retained task.
- Only validated terminal conversion failures may commit a broken medium. Unknown
  failures retain input. Task rescheduling replaces the pivot row before starting
  a worker, preventing the scheduler from overwriting the worker's retry decision.
- HTTP file lookups now wait at most 60 seconds (originally five seconds); pending or failed generation returns
  503 with `Retry-After: 2` and `Cache-Control: no-store`. Background lookups default
  to 60 seconds and can use `z_file_request:lookup_file/5` to choose another finite
  wait. A timeout does not cancel shared work. Entries bound waiting calls to 128.
- A monitored resolver keeps cache locks/subscriptions alive while its entry serves
  the result. Invalidation targets the owning entry, which discards the resolver.
  Resolver identity fences late completions. Failed resolution has a two-second
  backoff. Preview failures never generate a placeholder at the successful path.
- The client creates one monotonic deadline before packing/capability selection.
  It bounds hashing, admission, transfers, queueing, command execution, result
  installation and local fallback. `media_runner_wait_timeout` remains a **global**
  setting, in seconds: an explicit value replaces the default of command runtime
  plus 300 seconds. No new site configuration keys are introduced.
- Transient downloads retry the same authenticated endpoint twice before failover.
  Output hashes/sizes and endpoint binding remain enforced. Receipt is asynchronous,
  best effort, and limited to one second within the original deadline. The client
  does not rerender merely because receipt acknowledgement failed.
- Runner execution uses the remaining wire expiry budget, including staging and
  publication. Wire expiry is rounded up to a whole second; machines should keep
  clocks synchronized. There is no extra clock-skew allowance that extends expiry.
- Callback delivery preserves the manifest and result pins. Authenticated status
  remains available during the existing result-retention window (default one day
  after completion), independently of execution expiry. Receipt removes manifest
  and pins; retention cleanup handles lost receipts.
- Monitored guards remove private preview directories, downloads and runner work
  directories on worker death. Extension-appended CAFS outputs now use monitored
  `z_tempfile` paths. Preview storage is scanned in batches of 500 paths per cleanup
  cycle for managed `.preview-*` directories and `.download-*` files older than one
  day, excluding active guards and symlinks. Ordinary previews are untouched.
  CAFS renewal failures terminate their linked owner; claim tokens still fence
  final publication. Database-free library preview storage remains unchanged.

Roll out the runner retention fix before relying on status recovery. Older runners
still work with the client, but cannot restore manifests they already discarded.
Authenticated remote cancellation is a separate, deferred protocol enhancement;
abandoned jobs on older/current runners remain bounded by their submitted expiry.
The four core correctness fixes do not depend on cancellation.

Validation (2026-11-04)
----------------------

- The affected Zotonic selection passed **95 tests**, covering CAFS, old files,
  copying, previews, video, filestore, backup and database-free sites.
- Final focused reruns passed **29 runner/reliability tests** and **22 file-entry,
  CAFS, backup and no-database tests** after the ownership/cleanup refinements.
- Runner CI passed **34 standalone tests** and its full real HTTPS integration
  suite. This includes shared uploads, corrupt and killed upload recovery, a
  70 MiB upload, a verified 72 MiB result, receipt handling, result reuse and restart
  recovery. The added retention check recovers a delivered manifest after execution
  expiry and verifies idempotent receipt. A two-second client test verifies lost
  callback recovery before the operation deadline.
- Full `make dialyzer` completed without warnings (1235 analyzed project files).
  Both checkouts pass `git diff --check`.
- The CI harness now starts its guard registry and discovers only the runner site;
  its existing rollback assertions match Zotonic's `{rollback, Value}` contract.

The original design below is retained for context. The implementation notes above
record the shipped choices; remote cancellation remains deferred.

Current findings
----------------

The investigation found these behaviors (now corrected as described above):

| Finding | Current behavior | Required result |
| --- | --- | --- |
| R1: video failures | `z_video_convert:do_convert/2` replaces the medium with a broken marker for every conversion error. Successful replacement removes the task and durable CAFS pin. Legacy v2 tasks also unlink their queue file. | Temporary infrastructure failure retains the original, task and processing identity for retry. |
| R2: request latency | `z_file_entry:lookup/1` uses an infinite `gen_statem` call; locate/generation runs inside the entry process. | HTTP waiting is bounded independently of rendering; work can finish for a later request. |
| R3: deadlines | The runner deadline starts after packing/capability selection. Uploads and each download can take another hour; polling, receipt and local fallback have separate budgets. | Every phase uses a common remaining operation budget, including fallback. |
| R4: placeholders | `z_media_preview` reduces runner errors to `convert_error`; `z_file_locate` may persist a placeholder at the successful preview's pathname. Other errors become missing parts. | Transient failure remains retryable and is neither a persistent placeholder nor a cached 404. |

The existing input protection is useful and should be preserved: CAFS pins renew
while workers run, local readers use private hard links, remote cache readers hold
locks, and downloads verify size/hash before publication. Late callbacks cannot
directly overwrite local files after their registration is removed.

The external runner source is available locally in `apps_user/mediarunner` (the
build directory links to it). Inspection adds these requirements:

 - `mediarunner_store:finish_delivery/3` clears the result manifest after successful
   callback delivery, before the client's `received` acknowledgement. A failed
   download cannot subsequently recover that manifest from status.
 - `m_mediarunner_job` stops returning status at the job's `expires` timestamp.
   The runner checks expiry before execution, but does not clamp the command's
   execution timeout to the remaining job lifetime. Result publication can itself
   wait up to another hour.
 - Input pins survive processing; result pins survive callback delivery until
   receipt or configured result retention (default one day after completion).
   Callback expiry and result retention are therefore different lifetimes already,
   but the status API does not expose a usable recovery window consistently.
 - There is no cancellation control endpoint. Failover can leave the old remote
   job running even though its callback can no longer be accepted.

This investigation used source inspection and the earlier diagnostic results.
It did not run a live runner outage experiment or establish which revision is
currently deployed externally.

1. Preserve errors and video retry state (R1, foundation for R4)
--------------------------------------------------------------

Introduce one internal classification helper shared by video and previews. Keep
the original reason in logs and task state; classification is a policy decision,
not a replacement for diagnostic information.

| Category | Examples | Action |
| --- | --- | --- |
| Retryable infrastructure | Connection loss, callback/status timeout, temporary capacity refusal, retryable transfer failure, runner `worker_failed` or `job_expired` | Retry with backoff; retain inputs. |
| Operator action needed | Invalid credentials/configuration, protocol mismatch, invalid hash/result, local permission or disk failure | Pause automatic rendering; retain recoverable task/input and expose the reason. Do not bypass validation or fall back because of an integrity error. |
| Terminal processing | Validated command failure on unsuitable media, unsupported content, processing limit reached | Record a terminal conversion failure under an explicit policy. |
| Obsolete work | Medium replaced/deleted or processing number no longer matches | Discard output and remove that task's retention references. |

Do not classify everything wrapped in `media_runner_processing` as terminal:
`worker_failed` and `job_expired` are infrastructure failures too. Treat a command
runtime limit separately from the client's transport/operation deadline.

Change `do_convert/2` to return an explicit outcome. Both CAFS v3 and legacy v2
workers use it: only success, obsolete work or a committed terminal outcome may
remove the task. The v2 queue file must survive transient failure just as the v3
CAFS source pin does. If writing the resulting medium fails, keep the task/input.

Persist retry count, last reason, next attempt and paused state with the existing
unique video task, not only in worker memory. Preserve loading of existing v2/v3
task tuples; introduce a versioned representation if the arguments change.
Coordinate scheduler and worker updates so `convert_task/2` cannot overwrite a
new backoff decision with its current unconditional one-hour delay. Keep one active
worker per processing identity and retain the existing expected-medium checks.

Proposed retry policy: exponential delay from 30 seconds to one hour, with jitter,
and a finite automatic retry window (initially 24 hours). Exhaustion pauses the
conversion without discarding its source. Retain a lightweight reconciliation path
for paused tasks so deletion/replacement still removes their pins/queue files;
paused must not mean an immortal retention reference. Provide an explicit retry
operation and structured logs/status for operators. These values are proposed
policy defaults, not existing configuration.

2. Bound the complete operation and make results recoverable (R3)
----------------------------------------------------------------

Separate three budgets:

 - HTTP request waiting: short, independent of the processing worker.
 - Command runtime: the existing media-profile execution limit.
 - Total operation: packing, capability lookup, admission, upload, queueing,
   execution, download, verification and publication, shared across runner attempts
   and optional local fallback.

Create the operation deadline before packing. Use monotonic milliseconds locally,
with one helper computing the remaining budget. Pass it through protocol upload,
HTTP download, status polling and result installation. Cap per-transfer limits by
the remaining budget; do not multiply the budget by the output-file count. Bound
hashing/staging too, using deadline checks between chunks or a monitored worker.
Check expiry again before publishing verified outputs. A late result must not
silently be installed as a successful in-budget operation.

Use profile-aware total budgets instead of giving an image job the default
12-hour-plus-five-minute runner wait. Preserve the existing system-level
`media_runner_wait_timeout` as an explicit override/cap, document its precise
meaning, and choose/validate finite defaults for images, probes and videos using
slow-transfer tests. Do not invent a new deadline for each failover. Local fallback
must use only the remaining budget and the existing sandbox/integrity policy.

On the runner, clamp execution and result-publication work to the accepted remaining
job lifetime. Wire expiry uses wall-clock time, so document tolerated clock skew
and test it; do not transmit the client's monotonic timestamp between machines.

Keep the completed result manifest and its pins until an idempotent `received`
acknowledgement or an explicit result-retention deadline. Callback delivery only
marks delivery complete and clears callback credentials; it is not receipt of the
files. Allow authenticated status/result recovery during that retention window,
without allowing an expired job to start executing again. This requires changes
in `m_mediarunner_job`, `mediarunner_store`, `mediarunner_queue` and
`mediarunner_results` as well as the Zotonic client.

Retry downloading the selected runner's completed result within the same remaining
budget before rendering again elsewhere. Preserve endpoint/token binding on every
fetch. Send `received` only after every output has been verified and installed.
Acknowledgement is bounded and best effort: failure must not turn an already
installed result into another render. Lost acknowledgements leave pins to expire
through the runner's retention policy.

3. Bound browser waits and preserve preview retryability (R2, R4)
----------------------------------------------------------------

Add an explicit lookup wait option through `z_file_request`, `z_file_sup` and
`z_file_entry`. The HTTP controller should initially wait at most five seconds,
then return a retryable response; background callers can choose a different
bounded wait. A timed-out browser waiter must not own or cancel shared generation.

Keep the file entry responsive by moving expensive resolution/generation into a
monitored worker. Fence completion with a generation reference, discard stale
completions after invalidation, and bound/coalesce waiting requests. Removing an
expired or disconnected waiter must not stop a job needed by others.

Do not simply return worker-owned `#part_cache{}` values and let that worker exit:
cache locks, monitors and physical-file subscriptions have process ownership.
Prefer a completion notification followed by file-entry-owned resolution of the
published result, or an explicit acknowledged transfer of those protections.
Retain CAFS derivative claims and the existing local-path lock for non-CAFS
previews; add no database dependency to database-free library preview generation.

Preserve retryable errors all the way through `z_media_preview`, `z_file_locate`
and the file entry. In particular, do not reduce them to `#part_missing{}`.
`controller_file:service_available/1` already distinguishes non-ENOENT failures;
handle pending/transient work explicitly as HTTP 503 with `Retry-After` and
`Cache-Control: no-store`. Keep authorization and signature validation intact,
and do not expose internal runner errors in the response.

Never write an error placeholder under the normal preview pathname or CAFS
success identity. If retaining a placeholder for a terminal image error, serve it
as a separately identified response. Add short failure backoff to prevent a burst
of requests from repeatedly starting the same failed render. Distinguish that
backoff from the long retention policy for obsolete previews. Existing persistent
placeholders cannot reliably be identified from filenames alone; do not delete
all old previews as an attempted migration.

4. Close worker-lifetime gaps and limit duplicate remote work
------------------------------------------------------------

Use monitored ownership for private preview directories, extension-appended output
paths and `.download-*` files. Current `after` cleanup is sufficient for ordinary
errors but not `kill` or VM termination. Add a bounded orphan sweep for managed
artifacts after a safety interval, without traversing arbitrary paths or deleting
active work. Keep database-free library previews themselves outside this sweep.

Make CAFS pin/claim renewal failure visible to the owning worker. Stop publication
when the lease is lost; do not continue silently after an unlinked renewer dies.
Keep the claim-token check at publication as the final guard against stale owners.

Add an authenticated, owner-scoped, idempotent runner cancellation endpoint as a
separate protocol enhancement. Cancellation of queued work releases pins; running
work releases them only after its worker has stopped. The client sends bounded
best-effort cancellation after abandoning an attempt, never instead of fencing
late results. Older runners may finish/cache abandoned work; client correctness
must not depend on cancellation support. Advertise support through capabilities.

5. Verification, documentation and rollout
------------------------------------------

Implement in the order above, with error preservation first. Add regressions for:

 - Video failure then success for CAFS v3 and legacy v2, including restart,
   replacement/deletion during a retry, retry exhaustion and failed medium commit.
 - Slow or trickling uploads/downloads, multiple outputs, expired deadlines,
   final status probes, local fallback and stalled receipt acknowledgement.
 - Callback success followed by failed download: recover the same manifest and
   bytes without another render. Also test lost callback and lost acknowledgement.
 - Simultaneous preview requests where one HTTP wait expires but generation
   continues; a later request gets the real output. Verify 503/Retry-After/no-store,
   preserved ACLs and no placeholder poisoning or transient negative-cache entry.
 - Worker kill, node restart, renewal failure, remote cancellation races and stale
   completion. Check both reference retention and removal of partial artifacts.
 - Legacy originals with/without checksums, remote-only inputs and database-free
   library previews, preserving their distinct storage/cleanup behavior.

Use deterministic barriers and injectable short deadlines in tests, not hour-long
sleeps. Run the existing CAFS, preview, video, runner, filestore and backup suites,
then Dialyzer. Finally run a real client/runner integration test with interrupted
transfers and delayed callbacks; mocks alone cannot establish socket/lease behavior.

Document the error categories, retry/paused states, deadline scopes, receipt versus
callback semantics and recovery procedures. New per-site settings, if needed,
should follow `site.key`; existing global runner settings remain global rather
than being silently renamed. Update configuration catalogs alongside their code.

Implement runner changes in the actual `apps_user/mediarunner` source checkout,
not generated build artifacts. Deploy backward-compatible manifest retention and
capability changes first, then the client changes. Test old/new combinations;
client-side timeout and retry fixes must work with an older runner. Cancellation
can follow after the four core findings are resolved.
