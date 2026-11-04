# CAFS audit

Date: 2026-11-03. Scope: the working-tree CAFS implementation and its integration
with media insertion, copying, previews, cleanup, filestore, video and backup.
This is a review of the implementation before fixes for the findings below.


## Remediation status

A1–A9 have been addressed in the working tree. The findings below preserve the
original audit evidence; they describe the pre-fix implementation. The separate
mediarunner follow-up R1–R4 is implemented; see the reliability implementation notes.

| Finding | Implemented correction |
| --- | --- |
| A1 | File-cache identity includes normalized lookup roots and filters, separating internal archive access from public serving. |
| A2 | Unversioned URLs gain a first-observed edit baseline instead of blanket expiry; expired usages also clean exact legacy preview paths. |
| A3 | Pending CAFS deletes advance their retry timestamp so later objects can be collected. |
| A4 | Undispatched deletes can be cancelled under a filestore row fence; dispatched deletes return a controlled error, and partial staging pins are released. |
| A5 | Filestore prefix deletion uses literal prefix matching; repeated deletion requests preserve the original retention timestamp. |
| A6 | Forbidden/mismatched remote deletes retain registry rows, record failure and defer retries. |
| A7 | Derivative identity contains the effective rendering metadata, and the converter uses that same snapshot. |
| A8 | Legacy cleanup rechecks live references against concurrent medium writes and retains failed work with a retry timestamp. |
| A9 | Filename and preview transitions use SQL `IS DISTINCT FROM`; the installer updates the existing trigger. |

Validation after remediation: **63 targeted tests passed**, including 11 new
regressions and updated preview-retention tests. `make dialyzer` completed with
no warnings (1,229 project modules analyzed), and `git diff --check` passed.
The test suite covers fresh setup and repeated filestore fence upgrades, private
archive/cache access, missing/stale legacy checksums, pending-deletion reuploads,
GC fairness, literal prefix deletion, forbidden deletes, metadata-sensitive
previews, failed local/remote cleanup, live references and SQL NULL transitions.
Logs are in `/tmp/cafs-audit/fixes-tests-final.log` and
`/tmp/cafs-audit/fixes-dialyzer-final.log`. Remote deletion callbacks are mocked;
these checks do not constitute live cloud-store fault injection.

Old unversioned URLs cannot reveal edit changes made before their first observed
baseline. They are retained conservatively. Already-dispatched deletes must still
complete before the same bytes can be reuploaded; a timeout cannot safely be
interpreted as cancellation. Existing deleted filestore rows are conservatively
fenced during schema upgrade.

## Assessment

The on-demand migration approach is sound: an existing `medium.digest` is not
proof that bytes have been placed in CAFS. Local legacy files are hashed when
needed; remote legacy files are fetched only for an operation needing their
bytes. Copying creates CAFS mappings and computes the destination checksum from
the actual content, including when the legacy checksum is absent or incorrect.
There is no need for a bulk remote download.

The implementation is not ready to deploy unchanged. In particular, internal
file lookups can bypass the resource ACL through the shared file cache; old
preview URLs expire even when nothing changed; and blocked remote deletion can
stop garbage collection for unrelated objects.

## File and checksum coverage

| File state | Current behavior | Audit result |
| --- | --- | --- |
| New owned upload | Hash completed bytes, verify staged copy, attach mapping with medium transaction | Normal insertion and deduplication covered; pending-delete collision fails as described in A4 |
| Legacy local file, no SHA-256 | Keep legacy until needed; copy hashes bytes and promotes owned source | Reproduced successfully |
| Legacy local file, correct SHA-256 | Still establish CAFS from actual bytes rather than trusting metadata | Reproduced successfully |
| Legacy local file, stale SHA-256 | Copy uses actual hash; existing source digest is not necessarily rewritten | Reproduced successfully; source metadata can remain stale |
| Legacy remote file, no SHA-256 | Reading metadata/installing CAFS does not download; copy fetches on demand | Reproduced with mocked remote lookup |
| Legacy remote file, existing SHA-256 | Same on-demand fetch, not a checksum-only alias to nonexistent CAFS bytes | Reproduced with mocked remote lookup |
| Already mapped CAFS file | Copy clones the reference; no second legacy download | Reproduced, including after remote legacy promotion |
| Non-owned original or preview copied to another resource | Snapshot into a new owned CAFS alias instead of mapping the mutable source | Covered by existing copy tests |
| Missing source bytes | Cannot establish a new CAFS object merely from a legacy checksum | Correct restriction; callers must handle failure |

`m_media:copy/3` strictly prepares the associated preview before replacing the
destination. The older `duplicate/3` path remains best-effort for a missing
preview. A successful copy and a best-effort duplicate should not be treated as
identical error contracts.

## Findings

P1 means fix before deployment. P2 means a correctness or lifecycle defect that
also needs addressing. “Inherited” identifies existing integration behavior
exercised by CAFS, rather than a defect introduced solely in the new model.

### A1 — P1: Internal archive lookup can expose private media

Locations: `z_file_sup:ensure_file_process/6` (line 42),
`z_file_entry:reg_name/1` (line 117), the `archive` branch in `z_file_locate`, and
`m_media:duplicate_file/4` (line 635).

The file-entry cache is keyed by request path, without the lookup roots. An
internal `[archive]` lookup resolves without a resource ACL. A subsequent public
lookup of the same path reuses that entry. `controller_file` relies on its
visibility information, so the missing ACL matters at the serving boundary.

Reproduced both with a new private CAFS upload and by copying a private legacy
medium: the resource was not anonymously visible, but the resulting public file
lookup was. The cache design is inherited; CAFS promotion/copy exercises it.

Fix: preserve resource authorization on archive resolution and/or separate
internal lookup cache entries from public serving entries. Test internal-first,
public-first, private/public aliases sharing a hash, and ACL changes.

### A2 — P1: All pre-upgrade preview URLs eventually stop working

Location: `z_cafs_preview:prepare/5` (line 74), `current_edits/3`, and the preview
lookup in `z_file_locate`.

Every URL without `editrev` gets the installation's `legacy_since` deadline.
After the configured retention period, it returns `enoent` even if the source,
mediaclass and edits have never changed. Reproduced with an unchanged current
medium and an upgrade timestamp 91 days earlier.

This is deliberate in the current implementation, but exceeds the requested
collection policy for deleted sources and changed classes/edits. Stored HTML,
email and external links can break. Rejecting the URL also does not itself
remove the old physical preview from the legacy preview directory.

Fix: define compatibility for unversioned URLs explicitly. Keep unchanged legacy
URLs usable; collect legacy files under an independently tracked policy. Do not
use the upgrade date alone as evidence that a preview is obsolete.

### A3 — P1: Stuck remote deletion starves unrelated CAFS garbage collection

Location: `m_cafs:maintenance/1` (line 487), `collect/2` and
`delete_object_locked/2` (line 524).

Maintenance selects the oldest 100 overdue objects. Objects already `deleting`
keep their old `gc_after` while a filestore row or upload queue entry exists.
With remote retention set to Never, a disabled/read-only backend, or persistent
delete errors, the same rows can occupy every batch indefinitely.

Reproduced with 100 pending remote deletions and a newer eligible object: two
maintenance passes left the eligible object uncollected.

Fix: schedule deletion retries separately, or advance their retry timestamp with
bounded backoff. Ensure pending deletions cannot consume the entire collection
budget.

### A4 — P1: Reupload during pending deletion crashes and can remain blocked

Location: `m_cafs:stage_hashed/3` (line 229), `stage_medium/2` (line 326), and
`m_media` insertion (line 1123).

An existing object in `deleting` state returns `{error, deleting}`. The upload
staging caller matches only `{ok, Staged}`, producing a badmatch. Reproduced by
uploading identical bytes while their hash was pending deletion. If remote
deletion never completes, that content cannot be uploaded again through this
path indefinitely.

Fix: define safe retry/revival behavior and propagate a controlled error. Any
revival must coordinate with remote deletion already in flight; simply changing
the row back to `ready` is unsafe.

### A5 — P1: Legacy preview prefix deletion can delete unrelated previews

Location: `m_filestore:mark_deleted/2` (line 510). Inherited.

The prefix is passed directly to SQL `LIKE`. An underscore in a normal archive
filename matches any character, and percent signs are also wildcards.
Reproduced: deleting the prefix `a_b.jpg(` marked both `a_b.jpg(31x).jpg` and
`axb.jpg(31x).jpg` as deleted.

Fix: use literal prefix matching or escape all LIKE metacharacters. Cover
underscores, percent signs and backslashes in tests.

### A6 — P2: Remote deletion errors discard the only cleanup reference

Location: `mod_filestore:start_deleter_writable/2` around line 1084 and
`delete_ready_1/5` (line 1146). Inherited, with consequences for CAFS tombstones.

A forbidden delete response purges the filestore row; a credential location
mismatch can do the same. Reproduced the forbidden callback removing the row.
This is not proof that the remote object was deleted. CAFS then sees no remote
reference and may discard its own tombstone, leaving untracked remote bytes.

Fix: retain an actionable retry/error record unless deletion succeeded or the
object is known absent. A permissions or configuration failure is not absence.

### A7 — P2: Shared preview identity omits metadata used by the renderer

Location: `z_file_locate:shared_preview/6` (line 408),
`m_media:identify_medium_filename/2` (line 256), and
`z_media_preview:cmd_args/…` around line 296.

The key includes source hash, expanded filters, extension and renderer version.
Rendering also reads medium metadata, including orientation and dimensions,
through the logical original filename. Two records can therefore produce
different rendering commands while using the same derivative key.

Reproduced with identical image bytes in two records, one with orientation 1 and
one with orientation 6: commands differed but the requested previews shared the
same output. This matters for imported or corrected legacy metadata.

Fix: derive rendering metadata consistently from the bytes, or include all
effective rendering inputs in the derivative identity.

### A8 — P2: Legacy cleanup drops failures and does not recheck live references

Location: `z_media_cleanup_server:do_cleanup_1/2` (line 118) and
`do_cleanup_file/2` (line 136). Inherited; confirmed by code inspection.

Local unlink and filestore notification results are ignored before the cleanup
queue entry is removed. Failed deletion is not retried. Although CAFS alias
retirement checks live medium references, the following legacy unlink and
preview prefix deletion run unconditionally. A filename reused or shared by a
current record can therefore lose its legacy bytes/previews.

Fix: recheck live original and preview references before physical cleanup;
retain failed work with a retry schedule. Treat already absent files as success.

### A9 — P2: Clearing legacy filenames to SQL NULL misses cleanup

Location: `z_install:medium_update_function/0` around lines 680–692. Inherited.

The update trigger compares old/new filenames with `!=`. A transition to SQL
NULL is unknown rather than true, so the old file is not queued. Reproduced by
clearing an owned legacy filename: no `medium_deleted` row was created.
CAFS's separate trigger uses `IS DISTINCT FROM`, so mapping retirement and
legacy physical cleanup behave differently.

Fix: use `IS DISTINCT FROM` for both original and preview transitions, and
ensure the installer upgrades the existing trigger definition.

## Other review observations

- Final source removal is lazy. CAFS aliases, recovery retention, active pins and
  derivative references can legitimately postpone physical deletion. Removing a
  medium is not a synchronous promise that all source/preview bytes disappear.
- Preview invalidation for mediaclass/edit changes applies to the CAFS registry;
  old physical previews also need a collection policy. Retaining URL tombstones
  prevents stale requests from restarting their grace period, but creates
  growing metadata that needs operational monitoring.
- Preview generation promotes an archive source whenever a medium ID exists,
  without the ownership distinction used by copying. Review mutable non-owned
  archive references before relying on this behavior. This observation does not
  apply to the separate library-asset lookup path.
- Staging verifies newly copied bytes, but trusts an already-existing canonical
  file based on its presence. It does not repair or detect corruption of that
  file. Remote reads also are not a full content-integrity scrub. An optional
  verification/repair path would improve recovery without requiring a bulk fetch.
- Video conversion uses persistent CAFS pins and supports legacy queued tasks.
  Backup archives include the CAFS directory; remote-only data still depends on
  the remote storage and registry, as in the existing backup design.

## Validation and limits

Ran the 52 existing targeted tests for media copying, CAFS, previews, media
identification, video, backup and filestore queue/read-only/retry behavior.
All passed. Also ran 10 temporary audit probes covering the local checksum
matrix, private file cache behavior, prefix deletion, forbidden deletion,
blocked GC, legacy URL expiry, pending-delete reupload, rendering metadata,
NULL cleanup and remote legacy migration. All probes completed as expected.
These probes intentionally assert observed defects; their passing does not mean
those behaviors are correct.

The combined run reported **62 tests passed**. Diagnostic source and logs were
saved locally under `/tmp/cafs-audit/`; temporary probes were removed from the
normal test suite. No runtime implementation was changed by this audit.
The preceding Dialyzer run was clean; it was not rerun for this documentation-only
audit result.

Remote migration used a mocked remote lookup with real database state; deletion
callbacks were exercised directly. This was not a live S3/WebDAV end-to-end test,
a production data migration, or an exhaustive concurrency/crash fault campaign.

Recommended order: fix A1 first; then A3/A4 and A5/A6; resolve A2 compatibility;
then A7–A9. Convert the diagnostic cases into tests asserting corrected behavior
as each fix is implemented.

## Follow-up: external mediarunner and latency

The implementation and rollout notes are in
[mediarunner-reliability.md](mediarunner-reliability.md) (2026-11-04). R1–R4 are
implemented, together with runner-side result retention and worker cleanup.
The descriptions and diagnostic validation below are the historical findings,
retained to explain the changes. Remote cancellation remains a separate enhancement.

Reviewed the client runner protocol, upload/download handling, callback registry,
CAFS input scopes and derivative claims, file-entry serving, and video task
completion. The original findings concerned the then-current code, including the
renewable derivative claim now present in `m_cafs`.

### R1 — P1: Temporary runner failures permanently fail video tasks

Locations: `z_video_convert:do_convert/2` (line 123),
`handle_cast/2` (line 80), and `remove_task/1` (line 203).

`do_convert` maps every conversion error to `insert_broken`, including
`{media_runner_unavailable, callback_timeout}`. If installing the broken medium
succeeds, the outer CAFS worker sees success, removes the persistent task and
releases its durable source pin. Its comment promising retry on errors does not
cover errors already converted into a successful broken-medium replacement.
The original CAFS object is not immediately erased, but automatic retry and its
explicit retention root are lost.

Reproduced by injecting a runner callback timeout into the real CAFS video task:
the medium became broken and the durable source pin disappeared.

Fix: propagate transient transport/admission/download failures to the task
scheduler, retaining the task and source pin with bounded retry/backoff. Reserve
broken-medium replacement for terminal processing failures or an explicit final
retry policy. Apply the same distinction to legacy `convert_v2` tasks, whose
current path also deletes the queue file after `do_convert` returns.

### R2 — P2: Public file requests have no bounded wait for remote previews

Locations: `z_file_entry:lookup/1` (line 156), `locate/3` (line 202),
`z_file_sup:lookup_file/1` (line 55).

The lookup uses `gen_statem:call/2`, whose default timeout is infinity (verified
against the installed OTP implementation). Preview generation runs synchronously
inside the entry's locate handler. A slow upload, runner queue, render or download
therefore holds the calling request until completion or an independent upstream
request timeout. The default runner wait budget is 12 hours plus five minutes;
the normal ImageMagick execution limit is only two minutes, but does not include
remote queueing and transfers. Other lookups for that file also queue behind it.
This applies to legacy sources fetched/promoted on first preview access too.

A focused probe verified a lookup still succeeds after more than five seconds;
it does not have the five-second default of `gen_server:call/2`. No HTTP proxy
configuration was tested, so the precise browser-visible timeout depends on the
deployment. The issue is an unbounded application-level wait, not premature CAFS
source deletion.

Fix: give lookup an explicit bounded waiting contract, catch timeout, and return
an intentional retryable response while a separately managed generation remains
active. Avoid holding a browser request for the runner's potentially hours-long
budget. CAFS derivative waiters have their own 120-second deadline; align that
with the serving contract rather than treating it as the render timeout.

### R3 — P2: Runner deadline does not bound file transfers

Locations: `z_media_runner:ensure_uploaded/6` (line 331),
`z_media_runner_protocol:upload/5` (line 513),
`z_media_runner_download:fetch/3`, and `z_media_runner:await_result/5` (line 204).

Control admission/reservation requests respect the remaining job budget. Actual
uploads use a fixed one-hour timeout; each output download independently uses
one hour, regardless of `media_runner_deadline`. Downloading starts after the
result arrives, and can publish successfully after that deadline. Status polls
can overrun by their fixed five-second timeout; the final received acknowledgement
also uses a separate request timeout. Packing/hashing and capability selection
precede creation of the job deadline.

A probe passed an already-expired deadline to the download path and confirmed
that it still requested a 3,600,000 ms download timeout. This bounds each transfer,
but does not provide a total end-to-end bound. CAFS pins/claims remain held while
the work waits, so the immediate risk is worker/resource occupancy and poor
latency, rather than GC deleting the active input.

Fix: define separate render and total-operation budgets; carry an absolute
remaining deadline through uploads, downloads, polls and acknowledgement. Include
queueing and transfer time explicitly. Keep the receipt acknowledgement best
effort so a slow acknowledgement cannot unnecessarily delay a verified result.

### R4 — P2: Transient preview failure can leave a persistent placeholder

Locations: `z_media_preview:convert_temp/8` (line 166),
`z_file_locate:shared_preview/6` (line 408), `convert_error_part/4` (line 464),
and `locate_source_preview/5` (line 252).

All runner errors are collapsed to `convert_error`. The caller then tries to
render a placeholder at the requested legacy preview pathname. If the runner
recovers for that second operation, or fallback succeeds, the placeholder stays
there. Subsequent lookups prefer that existing file when no CAFS derivative has
been bound, rather than retrying the original. A transient network failure can
thus leave an incorrect preview after service recovery.

Confirmed by code inspection, not an end-to-end outage reproduction. Fix:
preserve transient error types, use a retryable response, and do not persist an
error placeholder under a successful preview's identity. If placeholders are
cached, use a separate short-lived cache entry with explicit retry semantics.

### File lifetime and late results

- CAFS `with_local_file/3` holds a renewable pin throughout the synchronous runner
  call. Local input uses a private hard link (copy across filesystems), so
  filestore unlink/rename does not invalidate it. Remote cache inputs are locked
  for the worker lifetime. Legacy checksums do not replace obtaining actual input
  bytes; after promotion the same protections apply.
- Derivative generation uses short database transactions and a renewable
  five-minute claim, refreshed each minute. A slow external render does not
  intentionally hold a database row lock throughout the HTTP wait, nor does the
  original five-minute claim expire during normal renewal.
- Output downloads go into private files, verify size and SHA-256, and only then
  rename into the caller's output. Preview conversion has a further private
  publication step before CAFS staging. Normal transfer failure removes partial
  downloads and does not publish partial CAFS content.
- Callback registrations are removed on completion/failover or caller death.
  Late callbacks cannot directly write local output paths; their expired job ID
  is rejected. A remote attempt may nevertheless keep running after client
  failover: no cancellation request is sent here. That can duplicate external
  work, even though only the selected attempt's verified result is published.
- Parent death forcibly kills the CAFS file worker. Pins are released by monitors,
  but Erlang `after` blocks do not run on an untrappable kill. The private input
  hard link is registered with `z_tempfile` and is cleaned by its process monitor.
  Derived paths such as `.preview-*` directories, extension-appended preview
  outputs and `.download-*` files are not registered that way and can remain.
  Add monitored cleanup or a sweeper for these artifacts; CAFS reconciliation
  only scans the canonical CAFS tree and is not that policy.
- Pin and claim renewers are separate, unlinked processes. Database failure can
  terminate renewal while processing continues. Claim tokens prevent an obsolete
  owner from publishing over a replacement claim, but duplicate work becomes
  possible after expiry. Renewal failure should be observable and should cancel
  or explicitly fail the generation rather than silently lose its lease.

### Original investigation validation

The existing runner, CAFS, video and preview selection passed **41 tests**.
Three additional diagnostic probes checked video timeout handling, the unbounded
file-entry wait, and the ignored download deadline. These assert existing
behavior, including defects. Logs and probe source are retained locally in
`/tmp/cafs-audit/mediarunner-tests.log`, `mediarunner-probes.log`, and
`cafs_runner_audit_tests.erl`. No runtime source was changed.

This covers the Zotonic client side and mocked runner failures. It does not
verify the deployed external runner's input leases, output retention, clock skew,
shutdown handling or real slow-network behavior. Those need a separate service
integration test. The recommended immediate fix is R1, followed by explicit
serving/transfer deadlines and transient-error handling for R2–R4.
