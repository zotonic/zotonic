# CAFS audit

Reviewed: **2026-11-10**, against Zotonic commit
`ee638331d`. Scope: CAFS storage and media integration, legacy files, previews,
cleanup, filestore, video, backup, and the Zotonic side of the external runner.

This report describes the current implementation. Earlier defect descriptions
and validation results are available in git history. This review checked source
and regression-test coverage; it did not rerun EUnit, Dialyzer, a live filestore
migration, or an external runner fault test.

## Current assessment

No immediate fix is identified by this source review. The follow-ups below are
separate enhancements and operational limits; this review is not a deployment
certification or an integrity scrub.

The module currently behaves as follows:

- Fresh upload originals use one SHA-256 pass over their private staging copy;
  `m_media:media_digest/3` reuses that hash. `stage_medium/3` only selects this path
  for the newly archived original identified by the upload routine. Legacy files,
  extracted previews and observer-supplied replacement filenames retain the
  source/copy comparison. An existing mapping is reused without hashing again.
- Maintenance can adopt a remote original with a valid stored checksum and size
  through the filestore registry, without downloading it. This is metadata-based
  adoption, not verification of remote bytes. Originals without a usable checksum
  and remote extracted previews wait until bytes become available.
- Existing valid previews can be promoted rather than regenerated. Source and
  preview promotion check ownership; mutable non-owned sources remain on the
  legacy path. Database-free sites continue using the local preview directory.
- Browser upload preflight reuses a matching current original only when its
  resource is visible. Tokens are user-bound, expire, and recheck access and the
  source mapping at submission. Materialization verifies the copied bytes and
  still runs normal media processing. This does not eliminate all server hashing.
- HTTP lookup now waits **60 seconds**, following commit `7440a0d32`, rather than
  the original five-second remediation. Timeout remains bounded; shared work can
  continue, and failure returns a retryable response. Browsers do not themselves
  retry failed image requests merely because a `Retry-After` header is present.

## Current file and checksum coverage

| File state | Current behavior and limits |
| --- | --- |
| New owned upload | Hash private staging copy once and use that hash for CAFS and the medium digest. Publication still uses an atomic rename and a staging pin. |
| Legacy local file without a checksum | Hash when copying, migrating or promoting available bytes; no installation-wide hash scan. |
| Legacy local file with correct or stale checksum | Legacy staging hashes actual source and copy. Migration's `attach_stage/7` updates the original digest under the medium row lock. The copy-specific `m_cafs:promote/3` path does not itself rewrite the source medium digest; the destination uses the actual hash. |
| Remote-only original without a checksum | Maintenance defers it. An operation requiring bytes can fetch it and then promote it; no bulk remote download is required. |
| Remote-only original with a valid checksum and size | Maintenance can adopt an existing remote registry entry atomically through `cafs_adopt`. Busy or deleting entries defer adoption. No download verifies the stored checksum during this operation. The old physical FTP/S3 location can remain the canonical object's remote location. |
| Remote-only extracted preview (`preview_filename`) | Does not borrow the original's digest. It waits for local bytes/download before promotion unless already mapped. |
| Already mapped CAFS file | Copy clones its reference. New mappings and active readers/pins protect bytes; deletion remains lazy. |
| Non-owned source | Copy snapshots bytes into an owned alias. Preview generation/promotion does not attach a CAFS mapping over the mutable source. |
| Database-free site | Library previews use the legacy preview directory without CAFS database access or CAFS preview GC. |
| Missing local and remote bytes | A stored digest alone is insufficient to establish availability. Remote adoption requires a usable registry/backend result; operations needing bytes can still fail. |

`m_media:copy/3` prepares the associated preview before replacing the destination;
`duplicate/3` remains best-effort for a missing preview. Recovery preflight reports
missing original/preview availability; it cannot make expired or absent bytes
recoverable. The five-week medium retention and filestore deletion delay remain
relevant to undelete and revision recovery.

## Remaining limitations and follow-ups

1. **Existing-object integrity is not verified on every access.**
   `m_cafs:stage_hashed/4` hashes the staging copy but trusts an already existing
   canonical local file if `filelib:is_regular/1` succeeds. Remote adoption trusts
   stored hash/size metadata and the remote registry. Neither is an integrity
   scrub, and a stale remote checksum is not repaired merely by adopting it.
   Runner result downloads and upload-token materialization have their own hash
   checks; these do not establish integrity for every serving/copy path. A bounded,
   on-demand verification/repair operation remains a useful separate enhancement.
2. **Remote cancellation remains deferred.** Client failover or timeout can leave
   remote execution running until its submitted expiry. Callback fencing prevents
   late results from directly overwriting local output, but duplicate remote work
   is still possible. External deployment/version compatibility and real outage
   behavior were not revalidated in this revisit.
3. **Compatibility and retention have deliberate limits.** Unversioned URLs cannot
   prove edits predating their first observed baseline. URL tombstones grow with
   distinct requested variants. Pins, recovery retention and remote deletion
   policies can postpone physical reclamation; a dispatched delete cannot safely
   be cancelled based only on a timeout.

## Ownership and worker lifetime

Non-owned preview sources are guarded by `z_media_cafs_migrate:is_owned/2`.
Private preview/download artifacts have `z_media_temp` guards and a bounded
orphan sweep. Pin and derivative claim renewers are linked to their owners, so
failed renewal does not silently leave ordinary processing running indefinitely.
Existing tests exercise ownership, worker-death cleanup and orphan sweeping.
Broader crash/partition fault injection remains outside this source review.

## Validation of this revisit

Reviewed storage, preview and runner behavior against the implementation and
relevant regression assertions, including staging, remote adoption, ownership,
upload preflight and recovery. Confirmed the 60-second HTTP wait against the code
and its later cold-preview regression. Checked the documentation diff for whitespace
errors. Only documentation was changed; no site data, files, queues, remote objects
or running nodes were modified.

No new runtime pass or Dialyzer result is claimed here. For a fresh release gate,
run the CAFS, preview, migration, copy/upload, reliability, video, backup and
filestore suites, then Dialyzer, and separately verify the deployed runner and a
real remote store under interrupted-transfer and delayed-callback conditions.
