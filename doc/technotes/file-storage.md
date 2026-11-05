File storage
============

Content-addressable file storage (CAFS), integrated into Zotonic core through
`m_cafs` for sites with a database. Existing files remain readable through the
legacy archive/filestore lookup and are promoted only when an operation already
needs their bytes. Database-free sites keep local library previews in the legacy
preview directory and do not use CAFS or preview garbage collection.

The server-side implementation covers uploads, duplication, shared previews,
filestore integration, retention, CAFS-backed video tasks, and browser-side
checksum preflight for uploads.


Sites without a database
------------------------

A site configured with `dbdatabase = none`, such as `zotonic_site_status`, must
still be able to generate and serve occasional previews of module `lib` files.
It has no uploaded media or medium records. CAFS tables, content hashes, pins,
derivative claims and preview-usage tracking are unnecessary for this case.

Required behavior:

 - Resolve the original through `z_module_indexer` and read that local library file.
 - Validate preview URLs/signatures and apply the normal rendering operations.
 - Store the result at `filename:join(z_path:media_preview(Context), PreviewPath)`:
   the existing site `files/preview/` directory, preserving the current pathname
   layout. `z_path` also supports older sites storing files under `priv/files/`.
 - Reuse an existing preview. On a miss, retain the converter's process-local
   generation lock, private output directory and atomic publication.
 - Keep the result local: no CAFS registration, remote filestore upload or GC.
   Files may remain indefinitely; manual removal is sufficient for these rare
   previews. Normal file-cache staleness checks still apply.
 - Do not add an upload fallback for these sites. Uploads require a database.

### Implementation

`m_cafs:is_enabled/1` checks the site's configured `dbdatabase` setting, accepting
`none`, `<<"none">>` and `"none"`. An omitted setting retains the usual
database-backed default. There is no new configuration option. Pool availability
is deliberately not the criterion: a database outage must not silently switch a
configured database-backed site to another storage model.

For database-free sites, `z_file_locate` checks the local library preview pathname
and, on a miss, passes the module indexer's absolute source filename directly to
`z_media_preview:convert/4`. It bypasses archive lookup and filestore notifications.
`z_media_identify` identifies the local file without consulting medium records.
Library URL generation, signature validation and filesystem/module-index cache
staleness checks remain in use. The existing `editrev=original` marker for library
images remains compatible and requires no database.

CAFS filename lookup returns `{error, enoent}` without querying a database; this
also protects the generic filename-to-filepath fallback. Staging returns
`{error, nodb}` before reading a file. CAFS installation and maintenance are no-ops,
and preview expiry returns `false`; preview maintenance is a no-op too.

The cleanup worker remains registered for compatibility with callers. Both its
scheduled and manual cleanup paths return without reconciliation, preview GC or
medium deletion queries. No database-free previews are garbage-collected.

Regression coverage in `z_cafs_nodb_tests` starts an isolated
`zotonic_site_status` supervisor using its actual configuration, with no database
pool. It verifies local generation, immediate reuse, reuse after the file-entry
stale-check interval, regeneration after deletion, invalid signatures, missing
library files and no-op maintenance. The test rejects SQL, medium identification
and filestore notifications on that site. Configuration tests cover all supported
`none` forms and ensure that a missing pool on a database-backed site does not
activate the fallback.


Previous storage model
----------------------

The upload pipeline in `m_media` identifies the file, checks permissions, runs
preprocessing and sanitization, and archives the result under a unique filename.
Upload-property observers can generate additional files, such as video posters.
The medium is stored in the database and `mod_filestore` optionally queues the
original and preview for remote upload.

Before CAFS, `m_media:replace_file_db/5` calculated a SHA-256 digest using
`z_crypto:hex_sha2_file/1` inside the database transaction. The digest alone did
not provide shared ownership or deduplication. Hashing now happens during staging,
before the medium transaction.

Previously, `m_media:duplicate/3` retrieved and copied the original and explicit
preview. Remote-only files had to be downloaded. This copying happens frequently in
environments like LearningStone, where courses and their videos are duplicated.

Other integration points:

 - `z_media_archive`: unique archive filenames and physical file operations.
 - `z_file_locate` and `z_file_request`: file resolution, preview generation,
   local/remote content access, and resource ACL information.
 - The `medium_update` database trigger and `z_media_cleanup_server`: queue
   retired filenames and remove their files after five weeks.
 - `m_filestore` and `filestore_uploader`: remote objects identified by path,
   upload queues, download claims, retries, and cache invalidation.
 - `mod_video` and `z_video_convert`: persistent conversion tasks, currently
   using source files in `files/video_queue/`.
 - `backup_create` and `backup_restore`: media backups currently use the archive
   directory and must also account for CAFS storage.


Solution
--------

Store immutable file contents by SHA-256, while keeping unique logical filenames
in `medium.filename` and `medium.preview_filename`. A filename maps to a content
hash; multiple filenames can refer to the same stored contents.

```text
medium A -> filename A --+
                        +-> CAFS hash -> one stored file
medium B -> filename B --+
```

This preserves existing URLs, filename reservations in `medium_log`, and
resource-specific permissions. Duplicating a CAFS-backed medium only creates new
filename mappings and a medium record; it does not read or copy file contents.

Start with site-local storage and tables. Cross-site deduplication is outside
this proposal. `mod_filestore` remains the optional remote storage backend.


Tables
------

`cafs` administrates the actual file contents:

 - hash: primary key, SHA-256
 - size
 - detected MIME type
 - extracted properties, including EXIF and text
 - metadata/extractor version
 - date created and metadata modified
 - lifecycle state
 - earliest garbage-collection time (`gc_after`)

Validate the hash length and use lowercase hexadecimal at API and storage-path
boundaries. The physical path is:

```text
files/archive/cafs/sha256/ab/cd/<full-sha256>
```

`filename_cafs` maps logical filenames to contents:

 - filename: primary key
 - hash: foreign key to `cafs`, indexed for reference checks
 - date created
 - optional retirement time

An original and its explicit preview/poster each have their own logical filename.
Generated variants do not require a mapping for every filename alias.

`cafs_derivative` shares preview generation across aliases of an original:

 - source hash: foreign key to `cafs`
 - normalized operations
 - renderer version
 - output hash: nullable foreign key to `cafs` until generation succeeds
 - state
 - claim token and claim expiry
 - unique key on `(source_hash, operations, renderer_version)`

Index derivative hash references for garbage collection. The renderer version
represents output-affecting changes in conversion code or configuration.

`cafs_pin` protects files used outside a committed filename mapping:

 - owner/task identifier
 - hash: foreign key to `cafs`
 - optional expiry
 - unique key on `(owner, hash)` and an index on hash

Pins protect staged uploads and background processing inputs. Durable tasks must
retain or renew their pins until completion or cancellation.

Keep original filenames, titles, permissions, and processing status on the
medium/resource. Only byte-derived metadata belongs in CAFS. Initially retain
existing medium metadata too, for compatibility with consumers.


Model boundary
--------------

`m_cafs` is part of `zotonic_core`. Periodic maintenance runs through the existing
media cleanup worker; remote uploads run through the filestore queue. `m_media` remains the public entry point for
uploading, replacing, and duplicating media.

Internal functions:

```erlang
m_cafs:stage_file(File, Context).
m_cafs:attach(Filename, StagedFile, Context).
m_cafs:clone_filename(Source, Destination, Context).
m_cafs:lookup(Filename, Context).
m_cafs:with_local_file(Hash, Fun, Context).
m_cafs:retire_filename(Filename, Context).
m_cafs:pin(Hash, Owner, Context).
m_cafs:unpin(Hash, Owner, Context).
```

`stage_file/2` hashes and publishes bytes and returns a protected staging
reference. `attach/3` creates the filename mapping and consumes that reference
within the medium transaction. `clone_filename/3` creates another mapping without
accessing the contents. `with_local_file/3` keeps the local/cache file available
for the callback's lifetime.

Public model callbacks must address authorized resources. Do not expose arbitrary
hash lookup, attachment, or deletion as a browser-accessible storage API.


Upload processing
-----------------

 1. Identify the upload and perform existing resource/category and upload checks.
 2. Run antivirus handling, preprocessing, sanitization, and property extraction.
 3. Produce explicit preview files in staging. Preserve existing ACL checks for
    the resulting resource and file type.
 4. Calculate SHA-256 over the final bytes of each file, after all byte-changing
    processing. Hashing and filesystem I/O happen outside the database transaction.
 5. Publish into CAFS, or reuse an existing ready object with the same hash.
    Protect the object with a staging reference.
 6. Generate unique logical filenames.
 7. In one database transaction, attach the filename mappings, update the medium,
    and record durable remote-upload work for new objects.
 8. Release staging protection after attachment. Reconcile abandoned staging
    work after failures.

Filesystem publication and database commit cannot be atomic together. Write to a
temporary file on the destination filesystem, publish atomically, and coordinate
concurrent uploads using the unique hash and lifecycle state. Never expose partial
contents at the final hash path. A failed transaction must leave recoverable or
collectible staged contents, not a dangling live mapping.

Existing upload observers expect `archive_file` to identify a physical archive
file. Audio and video observers call `z_media_archive:abspath/2` directly. Keep the
archive staging files available through property observers and post-insert
notifications, then remove newly staged aliases after successful publication.
Consumers of stored contents use file requests or `with_local_file/3`; a logical
filename is no longer necessarily a physical file beneath the archive directory.


Existing files and lazy promotion
--------------------------------

Do not backfill hashes or download all files from remote storage. Such downloads
are prohibitive. Support legacy archive/filestore files and CAFS files together
indefinitely. New uploads use CAFS; existing files are promoted on demand.

When copying a medium with `m_media:copy/3` or duplicating a resource, associated
originals and explicit previews get owned CAFS filenames. Non-owned archive and
library references are snapshotted without promoting or changing the source path;
the destination records the original's SHA-256 digest. Generated resize variants
continue to be shared through the derivative registry.

When duplicating owned files:

 1. If the source has a CAFS mapping, create a new logical filename referring to
    the same hash.
 2. If a legacy source is available locally, hash and promote it, preserving its
    existing logical filename. Attach both source and destination to the object.
 3. If a legacy source is remote-only, the existing duplication operation already
    needs to download it. Hash and promote those downloaded bytes so subsequent
    duplications do not download or copy them again.

Apply this to originals and explicit previews. Keep the legacy file and remote
entry until CAFS publication and reference updates are safe, including successful
remote storage where required. Only then retire the redundant legacy copy.
Promotion and duplication must coordinate with deletion and concurrent promotion.

Other operations that already obtain the bytes can also promote a file. Listing
or merely serving existing files does not require promotion. Legacy lookup is a
fallback only when there is no CAFS mapping; a failed CAFS read should not silently
serve an obsolete legacy copy. Static, non-owned module files keep their existing
behavior.


Previews
--------

The same original contents and the same effective preview operations produce the
same output. Share both the stored output and the generation work, independently
of logical filenames.

 1. Validate the preview URL/signature and retain the requested resource's ACL.
 2. Resolve the original filename to its source hash.
 3. Normalize effective operations, including resolved mediaclass settings and
    all other output-affecting inputs. Include the effective identification metadata
    (MIME type, dimensions, orientation and frame count), and pass that exact
    snapshot to the renderer. Resource IDs are excluded so equivalent copies share.
 4. Look up `(source_hash, operations, renderer_version)` in `cafs_derivative`.
 5. If ready, serve the output hash using the requesting resource's permissions.
 6. Otherwise atomically claim generation. Concurrent requests wait for the same
    result rather than starting another conversion.
 7. Pin the input, generate the preview, store the output in CAFS, and publish the
    derivative mapping conditionally on the claim token still being current.

Claims expire after worker failure and are renewed during long conversions. Tokens
prevent stale workers from publishing after another worker has taken over. Protect
outputs until their derivative mappings commit, then release processing pins.

Existing legacy previews can still be served as-is. When a missing preview requires
fetching a legacy original, hash and promote those already-fetched bytes before
checking the shared derivative table.

Explicit previews such as video posters remain ordinary filename mappings.
Generated variants use the derivative table. Their URL usage records track
retention independently of shared output bytes.

Preview retirement
------------------

`z_cafs_preview` maintains `cafs_preview_usage`: one record per requested preview
URL, including its source filename/resource, mediaclass name and checksum, edit
revision, and derivative identity. Identical operations still share one derivative;
provenance does not enter the rendering key.

New preview URLs include a signed `editrev` parameter. The
`media_preview_revision` fold notification collects stable, normalized settings;
`mod_image_edit` contributes its settings, including legacy crop-center settings.
The `original` option uses an explicit bypass marker. The converter strips this
metadata before rendering and derivative lookup. URL signatures validate the
extension in the URL, so changing a mediaclass's output format does not invalidate
previously signed URLs prematurely.

Every CAFS maintenance interval checks up to 100 usage records, oldest checked
first, against the current medium, mediaclass checksum, and edit revision. A usage
becomes obsolete when its source is removed/replaced, its mediaclass changes or
disappears, or its edits change. Reverting the settings clears obsolescence.
Checks use database and module metadata; they never download source files.

Obsolete usages have a fixed grace period, configured with
`site.cafs_preview_retention_days` (default 90 days). Requests do not extend it. During
the grace period an already-bound derivative can still be served even if the old
mediaclass definition has disappeared. After expiry the URL returns missing,
including when the physical output remains shared by another resource. File-entry
cache checks and maintenance invalidation enforce this on cached entries too.
Small usage tombstones are retained to prevent replay from restarting the grace
period; this policy collects preview bytes, not historical URL metadata.

Maintenance also checks up to 100 derivatives. Old derivatives without any current
or retained usage are removed after checking generation claims and source/output
pins under the source object's lock. Their output hashes enter normal CAFS GC,
which preserves any other derivative, filename, or pin referencing those bytes.
When all source aliases lose their recovery references, normal source GC can remove
its derivatives sooner, after the existing source retention period.

Upgrades timestamp existing derivatives without expiring unchanged legacy URLs.
For a URL without edit provenance, record the edit revision when first observed;
subsequent changes can retire it. Changes predating that observation cannot be
proven from an unversioned URL, so retain it conservatively while its source and
mediaclass remain current. Existing usage rows with no revision gain this baseline
lazily. The old site-wide upgrade timestamp is no longer an expiry policy.
No upgrade hashes or downloads old files.

Expired usage records also queue deletion of their exact legacy `preview/` path,
locally and in filestore. Failures are retried on later checks; URL tombstones
remain. Other legacy previews are removed by source-filename cleanup.


Serving and remote storage
--------------------------

Resolve logical filenames through CAFS before locating bytes in `z_file_locate`.
Keep resource ACL information on returned file parts and enforce it through the
existing request path. Sharing contents must not share permissions, including for
previews. Do not add a public download route authorized by hash alone.

Use physical content paths such as `archive/cafs/sha256/ab/cd/<hash>` as `m_filestore` keys.
Queue uploads per ready CAFS object, not per medium alias. Reuse filestore retries,
download claims, and caching. Keep local contents until remote upload succeeds.

Update bulk upload/move-to-local enumeration and cache invalidation for CAFS.
`filestore_uploader` currently translates `archive/` and `preview/` paths when
invalidating file entries; CAFS invalidation must also reach affected aliases.

CAFS contents are inside `files/archive/cafs`, so existing archive tar backups
and restores include them alongside legacy files. The database backup includes
the mappings. Remote-only objects retain the existing filestore backup semantics;
an archive tar alone does not include remote-only bytes. Retention must allow
restoration of retained database backups.


Deletion and garbage collection
-------------------------------

On medium deletion or replacement, retire old filename mappings instead of deleting
shared contents. Adapt the `medium_update` trigger and cleanup worker so legacy
deletion and CAFS reference retirement take their respective paths.

Preserve retired mappings during the existing recovery period. Afterwards remove
eligible mappings and schedule their hashes for garbage-collection checks. A flag
or timestamp schedules a check; it is not proof that an object is unreferenced.

An object cannot be collected while protected by:

 - An active or retained filename mapping.
 - An upload or processing pin.
 - A retained derivative relationship or other explicit retention reference.

Derivatives are dependent cache entries, not permanent roots keeping every source
alive. Retain their outputs while retained sources need them, or explicitly evict
regenerable derivatives under a cache policy. When a source loses all retention
roots, remove eligible derivative relationships and check their outputs too.

Attachment, derivative publication, promotion, and GC must use the same database
locking/state protocol. Staging identical bytes can cancel a deletion that has
not been dispatched. The filestore dispatcher fences the row with `delete_started`
before sending a delete; cancellation locks that same row. Once dispatched,
staging returns `{error, deleting}` until remote absence is confirmed. Media
insertion propagates this retryable error without replacing the destination.
Complete outstanding asynchronous deletion before reusing the same physical object
key; an old remote delete must never remove newly referenced contents. On upgrade,
already-deleted filestore rows are conservatively fenced because older code might
have dispatched them.

Keep retryable deletion state until physical removal is confirmed. Honor configured
remote retention, including "Never", and account for it before releasing deletion
state. A pending CAFS tombstone is rescheduled one hour later so it cannot starve
other garbage collection. Filestore deletion failures retain their registry rows,
record errors and defer retry by one hour; forbidden responses and service URL
mismatches are not proof of absence. Repeated deletion requests do not restart the
remote retention interval. Do not apply legacy filename-prefix deletion to shared
content objects.

Legacy cleanup checks both original and preview references before unlinking files.
The reference check and local cleanup use a short, nonblocking table lock against
medium writers; a busy table defers the cleanup instead of waiting. Failed or
still-referenced work remains in `medium_deleted`, with `retry_after` advanced one
hour. Remote prefix matching is literal, including underscores and percent signs.
The medium update trigger queues cleanup on transitions to SQL NULL too.


Video render queue
------------------

Reuse the existing `mod_video` supervisor and pivot task queue. Replace the local
queue filename with a pinned CAFS source hash so input can also reside remotely.

 - Store and pin the uploaded original, then schedule the persistent render task.
 - Start a worker if none is running; let the task retry after a delay while work
   remains. Recover abandoned work after worker/node failure.
 - Obtain a local input through `with_local_file/3` and run ffmpeg on temporary
   outputs.
 - Store the converted video and poster through CAFS and update the medium through
   `m_media`.
 - Preserve the current-upload check, making final replacement conditional in the
   transaction so a late conversion cannot overwrite a newer upload.
 - Remove the task and release input pins on completion or cancellation.

The source used for processing need not be exposed as the resource's downloadable
medium while conversion is pending.


Upload preflight
----------------

The chunked uploader and wired multipart forms calculate SHA-256 in a worker,
reading at most 1 MiB per chunk. The browser checks before transmitting bytes.
`m_media` exposes `/upload_check`; `m_fileuploader` accepts the same optional
`digest` in `/new` and only starts an upload process on a miss.

Reuse requires a current medium original with the CAFS hash and a visible source
resource. Visible legacy originals with a matching checksum are lazily promoted
and rechecked. Unknown, inaccessible and unavailable content returns the same
miss; originals without a known checksum upload normally. Signed reuse tokens are user-bound and expire after one hour. Source
visibility and mapping are rechecked at submission; the normal upload pipeline
still enforces destination ACLs, scans and processes bytes. Remote-only objects
may need a server-side download for that processing. Client hashes never grant
access by themselves. A raw upload changed by sanitization or conversion will
not match the resulting stored bytes and uploads normally.

Browsers without worker support, failed checks and unmatched files use the normal
upload path. Mixed multipart selections omit only the matched files when the
browser supports constructing FileLists; otherwise that selection uploads normally.


Verification and operational notes
----------------------------------

The core installer and upgrade path use the same idempotent schema installation.
No upgrade enumerates or downloads existing media. Processing pins are renewed
while workers run; persistent video tasks retain durable pins until completion or
cancellation. Local orphan reconciliation visits one hash-prefix directory per
maintenance interval and registers old completed publications for normal GC;
it does not inspect or fetch legacy remote files. Dispatched remote deletion
tombstones remain until the filestore entry has been removed, so delayed deletes
cannot remove newly attached contents. Undispatched deletes can be cancelled by
an identical reupload through the shared deletion fence.

Use the `site.cafs_renderer_version` configuration value to invalidate generated
variants after output-affecting changes not already represented in the renderer
identity. Call `z_file_sup:refresh/0` after changing this setting to invalidate
already-running file entries too. Operations retain their pipeline order and include the output extension,
expanded mediaclass filters, effective rendering metadata, ImageMagick selection,
and default colorspace. Internal archive/root/filter lookups have separate file
cache identities from public requests; an internal lookup cannot remove a public
request's resource ACL.

Legacy copies retained during promotion are left for the existing filename cleanup
path; promotion itself does not remove a remote legacy object. Upload preflight is exposed by the media and fileuploader models.

Verify fresh installs and upgrades, concurrent identical uploads, duplication
without file reads for mapped sources, lazy promotion of legacy sources, and
continued access to untouched remote-only legacy files. Cover shared preview
generation across aliases, claim recovery, and renderer-version changes.

Also verify deletion of one of several references, attachment racing GC, failures
between filesystem publication and database commit, remote deletion retries,
private/public aliases sharing bytes, conversion after medium replacement, and
backup restoration with both legacy and CAFS files.

### External renderer failures and request waiting

See [Mediarunner reliability](mediarunner-reliability.md) for deadline, retry and
result-retention details. HTTP preview lookups wait up to five seconds independently
of shared generation; pending or failed work returns 503 with `Retry-After: 2` and
`Cache-Control: no-store`. A later request can use the completed preview. Failed
renders never write a placeholder under a successful preview identity.

Video conversion retains CAFS pins or legacy queue files on temporary failures and
failed medium commits. Retry metadata persists in the task; automatic retries pause
after 24 hours of recorded failures, while hourly reconciliation releases inputs
for deleted/replaced media. Administrative retry uses `mod_video:retry_task/2`.
Private temporary preview/download artifacts are monitored and old orphan artifacts
are swept in bounded batches. This never collects ordinary no-database previews.

The existing global `media_runner_wait_timeout` is a total operation budget in
seconds, including transfers and failover. Its default is the command timeout plus
300 seconds. It is not a per-site CAFS setting; no additional site configuration
keys are needed for this reliability behavior.
