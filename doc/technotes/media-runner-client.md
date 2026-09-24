# Media runner client on Zotonic 0.x

This is a client-only backport for Erlang 22.3, compatible with the existing
media runner's version 3 protocol. It includes submission, cached input uploads,
authenticated callbacks, polling recovery, verified output downloads, and image
and video integration. It does not host runner jobs or add a local sandbox.

Client updates through master commit `b0b9e6909` (#4543) are included: overload
backoff, deadline-bounded submission/reservation requests, and short temporary
download names. The earlier HTTP/HTTPS selection update (#4539) is also included.

## Execution contract

| Configuration | Behaviour |
| --- | --- |
| No runner, or an explicit empty pool | Existing local media processing |
| Non-empty valid runner configuration | Remote ImageMagick and FFmpeg processing only |
| Invalid configuration or remote failure | Return an error; never retry locally |

`media_runner_local_fallback` is not supported; even setting it to `true` cannot
enable fallback. Local MIME sniffing with `file` and Erlang EXIF handling remain
identification steps, as on master. Custom site observers and code calling
`os:cmd` directly are outside this routing boundary.

The legacy string-returning `z_exec:run/1,2` API is unchanged. Media calls use
`z_exec:run(Profile, Command, Options, Context)`, returning `{ok, Binary}` or
`{error, Reason}`. Supported profiles are `imagemagick`, `imagemagick_pdf`,
`ffmpeg`, `ffmpeg_preview`, `ffprobe` and `file`.

Declare all transferred files explicitly with `read` and `write` lists of
absolute paths. Only declared output IDs may become local files. Commands retain
shell syntax, so callers must escape filenames. The public context-free
identification and video functions remain usable locally; remote processing
requires their context-aware variants. A missing context returns an error rather
than permitting local execution.

## Configuration

Add node-wide entries to the `zotonic` application configuration:

```erlang
{media_runner_hostname, <<"media.example.com">>},
{media_runner_protocol, <<"https">>},
{media_runner_oauth2_key, <<"YOUR-OAUTH2-BEARER-TOKEN">>}
```

Hostnames may include a port. HTTPS is the default; HTTP is also accepted.
Values may be binaries or strings. A hostname must not contain a URL scheme,
credentials, path, query or fragment.

An explicit pool overrides all single-runner settings:

```erlang
{media_runners, [
    #{hostname => <<"media-a.example.com">>, oauth2_key => <<"TOKEN-A">>},
    #{hostname => <<"media-b.example.com:8080">>, protocol => <<"http">>,
      oauth2_key => <<"TOKEN-B">>}
]}
```

Pool entry keys may be atoms or binaries. A pool has at most 32 entries. Missing
tokens, duplicate endpoint/credential pairs or invalid entries reject the whole
pool. `{media_runners, []}` explicitly selects local mode, overriding a configured
single hostname. Without a pool, an unset or empty hostname selects local mode.

Optional limits, with their defaults:

```erlang
{media_runner_wait_timeout, 43500},             % seconds: 12 hours + 5 minutes
{media_runner_max_input_bytes, 17179869184},    % per input: 16 GiB
{media_runner_max_output_bytes, 17179869184},   % outputs: also capped by profile
{media_runner_max_callback_bytes, 135266304}   % encoded JSON: may be reduced
```

Profile timeouts and output limits are defined in `z_media_limits.hrl` and
`z_exec:profile/1`. Transfers have separate one-hour timeouts. Existing local
preview/video queue limits still apply. No CA dependency or native helper is
added; the branch's existing Rebar 2 dependency lock is retained.

## Transport and runner selection

Runner HTTPS connections use `{verify, verify_none}` in every environment:
certificate and hostname verification are disabled. Bearer authentication remains
required. Redirects are disabled, and downloads are pinned to the configured
runner's result endpoint; a callback cannot redirect credentials elsewhere.

Input hashes are calculated incrementally. Submission initially sends hashes,
and uploads only inputs the runner reports missing. Upload reservations avoid
concurrent duplicate transfers. Results stream into private temporary files;
size and SHA-256 must match before publication. All outputs are verified before
any destination is replaced; each rename is atomic, not a multi-file transaction.
Successful receipt is acknowledged to the runner.

Successful control responses and file downloads are streamed with limits. OTP 22
`httpc` buffers non-streamed HTTP error bodies before the client can size-check
them; the accepted response limit is not a hard memory cap on those error bodies.

Pool selection uses deterministic input-hash affinity. Availability failures may
try another remote runner; authentication, invalid results and confirmed command
failures do not. The legacy client does not maintain master's cross-job load and
file-location hints; the runner remains authoritative about cached inputs.

HTTP 429 and `full` responses to submission or upload reservation retry the same
request with randomized backoff for up to one minute, bounded by the remaining
job deadline. Busy upload reservations use a short randomized wait before
reserving again. Submission, reservation and submission-recovery HTTP timeouts
are capped by the remaining deadline; file transfers retain their separate
one-hour timeout. Persistent overload can still cause remote failover.

ImageMagick capabilities are cached and probed concurrently with a bounded wait.
The selected major version and executable determine command generation and
eligible runners, so v6 and v7 commands are not mixed. A configured remote client
never probes local ImageMagick as a fallback.

## Media integration

The built-in paths cover image identification, resizing, PDF/PostScript previews,
video metadata, video stills, transcoding and rotation metadata reset. Existing
media property lists and template interfaces are retained. New context-aware
entry points are `z_media_identify:identify_file_direct/3`,
`mod_video:video_info/2` and `mod_video:video_preview/3`.

Previews are generated into a temporary file next to their destination, checked
for non-empty output, then renamed. Failure preserves an existing preview.
Conversions to the same destination are serialized, and waiting callers do not
mistake an old preview for a successful conversion.
Preview and download temporary names use short random basenames in the same
directory, so long destination filenames do not overflow the filename limit.

Video infrastructure/configuration failures preserve the queued source and its
existing retry task. Input file access/size failures also retain the source, so
fixing storage access or input limits allows a later retry. Local media execution
reports timeout and output-limit failures explicitly; partial outputs are not
published and video sources remain queued. The legacy string-returning execution
API is unchanged. A confirmed processing failure follows the existing
broken-video path. Intermediate rotation files and failed outputs are removed.

Custom FFmpeg command settings and ImageMagick `magick` filters must work on the
runner. Extra fonts, delegates, binaries or files referenced by those commands
must exist there, or be explicitly included as inputs by custom calling code.
Client filesystem paths embedded in arbitrary custom command arguments are not
automatically discovered and uploaded.

## Registry and callbacks

`z_media_runner` starts before the sites supervisor. `register/0` returns
`{ok, Id, Secret}` for the calling process. Both values use cryptographically
strong randomness; the registry retains only a SHA-256 digest of the secret.
The submission code unregisters each attempt in an `after` clause. Process
monitors and expiry timers provide cleanup if the caller exits or times out.
There are at most 1,000 pending registrations, and status reports redact state.

Each site's `mod_base` exposes `POST /media-runner/callback?id=...`, authenticated
with `Authorization: Bearer <per-job-secret>`. The controller authenticates before
reading the body and does not use browser sessions or form parsing. The body must
be a bounded JSON object, including when sent with chunked transfer encoding.
Duplicates are acknowledged but only one result is delivered to the waiting
process. Credentials are checked again after reading the body.

Responses are non-cacheable: 204 acknowledges delivery; 400 rejects malformed
JSON; 401 rejects malformed/missing credentials; 410 rejects unknown/expired jobs
or wrong secrets; 413 rejects oversized bodies; 415 rejects non-JSON content;
503 indicates registry/configuration unavailability. A 204 does not assert that
result files passed verification.

Callback URLs come from a fresh site context and the canonical dispatcher,
never the incoming request's Host header. The route requires HTTPS. Configure
`mod_ssl` and the externally reachable site hostname/port accordingly, including
when using a reverse proxy. The runner must reach this endpoint. In a cluster,
callbacks must reach the submitting node: registrations are in memory and are
lost on restart. Polling every 15 seconds recovers lost callbacks and detects
unavailable runners while the submitting process remains alive.

## Logging

The client uses Lager. Debug logs trace submission, missing input uploads,
completion time, and recovery through status polling, with job IDs and runner
endpoints for correlation. Warnings report failed jobs, remote failover, callback
configuration errors, and failed receipt acknowledgements. Receipt acknowledgement
failure does not invalidate a successfully downloaded result.

Error logs contain categories rather than raw error responses. Tokens, callback
secrets, command text, file paths, and media payloads are not logged.

## Running the focused tests

Use an existing OTP 22.3 build of the checkout and its dependencies:

```sh
ZOTONIC_TEST_ERL=/Users/marc/erlang/22.3/bin/erl test/media-runner.sh
```

Substitute the Erlang path as needed. The script checks for OTP 22.3, compiles
changed modules into a temporary directory with warnings treated as errors, and
runs EUnit. It does not rebuild dependencies, replace existing application beams,
start Zotonic sites, or connect to a database. It needs localhost TCP access and
`openssl` to create an ephemeral self-signed test certificate. No ImageMagick,
FFmpeg, external runner or external network service is needed; the sentinel test
uses the local `file` utility for MIME sniffing.

Coverage includes configuration states; registry lifecycle and authentication;
Webmachine JSON negotiation and fixed-length/chunked body limits; multi-megabyte
uploads and cache reuse; callbacks and polling recovery; remote failover; all
media profiles; verified downloads and rejected hashes/paths; redirects and
transfer timeouts; self-signed HTTPS in development and production; Unicode,
quotes and overlapping filenames; local API compatibility; atomic preview
publication and concurrent failures. Local tool stubs prove that configured
runner failures do not execute local ImageMagick or FFmpeg. Existing preview
command-generation tests run alongside these tests.

The fake runner implements the wire protocol and returns synthetic outputs. It
does not establish that actual media renders correctly or that a deployed site's
proxy, dispatcher and TLS listener are configured correctly.

## Staging acceptance before merging/deployment

On an isolated 0.x site running Erlang 22.3, with a real protocol-v3 runner:

1. Confirm that `z_media_runner:callback_url(Context)` returns the canonical,
   externally reachable HTTPS endpoint, including from a background job.
2. Upload JPEG, PNG, animated GIF and PDF/PostScript fixtures. Check dimensions,
   orientation, thumbnails, cropping and background removal against local mode.
3. Upload a rotated video. Check metadata, poster image, rotation reset and MP4
   output; exercise existing custom command settings if used by maintained sites.
4. Stop the runner or invalidate its token. Confirm image requests fail without
   local tools, queued video sources remain, and restoring service permits retry.
5. Exercise a large file and simultaneous previews. Check transfer timeouts,
   temporary-file cleanup and the configured concurrency limits.
6. If using a pool, test v6/v7 ImageMagick compatibility and runner failover. If
   using multiple Zotonic nodes, verify callback routing to the submitting node.
7. Disable remote mode with an empty pool and confirm the site's existing local
   workflow. Remove that override to re-enable remote processing.

These deployment checks require a configured site and real runner and are not
claimed by the automated suite.
