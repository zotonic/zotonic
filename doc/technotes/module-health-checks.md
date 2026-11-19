# Generic module health checks

**Status: proposal for a separate pull request. Not implemented.**

Introduce a shared health-check service for Zotonic modules, with isolated
execution, structured results, and a consistent admin status panel. The current
admin status loader fixes and presentation changes do not implement this service.

## Current behavior

Health checks are model calls assembled through the modules’
`_admin_status_alert.tpl` templates. Filestore and ClamAV checks are requested
asynchronously when the status page loads; system checks run during page
rendering. There is no shared execution, timeout, caching, or result contract.

| Check | Current implementation |
| --- | --- |
| Filestore | `m_filestore:health/1` reads database statistics. It warns when there are at least five failed remote reads and at least 20% failures in the current and preceding fourteen minutes, or unresolved recoverable read errors older than 24 hours. It does not probe FTP/S3 connectivity. |
| ClamAV | `m_clamav` calls `z_clamav:ping/0`, sending `PING` and expecting `PONG`. |
| Disk space | `m_admin_status` compares `disksup` measurements with its configured usage threshold, excluding hidden system disks. |
| SSL configuration | `m_admin_status` checks whether `ssl.session_lifetime` is configured. This is not a certificate or TLS connectivity check. |
| OS memory | The alert check currently returns `false` unconditionally, pending an OTP-related fix. It must be represented as disabled rather than healthy in the new service. |

“No problems found” currently refers only to these limited checks. A lack of
recorded failures does not establish that an external service is reachable.

## Module registration

Add a notification through which active modules contribute check definitions.
Discovery should be inexpensive and must not itself execute network probes.
Each definition supplies:

- A stable module-qualified ID and translated title.
- Scope: `site` or `node`.
- An execution callback.
- A bounded timeout and cache lifetime.
- The permissions needed to view its result.

A module can register several checks. Refresh registrations when modules are
activated or deactivated, and discard results for removed checks. Node-wide
checks should be shared across sites only when they refer to the same service
and configuration; site-specific endpoints remain site-scoped.

The notification name and exact Erlang record/map contract should be finalized
in the implementation PR. Document the notification in both its header and
`zotonic_observer.erl`, following the existing notification conventions.

## Results

Use a common result structure with a stable check ID, status, translated
explanation, scope, completion timestamp, and expiry. An optional administration
link can direct maintainers to configuration or troubleshooting information.

| Status | Meaning |
| --- | --- |
| `ok` | The check completed successfully and found no problem within its stated scope. |
| `warning` | The check found a condition requiring attention. |
| `error` | The check established that the checked service or condition is unhealthy. |
| `unknown` | No reliable result is available, including callback crashes, timeouts, and expired results. |
| `disabled` | The check is deliberately not performed; explain why. |

Keep execution state, such as queued or running, separate from health status.
While refreshing a result, retain the previous result and timestamp, clearly
marking it stale after expiry. Never translate an exception or timeout into `ok`.

Results must not contain credentials, raw exception dumps, or sensitive
connection details. Log diagnostic details server-side and return a concise
explanation to authorized administrators. Translate and escape results when
rendering them; cached results should not depend on the requesting user’s language.

## Execution and caching

A supervised coordinator runs callbacks asynchronously in isolated workers,
with bounded concurrency and individual timeouts. Concurrent requests for the
same check share one execution. A slow or failing module must not prevent other
checks from completing or delay the initial page render.

Cache results independently of page views. Support an authorized refresh action,
subject to deduplication and rate limits. Cancel timed-out workers and clean up
their bookkeeping. After a coordinator restart, missing results are unknown and
can be checked again; persistence is not required for the initial implementation.

The coordinator and result cache must work without a site database. A check that
requires a database should be absent or explicitly disabled on a database-free
site. A database outage on a database-backed site is a failure condition, not a
reason to silently disable the check.

Checks should be read-only by default. Do not introduce test uploads, remote
deletes, automatic repairs, or other mutations as part of this migration.

## Model and live updates

Expose definitions, cached results, and refresh requests through an
ACL-protected `m_health` model. Apply the same authorization to model access and
MQTT subscriptions; background execution must not expose privileged results to
less privileged viewers.

Publish result changes under a generic system-status topic, provisionally
`model/system/event/status/health`, allowing other status topics alongside it.
Use retained snapshots for newly opened pages, with bounded retention aligned
with the existing system-status convention (currently two hours). Retention is
not a freshness guarantee: each check’s completion time and expiry remain
authoritative, and the UI must show expired results as stale/unknown even if a
retained message is still available.

The model provides an initial snapshot and recovery path if a client misses an
update. Topic scoping must prevent results leaking between sites.

## Admin presentation

Keep a single Health checks panel near the top of `/admin/status`. Render stable,
labelled rows before starting checks, reserving space for an indicator and result
text so the loader does not move surrounding content. Start checks on page load,
without requiring the panel to scroll into view.

Use the existing admin success checkmark (`glyphicon-ok-sign` and
`text-success`) and consistent warning/error indicators. Keep explanatory text
so neither color nor icons are the only indication of status.

Show an overall “No problems found” only when all applicable enabled checks have
completed successfully and their results are fresh. Show running, unknown,
disabled, and stale checks explicitly; do not hide them behind a green summary.
Distinguish passive observations, such as filestore read statistics, from active
connectivity probes in the descriptions.

Modules should return data rather than supply their own loading templates.
Preserve existing template extension points during migration for third-party
modules; new checks should use the shared contract.

## Initial implementation and validation

The separate PR should introduce the coordinator, notification contract, model,
MQTT updates, and shared panel, then migrate the existing checks without changing
their thresholds. Report the disabled memory check honestly. Document the new
model, notifications, permissions, and any configuration introduced by that PR.

Verify independent completion, concurrency limits, timeout/crash handling,
deduplication, expiry, restart recovery, module activation/deactivation, site
isolation, ACL enforcement, and operation without a database. Exercise the UI
with slow checks, failures, stale retained snapshots, and all-success results.

Later PRs can add checks for mail delivery, database availability, mediarunner,
and backups. Active storage probes and automated remediation are outside the
initial scope.
