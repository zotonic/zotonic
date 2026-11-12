# Traffic guard

`mod_traffic_guard` detects sustained content fetching independently of the User-Agent.
It is optional and defaults to **observe** when enabled. It needs no analytics module,
JavaScript, new cookies, external verification service or database tables.

## Enable and configure

Enable **Traffic guard** in the site's Modules screen. Use **System → Configuration**
with module `mod_traffic_guard`, or the trusted server-side configuration API:

```erlang
C = z:c(your_site).
z_module_manager:activate_await(mod_traffic_guard, C).
m_config:set_value(mod_traffic_guard, mode, <<"observe">>, C).
```

Configuration changes are administrator operations; this module adds no public
configuration or client-state API. Use observe mode to tune against normal traffic
before setting `mode` to `enforce`.

| Setting | Default | Accepted values |
| --- | --- | --- |
| `mode` | `observe` | `off`, `observe`, `enforce` |
| `period` | `60` | 1–3600 seconds |
| `request_limit` | `120` | 1–100000 requests replenished per period |
| `request_burst` | `30` | 1–1000 immediately available request credits |
| `distinct_limit` | `60` | 1–100000 distinct-path credits replenished per period |
| `distinct_burst` | `20` | 1–64 immediately available distinct-path credits |
| `max_clients` | `5000` | 1–50000 retained client budgets per site/node |

Invalid settings use their defaults. Changes reset transient budgets. With the
default settings, a new client can request 30 times immediately, with two request
credits replenishing per second. It can fetch 20 distinct paths immediately, with
one distinct credit replenishing per second. Both budgets must permit a request.
Repeated visits to a remembered path consume only the request budget.

- **off:** bypass detection and emit no guard decisions.
- **observe:** allow requests, reporting `would_throttle` when the shadow budget
  would reject them. Shadow admission/recovery matches enforce mode.
- **enforce:** reject excess requests with a small HTTP 429 text response,
  `Retry-After` in seconds and `Cache-Control: no-store`.

Rejected requests do not consume further credits or extend a blocking period.
Clients recover automatically. Nothing is queued intentionally to delay a client.

## Request coverage

GET/HEAD requests to `controller_page`, `controller_template`, `controller_api`,
`controller_agent_content` and `controller_agent_discovery` are covered. Other
controllers require `{traffic_guard, true}` in their dispatch options. Use
`{traffic_guard, false}` to exempt an individual route:

```erlang
{expensive_page, ["catalog"], controller_custom, [{traffic_guard, true}]},
{health, ["health"], controller_template, [{template, "health.tpl"}, {traffic_guard, false}]}
```

Static controllers, POST requests, MQTT, and requests rejected before dispatch are
outside this version's coverage. A GET model call is covered; a POST to the same
model is not. This is not a general API abuse firewall.

Budgets aggregate covered routes per **site and client IP**. User-Agent changes,
cookie rotation and language changes do not reset the request budget. There are
no automatic administrator or self-declared crawler exemptions. Shared offices,
carrier NAT and proxies share allowances: do not deploy strict IP limits without
observing legitimate traffic. Client IP comes from Cowmachine's trusted-proxy
processing; never use arbitrary forwarded headers to identify clients.

Distinct targets are SHA-256 hashes of request paths, excluding query strings.
This is deliberately conservative about query variations: all variants consume
request credits, but do not each count as another distinct path. Up to 64 hashes
are retained per client for `period` seconds. If that set is full, unremembered
paths consume distinct credits on each attempt until there is room.

## Analytics

Enable `mod_analytics` separately. Its default **Page requests** chart adds
**Would throttle** and **Throttled** lines for page attempts classified as `other`
or `unknown`. A companion recording reserves 10,734 rows within the existing row
budget. Existing page-view history is preserved; guard history starts when the
companion is created. Missing earlier samples remain gaps. Deleting the companion
does not recreate it on restart. It remains visible in recording configuration,
while the dashboard combines it with the default Page requests chart.

For additional breakdowns, under **System → Analytics configuration**, create
an active recording with event **Traffic guard decisions** (`mod_traffic_guard:request`).
For example, break down by `guard_action` with four groups (three actions plus
overflow). In observe mode, filter `guard_action = would_throttle` and group by
`guard_reason` to tune the budgets.

| Dimension | Values |
| --- | --- |
| `crawl_behavior` | `normal`, `excessive`, `unknown` |
| `guard_action` | `allow`, `would_throttle`, `throttle` |
| `guard_reason` | `none`, `request_rate`, `distinct_targets`, `state_capacity`, `unavailable` |
| `traffic` | `crawler`, `agent`, `other`, `unknown` |
| `type` | `page`, `api`, `other` |

Each checked request emits one guard decision. Allowed requests emit at the
handled phase; early rejections emit before returning 429. Requests aborted by
an error or connection upgrade before that phase may have no decision event.
Existing `page_request`, `api_request` and `request` events also expose the three
guard dimensions when assessed. Guard-disabled or out-of-scope requests have no
guard fields. Early rejected requests do not become page views.

Guard and ordinary request events overlap: do not add their totals together.
Analytics is best-effort and retains its consent, admin and `analytics_exclude`
policies. Rejected requests initialize authentication only when a guard recording
is interested, without reading the request body. Allowed requests retain normal
controller authentication. Enforcement is
independent of those policies, analytics availability and configured recordings.
Analytics events contain no client IPs, cookies, tokens, path hashes, query strings
or visited-URL lists. The administrator-only current-throttling view exposes IPs
for operational decisions.

## Operational boundaries

One worker per site serializes admission so concurrent requests share a budget.
Its client map is capped; idle entries are swept every 30 seconds after budgets
could fully recover. IP addresses and hashes exist only in transient memory.
Restarting the worker clears them. Only administrators can inspect the bounded current-throttling snapshot.

At client capacity, a new client is allowed with `state_capacity`; existing clients
remain checked. If the worker is absent, has a queue of at least 100 messages when
checked, or does not reply within one second, requests are allowed with
`unavailable`. The queue check is an overload safeguard, not a hard mailbox bound.
These fail-open outcomes report `crawl_behavior = unknown`, never accuse the client
of excessive crawling. The guard should be backed by reverse-proxy limits when
strong availability guarantees are required.

Limits are per node, not cluster-wide. There is no distributed identity detection,
TLS fingerprinting, challenge page, account-level budget or concurrency limit in
this version. Anonymous clients rotating IP addresses can evade per-IP detection.

## Validation

Pure/worker regressions are in `z_traffic_guard_tests`. The explicit development-site
integration test `traffic_guard_integration_tests:run(Context)` exercises real HTTP
requests in all three modes, recovery, response headers and analytics. It restores
configuration/module activation and deletes its temporary recording afterward.

`traffic_guard_page_tests:run(Context)` checks the default chart, history preservation,
row-budget exhaustion and deletion semantics on a development site with both
modules enabled. See [Audit and benchmarks](doc/audit-and-benchmark.md) for
security findings, measurements and reproduction commands.


## IP allow and block entries

Open **System → Traffic guard**. Entries are stored in the site's `traffic_guard_ip`
table: list kind, normalized IP, required note, creation date and optional expiry.
Adding an existing address to the same list updates its note and expiry. Each kind
allows at most 256 site entries; multi-address saves are atomic. Exact IPv4 and IPv6
addresses can be separated by spaces, commas or newlines. IPv4-mapped IPv6 addresses
match IPv4 entries. Networks, ranges, hostnames and zone identifiers are rejected.

Choose hour, day, week, month (30 days), forever, or an explicit UTC date/time.
Expired entries stop matching on the next request, including when rules are cached.
Hourly maintenance deletes expired database rows. Empty expiry means forever.
**Add IP entries** and each row's **Edit** button open dialogs for notes and expiry.
The merged site/global list shows allow entries first with a green left bar, then
deny entries with a red bar. Text labels also identify the rule type.
**Add to allow list** opens the add dialog with the current request's trusted
client IP prefilled; add a note and save it.

Allow takes precedence across both site and global entries and bypasses rate
budgets. Blocks follow mode: observe records `would_throttle`, enforce returns
429, and off bypasses checks. Rules apply only to guarded routes and methods.
Explicit IP rules report `ip_allow`/`ip_block` reasons; a block is not evidence of
excessive crawling, so its crawl behavior is unknown. Worker overload still fails
open. This is not a site-wide firewall.

### Current throttling

The admin page shows up to 200 currently rate-limited IPs on this node, including
observe-mode decisions, with reason, rejection count and retry interval. A row
expires when its retry interval ends or the client recovers. It contains no paths,
query strings or user agents. The list is bounded by the worker's client capacity,
is not persisted, and clears when the worker restarts or settings change.

**Block IP** adds a timed block with a server-generated note containing the reason,
rejection count and addition time. Choose hour, day, week, 30-day month or forever.
If the client has already recovered, refresh and use the normal add form if needed.
Existing explicit blocks are shown in the rules section, rather than tracked again
as rate-limit events. Refresh the page for current evidence.

### Global rules in zotonic.config

Configure these under the `zotonic` application:

```erlang
{traffic_guard_allow, [
    #{ip => "192.0.2.10", note => "Office monitoring"},
    "2001:db8::10"
]},
{traffic_guard_block, [
    #{ip => "198.51.100.20", note => "Operator block"}
]}
```

Both plain address strings/binaries and maps with `ip`/`note` are accepted. Plain
entries get the note `Configured in zotonic.config`. These permanent rules are
always merged when each site's guard settings load. They are shown as read-only
rows and cannot be removed by deleting site rows. Allow takes precedence even if
a global block and a site allow overlap. They still respect off/observe/enforce.
Apply file changes through the normal Zotonic configuration restart; resetting a
site worker reloads the currently loaded global settings. No JSON-list migration
is included.

### API and access

Only administrators can read entries or current throttling. Writes also reject
read-only credentials. Through the normal HTTP/MQTT model interface:

- GET `traffic_guard/lists`: site and global rule rows.
- GET `traffic_guard/throttled`: current node's rate-limited IPs.
- POST `traffic_guard/entry`: `kind`, `ips`, `note`, `validity`.
- POST `traffic_guard/block`: `ip`, `validity`; generates the note from current evidence.
- DELETE `traffic_guard/entry/<id>`: delete a site row.

`validity` is `hour`, `day`, `week`, `month`, `forever`, or `YYYY-MM-DDTHH:MM` in UTC.
API expiry values are Unix seconds; zero represents forever. Rule changes reload
the local in-memory copy synchronously without clearing other clients’ budgets or
current throttling evidence. Rate budgets and snapshots remain node-local.

`traffic_guard_ip_integration_tests:run(Context)` checks real database saves and
edits, escaping, read-only denial, global-rule merging, cached expiry, cleanup,
automatic block notes and live HTTP admission in every mode. It restores touched
configuration and deletes its reserved test entries. Run it only on a development
site with the module active and no existing rules for its reserved test IPs.
