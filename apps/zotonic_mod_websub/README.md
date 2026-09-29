# Resource WebSub

`mod_websub` implements resource publication and subscription using
[WebSub](https://www.w3.org/TR/websub/). Enable it on both Zotonic sites. The
subscriber must be allowed to view the original resource; anonymous visitors can
subscribe to public resources. The `use mod_websub` permission is only needed for
administration. Both sites need reachable HTTPS URLs.

## Two Zotonic sites

Assume A publishes a resource and B imports it.

| Step | Request or action | Persistent result |
| --- | --- | --- |
| Import | B GETs A's `/id/123` with `Accept: application/json` | B keeps A's language-less semantic URI as its resource `uri` and subscription `source_uri`. |
| Discover | B reads Link hub/self, following representation redirects as needed | The self URL, e.g. `/.zotonic/websub/topic/123`, becomes `topic_url`; it never replaces the resource URI. |
| Request | B POSTs `hub.mode=subscribe`, `hub.topic`, `hub.callback`, `hub.secret`, and `hub.lease_seconds` to A's hub | B records pending intent before sending; A accepts bounded durable work with 202. |
| Verify | A GETs B's unique callback with mode, topic, challenge, and granted lease | B matches pending intent and returns the exact challenge; A activates only after a successful response. |
| Publish | A POSTs the full JSON export to B with `Content-Type: application/json`, Link hub/self, and `X-Hub-Signature` | B verifies the body, queues work, and acknowledges promptly. |
| Import update | B consumes public signed JSON, or refetches `/id/123` with the editor's credentials | B checks the source URI, payload URI, current resource URI, version, and current editor permissions under the resource lock. |
| Renew | B rediscovers and requests a replacement callback/secret before expiry | The previous callback remains active until replacement verification succeeds. |
| Stop | The editor stops updates; B requests verified unsubscription | B immediately disables local imports and discards queued work. |

Initial import requires an explicit checkbox. By default it applies only to the
selected resource. A separate **Also subscribe to connected resources** checkbox
follows the **Connections** option across all predicates: none, direct connections,
or connections up to the selected deep-copy depth. Re-import options do not
silently restart a stopped parent subscription. Successful verification schedules
a catch-up fetch.

## Collection lifetime

A collection is a resource whose ordered `haspart` edges refer to individual
resources. Its WebSub topic contains the collection export and member references,
not the complete content of every member.

* **Initial import:** the connection-depth option controls which edges are imported.
  With depth zero, membership is not imported. With depth one, direct members are
  created or matched by semantic URI and their content is fetched asynchronously.
  Deeper imports follow connections up to the configured depth.
* **Membership updates:** insertion, deletion, and reordering of publisher edges
  advance the authoritative subject's version and queue publication. This includes
  edge-only changes, without requiring an editor to save the collection again.
  The receiver replaces membership/order using its saved import options and ACLs.
* **New members:** incoming updates first create placeholders. A durable task,
  queued in the same transaction, fetches previously unimported references after
  commit. It uses the subscribing editor's current permissions, saved per-resource
  depth/options, and WebSub's outbound URL/redirect checks. Already imported items
  are reused; a collection update does not refetch all existing member content.
* **Deleted members:** manually deleted imports stay deleted when a collection or
  another resource refers to them again. Automatic cleanup of an unconnected
  dependent resource permits it to be imported again if it returns. Older deletion
  records without a reason are treated as manual deletions. The import and
  re-import dialogs offer **Always import all resources, including manually deleted
  resources** (`is_import_deleted`). This saved option also applies to future
  updates and new references, within the selected connection depth and permissions.
  It does not force a refresh of already imported members.
* **Member content:** a member edit is a change to the member's own topic. The
  collection subscription alone does not keep that content current. Enable the
  separate connected-resource option, or subscribe to each item explicitly.
* **Optional connected-resource subscriptions:** after importing references,
  successfully imported resources get their own ACL-checked subscriptions within
  the selected connection depth, across all predicates. New connections added
  later are included. Connected imports inherit `is_subscribe_connections` with
  decremented `import_edges`; embedded resource references do not inherit it.
  Depth zero subscribes only the selected resource. Traversal handles shared
  resources and cycles without restarting subscriptions. Each source must support
  WebSub; failures appear on that resource's subscription.
* **Removal and stopping:** removing an edge does not delete the local item or stop
  its independent subscription. Stopping/deleting the collection or clearing the
  item option likewise leaves existing item subscriptions intact. Stop them on
  the items themselves. The normal Zotonic cleanup rules still apply to resources
  explicitly marked dependent.

Failed item imports remain placeholders and can be retried by a later collection
update or manual re-import. The optional item policy can re-enable a stopped item
on a subsequent collection update; clear the collection option to prevent this.

## Identity and standard delivery

The language-less `/id/123` or `/id/person` remains the unique semantic resource
identifier. It may negotiate HTML, JSON, or other semantic-web representations.
WebSub discovery instead advertises a stable JSON topic at the `websub_topic`
dispatch. That endpoint always returns the complete resource-export JSON envelope:

```json
{
  "status": "ok",
  "result": {
    "uri": "https://a.example/id/123",
    "resource": {},
    "websub": {
      "hub": "https://a.example/.zotonic/websub",
      "topic": "https://a.example/.zotonic/websub/topic/123"
    },
    "links": [
      {"rel": "self", "target": "https://a.example/.zotonic/websub/topic/123"},
      {"rel": "hub", "target": "https://a.example/.zotonic/websub"}
    ]
  }
}
```

The example abbreviates the export: actual delivery contains all export fields.
The optional `websub` object is a Zotonic JSON convenience extension: POST a
standard WebSub subscription request to `websub.hub`, using `websub.topic` as
`hub.topic` and your subscriber endpoint as `hub.callback`. Use the exported URLs
as supplied, rather than constructing paths. `uri` remains the semantic identity;
it is not replaced by the delivery topic. These fields contain no credentials or
callback secrets. They are omitted when the resource is non-authoritative or
`mod_websub` is inactive.
The topic controller and publisher both use `m_rsc_export:full/2`. HTTP headers,
HTML link elements, and the optional JSON `links` extension advertise the same
hub and JSON topic. Imported copies do not advertise a local hub for another site's
resource. All standard fields refer to the discovered topic; resource mapping uses
`result.uri` instead.

## Non-Zotonic peers

Any WebSub subscriber can subscribe to the JSON topic using standard discovery,
form parameters, verification, signatures, leases, and unsubscription. It receives
the full representation and matching content type, not a custom change notice.
There is no brand detection or opt-in private wire protocol.

A third-party WebSub hub may deliver this JSON representation unchanged to Zotonic.
A third-party publisher may be imported if it implements Zotonic's resource-export
JSON schema. Importing arbitrary RSS, Atom, or HTML documents is outside the resource
importer's scope; discovering links in such documents does not add a document importer.

Private subscriptions authenticate to the hub with an HTTP Authorization header
and require current resource access. Browser cookies alone do not authorize private
subscriptions. Verification and delivery retain the original authentication's group
restrictions and intersect them with the user's current groups. Legacy subscriptions
without stored restrictions receive only anonymous content until renewed.
They authorize sending the full private JSON representation to the verified callback.
Zotonic receivers may still refetch using their own credentials. The previous private
notification-only fallback has been removed because it was not standard full-content
delivery. Do not register an untrusted callback for private content.

## Security and deployment

Outbound requests use `z_fetch` with `{autoredirect, false}` and verified TLS.
Each destination is checked before fetching, including redirects, denial callbacks,
and deliveries. Private, loopback, link-local, multicast, and reserved addresses
are rejected. In the `development` environment, `.test` hostnames may resolve to
loopback or private-network addresses and use self-signed TLS certificates.
This exception applies to each request, including discovery, callbacks and
redirects; other hostnames and environments retain the public-address and verified
TLS requirements. Link-local metadata services and other reserved addresses remain
blocked, even for development `.test` peers.

OAuth2 consumer tokens configured with `mod_oauth2` are supported through the normal
`z_fetch` integration. Token lookup uses the original HTTPS URL, hostname, and
subscribing user context. The source's own hub can use the same token; unrelated
hubs and callback verification/delivery use anonymous context. Token lookup and
renewal remain owned by `mod_oauth2`.

Cross-origin GET redirects permanently switch to anonymous context;
HTTPS-to-HTTP redirects fail. Requests have timeouts and response-size limits.

### Open TODO: connection pinning

Add validated-address pinning to `z_fetch`/`z_url_fetch` while preserving the
original Host header and TLS hostname. The current DNS check is a preflight check:
the fetch library resolves the hostname again, so DNS rebinding between validation
and connection is not yet prevented.

Hub admission deduplicates identical pending requests. Each site accepts at most
120 new verification jobs per minute and holds at most 1000 pending verification
jobs. Database serialization enforces limits across nodes. Excess requests receive
503 and have not been accepted; callers may retry later. These limits bound the
WebSub contribution to the shared pivot task queue.

Status/start/stop require resource edit permission. Verification matches callback,
topic, action, and pending deadline. Responses are plain text with `nosniff`. Import
identity and permissions are checked again in the transaction that updates the resource.
Schema version 3 preserves existing semantic source URIs separately from discovered
topics; active subscriptions rediscover on renewal.

## Admin subscription overview

Open **Content → WebSub subscriptions** (`admin_websub`, `/admin/websub`).
The overview requires `use mod_websub`, checked by the controller and model.
Filter by subscription type (incoming subscribers / outgoing subscriptions) and
numeric **local resource ID**, **external hostname**, **status** (active, pending,
expired, stopped), and **errors** (all, with errors, without errors). Filters combine
and apply before pagination. Hostnames match exactly, case-insensitively, ignoring
ports and a trailing DNS dot. Enter a hostname without a scheme or path; IPv6
literals are accepted with or without brackets. Incoming subscriptions match the
callback host; outgoing subscriptions match the source resource host, not the hub.
Normalized hostname expression indexes on callback/source URLs support both the
filtered count and page query. PostgreSQL maintains them when URLs change.
The optional `($n is null or column = $n)` filters are simplified in custom
plans: missing arguments remove the filter; supplied arguments allow index
conditions. Zotonic's `epgsql:equery` reparses an unnamed statement per call, so
normal automatic planning uses custom plans. Forcing generic plans retains the
`OR` and can prevent selective index scans.

The errors filter uses the same error indicator as the table (delivery errors for
incoming subscriptions; subscription or credential errors for outgoing ones). For an imported page, use the local copy's ID; its
external semantic `/id` identity remains unchanged.

Results show the local page, remote hostname, state, lease expiry, last sent or
received update, and an error indicator. Failed outgoing subscriptions have a **Retry** button for users who can edit the
imported page. It starts a fresh subscription attempt immediately and reloads the
overview with its current filters. The standard pager uses the filtered subscription
count and 50 rows per page. Filter URLs use `qtype`, `qrsc_id`, `qhostname`,
`qstatus`, and `qerrors`, preserved by the pager’s `qargs` option. Retained
expired/stopped records are included; renewal generations appear separately, so
two active callbacks can temporarily refer to the same imported page. Incoming
subscriptions remain active until lease expiry even after a delivery error.
Callback URLs, secrets, tokens, and authentication data are never returned by the
overview model. Invalid filters show an error instead of listing all resources.

The resource edit sidebar shows the count of active incoming subscriptions and,
for administrators with overview access, links filtered to that resource and
direction. Editors without overview access can see the count for editable
resources, but cannot inspect subscriber details. Imported resources retain their
existing automatic-update status and start/stop controls.

Template API: `m.websub.subscriptions::%{type: "export", rsc_id: id, page: 1}`
(`type` also accepts `import` or `all`) and `m.websub.subscriber_count[id]`.
The latter requires resource edit permission, as does `m.websub.status[id]`.

## Validation

```
./rebar3 compile
bin/zotonic runtests websub_protocol_tests mod_websub_tests websub_lifecycle_tests websub_admin_tests m_rsc_import_db_tests
```

Tests cover discovery, semantic identity versus topic, signatures, IP restrictions,
OAuth2 token handling, credential redirects, subscription lifecycle, import identity checks, admission limits,
full JSON delivery, saved import options, and admin template rendering.

### Audit hardening

Automatic subscription work and imports reject disabled users. Subscription starts
recheck the resource source URI under the resource lock. A denial callback is
accepted only for a currently pending subscription request, so a late denial cannot
revoke a confirmed lease. Private hub requests authenticate their OAuth2 header
from an anonymous context; an existing browser login cannot supply the authority
for an invalid token.

Queue retries and exhausted-retry deletion are conditional on the version that
failed, preserving newer work arriving during HTTP requests. An older notification
still publishes current content, and an older pushed payload cannot replace a
newer pending refetch. Incoming version values are bounded positive integers. The hub only registers and
publishes authoritative local resources, matching the JSON topic controller.
Refetches are limited to 1 MiB and callback responses to 64 KiB; HTTP error bodies
and headers are not persisted in subscription errors or logged by the refetcher.

WebSub topic and callback parameters normalize unreserved percent-encoded URL
characters as required by the protocol; escaped delimiters remain escaped.
Discovery normalizes topic/hub URLs too, so verification uses the same topic identity.
The resource's semantic source URI remains independent of the WebSub topic.

Outgoing subscriptions reject authoritative local resources, including imported
copies whose source URL resolves to an authoritative resource on the same site.
Discovered and stored topic URLs are checked before subscription requests, including
renewals. Both the current callback and its still-active renewal predecessor are
stopped when a self-topic is detected. A self-subscription is stopped with `self_subscription` before sending
the request. Resource resolution includes language-less `/id` and JSON topic URLs;
other Zotonic sites on the same server remain eligible external sources.

## Circular subscriptions

WebSub specifies publisher-designated canonical topics and hub subscription policy,
but no global cycle detection, hop count, or definition of an authoritative copy.
See [Discovery](https://www.w3.org/TR/websub/#discovery) and
[Subscription Validation](https://www.w3.org/TR/websub/#subscription-validation).
Cycle prevention for resource replication is therefore an application policy.

Zotonic publishes only authoritative resources. Imported non-authoritative copies
advertise no local hub/topic, cannot register subscribers, and never enqueue
outgoing notifications. If a published resource becomes non-authoritative, its
next update removes its export subscriptions and queued deliveries. Delivery also
rechecks authority. Unsubscription remains possible after authority changes or
resource deletion; callback verification is still required.

For A → B → C, B's imported copy is not a WebSub source: C should import from A
and subscribe to A's advertised topic/hub. A copied page intentionally made
independent and authoritative is a new publication, not an automatically relayed
replica. Source URI checks, version checks, and the self-subscription guard provide
additional protection. A third-party application that rewrites identity and
republishes data as new authoritative content is outside this loop-prevention
policy; WebSub itself cannot detect that provenance.

## Queue scheduling

The per-site `mod_websub` server keeps a dirty flag and the earliest persisted due
time. One-second ticks consult this state without querying the database or starting
idle workers. Subscription changes and queued imports/pushes signal the server via
`queue_changed/1`; Zotonic defers these notifications until transaction commit.

A unique, monitored sidejob processes a batch and reads the next due time across
subscriptions, pushes, and imports. This preserves prompt short-lease renewal,
retry backoff, and draining of batches. Signals received during a batch remain
pending. Failed workers and sidejob overload retain pending work for the next tick.
A startup scan and a ten-minute fallback scan recover persisted work after restarts,
missed signals, or writes from another node. The database remains authoritative.

## Batching outgoing updates

`mod_websub.push_quiet_seconds` defaults to **10 seconds**. Each newer resource
version restarts this quiet period. `mod_websub.push_deadline_seconds` defaults to
**300 seconds**: publication becomes due at this deadline even if editing continues.
The deadline starts at the first pending update for each subscriber; later changes
retain it. Duplicate or older version notifications do not move either clock.
Queue rows survive restarts, and each delivery contains the latest full resource.

Both settings are available in module configuration. Quiet time accepts 0–86400
seconds; the deadline accepts 1–86400 seconds. Invalid values use their defaults.
The deadline caps the quiet period if configured shorter. It bounds batching, not
network delivery time: sidejob capacity and transport retry backoff still apply.

When optional `mod_presence` is active, WebSub consumes its MQTT
`presence/status/mod_admin/<id>` heartbeats. Fresh `ACTIVE` (4) heartbeats defer
publication by another quiet period, capped by the original deadline. Other states
remove that tab's activity; multiple editors/tabs are tracked independently.
Heartbeats expire after 20 seconds without refresh. Only authenticated publishers
with admin access and edit permission for the topic resource can affect the delay;
payload user IDs and locations are not trusted, and retained messages are ignored.
No presence module dependency is required. Without it, ordinary quiet-time batching
applies. Presence is an in-memory hint and is rebuilt from heartbeats after restart.

## Local subscriber callback policy

Zotonic accepts subscriber callback URLs only with a DNS hostname, using HTTP
or HTTPS. IPv4 and IPv6 literals, including numeric IPv4 shorthand, are rejected.
Use ASCII hostname labels (punycode for internationalized names); the final
label must start with a letter. A single trailing DNS root dot is allowed.
Invalid callbacks receive HTTP 400 with a plain-text policy explanation before
verification is queued. The policy also applies before sending verification,
denial, or delivery requests for previously stored subscriptions.

This is a **local Zotonic policy**, permitted by
[WebSub section 5.1.2](https://www.w3.org/TR/websub/#subscription-response-details),
not a WebSub requirement to reject IP addresses. It applies to subscriber
callbacks, not discovery URLs, remote hubs, or topics. DNS hostnames must still
resolve exclusively to public addresses, except for the development `.test`
exception above; using an arbitrary hostname does not bypass the SSRF checks. The existing DNS pinning TODO remains open.

### Subscription errors

The editor shows translated explanations with the original error under **Technical
details**, including for older stored errors. An `access-denied-websub` denial
can still come from older publishing sites which require `use mod_websub`.
Current publishers authorize subscriptions by resource visibility, without that
module permission. They recheck visibility before delivering updates.
Browser login cookies do not authenticate the subscription request; private
subscriptions need configured credentials. After upgrading a publisher or
correcting access, start automatic updates again: a hub denial stops the subscription.

The edit-page automatic-update status is a live template. Subscription confirmation,
denial, and worker attempts publish an empty invalidation event on
`model/websub/event/rsc/<id>`. Only editors of that resource can subscribe;
the refreshed template and status model also enforce resource edit permissions.
