# Resource WebSub

`mod_websub` implements resource publication and subscription using
[WebSub](https://www.w3.org/TR/websub/). Enable it on both Zotonic sites. The
publishing site's ACL policy must permit `use` of `mod_websub` to the subscribing
user (anonymous for public subscriptions). Both sites need reachable HTTPS URLs.

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
selected resource. A separate **Also subscribe to imported collection items
(haspart)** checkbox includes direct collection members. Re-import options do not
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
* **Member content:** a member edit is a change to the member's own topic. The
  collection subscription alone does not keep that content current. Enable the
  separate collection-item option, or subscribe to each item explicitly.
* **Optional item subscriptions:** after importing references, direct `haspart`
  members with successful imports get their own ACL-checked subscriptions. New
  members added later are included. The option is saved on the collection and is
  not inherited by nested collections or unrelated connections. Each source must
  support WebSub; failures appear on that item's subscription.
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
{"status":"ok","result":{"uri":"https://a.example/id/123","resource":{}}}
```

The example abbreviates the export: actual delivery contains all export fields.
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
are rejected. Internal-only or localhost peers need publicly reachable endpoints.

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
The overview requires `use mod_admin_config`, checked by the controller and model.
Filter by subscription type (incoming subscribers / outgoing subscriptions) and
numeric **local resource ID**, **external hostname**, **status** (active, pending,
expired, stopped), and **errors** (all, with errors, without errors). Filters combine
and apply before pagination. Hostnames match exactly, case-insensitively, ignoring
ports and a trailing DNS dot. Enter a hostname without a scheme or path; IPv6
literals are accepted with or without brackets. Incoming subscriptions match the
callback host; outgoing subscriptions match the source resource host, not the hub.
The errors filter uses the same error indicator as the table (delivery errors for
incoming subscriptions; subscription or credential errors for outgoing ones). For an imported page, use the local copy's ID; its
external semantic `/id` identity remains unchanged.

Results show the local page, remote hostname, state, lease expiry, last sent or
received update, and an error indicator. Pagination uses 50 rows per page. Retained
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
