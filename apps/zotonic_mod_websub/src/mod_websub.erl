%% @copyright 2021-2026 Marc Worrell
%% @author Marc Worrell <marc@worrell.nl>
%% @doc Publish and subscribe to resources between sites using WebSub.
%% @end

%% Copyright 2021-2026 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(mod_websub).
-moduledoc(#{
    zotonic_keywords => [
        "reference", "integrator", "module", "export_and_syndication", "api_and_integration", "structured_data", "websub"
    ]
}).
-moduledoc("""
WebSub resource synchronization, following https://www.w3.org/TR/websub/.

## Integration points

Enable `mod_websub` on the publisher and importing site. Public subscriptions
require the publishing ACL policy to allow anonymous `use` of `mod_websub`;
private subscriptions use explicit HTTP authorization and current resource access.

* `controller_websub` handles hub requests, callback verification, and deliveries.
* `controller_websub_topic` serves the complete, fixed JSON topic representation.
* The admin Content menu links to a subscription overview, filtered by direction
  and local resource ID. It requires `use mod_admin_config`; edit pages show the
  active incoming subscriber count and a filtered overview link.
* `m_websub` provides the ACL-checked status/start/stop API and delivery/import queues.
* `z_websub_subscription` persists subscriber intent, leases, and renewal work.
* `z_websub_discovery` and `z_websub_http` handle discovery and outbound fetch policy.

The `resource_headers` and `rsc_export_done` observers advertise discovery links.
Full resource exports also include `websub.hub` and `websub.topic` for clients
subscribing from exported JSON. This optional JSON extension is present only on
authoritative resources and agrees with the standard discovery links. The semantic
resource identifier remains `uri`.
`rsc_import_fetch_result` exposes subscription availability to the import dialog;
`rsc_import_done` starts an explicitly requested subscription. `rsc_update_done`
queues publication, second/minute ticks schedule the unique queue worker, and the
daily tick performs maintenance. The protocol adapters are documented separately
from the resource importer: external peers need no Zotonic-specific WebSub fields.

## Resource identity versus the WebSub topic

A resource's identity is its language-less `m_rsc:uri/2`, including `/id/name`.
Imports store this in `source_uri` and the resource's `uri`. It is never replaced
by a page, representation, or hub URL. Discovery advertises `rel=self` pointing
to the fixed JSON `websub_topic` dispatch, and `rel=hub` to the local hub. The
WebSub topic is stored separately in `topic_url`. This distinction lets `/id`
retain semantic-web content negotiation while a WebSub topic has one media type.

## Flow between two Zotonic systems

1. On importing a connected resource from A into B, B fetches A's `/id` URI with
   `Accept: application/json`. The JSON export retains A's semantic resource URI.
   HTTP Link headers (or the JSON links extension) advertise A's JSON topic and hub.
2. If the editor opts into automatic updates, B persists the source URI, local
   resource, editor, and desired subscription state. Recursive imports do not opt in.
3. B rediscovers the source and POSTs a standard URL-encoded subscription request
   to the advertised hub. `hub.topic` is the discovered self URL, not necessarily
   `/id`. B supplies a unique callback, random secret, and requested lease.
4. A bounds and persists verification work and replies 202. Independently, A
   checks the subscribing user's resource access and GETs B's callback with the
   topic, mode, random challenge, and lease. B verifies pending intent and echoes
   the challenge as plain text with nosniff. A records the verified subscription.
5. Changes on A queue the resource's newest version. A rechecks access and lease,
   then POSTs the complete JSON topic representation, Link hub/self headers, and
   an HMAC-SHA256 signature to B. Private topics also send full authorized JSON;
   there is no notification-only or Zotonic-specific wire format.
6. B verifies the signature and callback, queues work, and quickly acknowledges.
   Public imports consume the signed export; credentialed imports refetch A's
   semantic URI using the editor's source credentials. Inside the import transaction
   B checks that the stored source, current resource URI, and payload URI agree,
   then applies newer content with the editor's current permissions and saved options.
7. B renews before lease expiry, rediscovering the hub/topic and rotating its
   callback and secret. The old callback remains active until the new one verifies.
   Stopping immediately disables imports and queues verified remote unsubscription.
   Confirmation also queues a catch-up fetch to recover missed updates.

## Collections and connected resources

Edge insertions, removals, and reordering advance the authoritative subject's
version and publish its updated export. Receivers synchronize edges using saved
import depth and filters, then fetch newly referenced placeholders after commit.
Existing imported members are reused, not refreshed by the collection's delivery.

The optional `is_subscribe_haspart` import setting subscribes direct collection
members after import, including later additions. It defaults to false and is not
inherited by connected resources. Members have independent subscriptions: removal
from a collection or stopping the parent does not unsubscribe them. See the module
README for the complete collection lifetime and retry behavior.

## Interoperation with other WebSub implementations

No peer-brand detection or private protocol extension is required. Any subscriber
can discover the fixed JSON topic from headers or HTML, subscribe using standard
form fields, answer verification, and receive its full `application/json` body.
The body's `result.uri` is the semantic identity; the delivery Link self names the
WebSub topic. Private subscriptions require explicit HTTP authorization and current access at
A's hub (cookies alone are insufficient); granting one authorizes full content delivery to the verified callback.

A non-Zotonic hub can relay the same JSON topic to B without changes. A non-Zotonic
publisher can be imported if it supplies the resource-export JSON understood by
Zotonic's resource importer. WebSub is media-type agnostic, but this application
adapter imports resources, not arbitrary Atom/RSS/HTML documents. It does not
claim that those document formats can be imported as Zotonic resources.

Standard subscription verification, signatures, leases, 307/308 hub redirects,
and unsubscription apply to all peers. A 202 alone never activates a subscription.
Delivery retry exhaustion drops only that notification, preserving the lease for
future updates. Semantic resource identity is independent of WebSub topic migration.

## Security and operation

All WebSub requests use `z_fetch` with automatic redirects disabled. Destinations
are checked for non-public addresses on each hop; private, loopback, link-local,
and reserved networks are rejected. HTTPS downgrades are refused. Cross-origin
GET redirects permanently drop user credentials for that chain. Callback requests
are anonymous. `z_fetch` integrates `mod_oauth2` consumer tokens using the original
HTTPS URL, hostname, and subscribing user; the source's same-origin hub can use
that token as well. Token lookup and renewal remain owned by `mod_oauth2`.

TODO: add validated-address pinning to `z_fetch`/`z_url_fetch`, preserving the
original Host header and TLS hostname. The current DNS preflight does not prevent
DNS rebinding between validation and connection establishment.

Hub admission deduplicates identical requests and limits each site to 120 new
verification tasks per minute and 1000 queued tasks. Admission is serialized in the
database across nodes; overload returns 503 before accepting work. Token group restrictions are retained when recreating delivery ACL contexts. Admin start,
stop, and status require edit permission. Callback tokens and secrets are not exposed
by status. Imports serialize identity and permission checks with the update.

The unique per-site sidejob processes renewal, delivery, and import queues. Schema
version 3 separates source identity from topic and adds durable admission accounting.
See README.md for deployment details and the regression suite.
""").

-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Resource WebSub").
-mod_description("Publish and subscribe to resources between sites using WebSub.").
-mod_depends([ cron ]).
-mod_schema(3).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").

-export([
    event/2,
    observe_admin_menu/3,
    observe_resource_headers/3,
    observe_rsc_export_done/3,
    observe_rsc_import_fetch_result/3,
    observe_rsc_import_fetch/2,
    observe_rsc_import_done/2,
    observe_tick_1s/2,
    observe_rsc_update_done/2,
    observe_edge_insert/2,
    observe_edge_update/2,
    observe_edge_delete/2,
    observe_tick_1m/2,
    observe_tick_24h/2,
    manage_schema/2,
    sidejob_check_queues/1
]).

%% @doc The overview has the same ACL at the menu, controller, and model boundary.
observe_admin_menu(#admin_menu{}, Acc, Context) ->
    [#menu_item{
        id = admin_websub,
        parent = admin_content,
        label = ?__("WebSub subscriptions", Context),
        url = {admin_websub, []},
        visiblecheck = {acl, use, mod_admin_config}
    } | Acc].

event(#postback{message = {subscription_start, Args}}, Context) ->
    subscription_result(m_websub:subscribe(m_rsc:rid(proplists:get_value(id, Args), Context), Context), Context);
event(#postback{message = {subscription_stop, Args}}, Context) ->
    subscription_result(m_websub:unsubscribe(m_rsc:rid(proplists:get_value(id, Args), Context), Context), Context).

subscription_result(ok, Context) ->
    z_render:wire({reload, []}, Context);
subscription_result({error, _}, Context) ->
    z_render:growl_error(?__("Could not change the automatic update subscription.", Context), Context).


observe_resource_headers(#resource_headers{ id = Id }, Acc, Context) when is_integer(Id) ->
    case m_rsc:p_no_acl(Id, is_authoritative, Context) of
        true ->
            publisher_headers(Id, Acc, Context);
        _ ->
            Acc
    end;
observe_resource_headers(#resource_headers{}, Acc, _Context) ->
    Acc.

publisher_headers(Id, Acc, Context) ->
    ContextNoLanguage = z_context:set_language('x-default', Context),
    HubUrl = z_context:abs_url(z_dispatcher:url_for(websub, [], ContextNoLanguage), ContextNoLanguage),
    SelfUrl = m_websub:topic_url(Id, ContextNoLanguage),
    % One combined header survives HTTP response maps which coalesce header names.
    [{<<"link">>, <<"<", HubUrl/binary, ">; rel=\"hub\", <", SelfUrl/binary, ">; rel=\"self\"">>} | Acc].


observe_rsc_export_done(#rsc_export_done{id = Id}, Export, Context) ->
    case m_rsc:p_no_acl(Id, is_authoritative, Context) of
        true ->
            publisher_export(Id, Export, Context);
        _ ->
            Export
    end.

publisher_export(Id, Export, Context) ->
    Ctx = z_context:set_language('x-default', Context),
    Self = m_websub:topic_url(Id, Ctx),
    Hub = z_context:abs_url(z_dispatcher:url_for(websub, [], Ctx), Ctx),
    Export#{
        <<"websub">> => #{
            <<"hub">> => Hub,
            <<"topic">> => Self
        },
        <<"links">> => [
            #{<<"rel">> => <<"self">>, <<"target">> => Self},
            #{<<"rel">> => <<"hub">>, <<"target">> => Hub}
        ]
    }.

observe_rsc_import_fetch_result(#rsc_import_fetch_result{final_url = Url, headers = Headers},
        #{<<"result">> := Result} = JSON, _Context) when is_map(Result) ->
    Discovery = z_websub_discovery:links(Url, Headers, <<>>, maps:get(<<"links">>, Result, undefined)),
    JSON#{<<"result">> => Result#{import_options => #{is_websub_supported => element(1, Discovery) =:= ok}}};
observe_rsc_import_fetch_result(_, JSON, _) ->
    JSON.

%% Automatic imports of referenced resources use the same redirect/SSRF policy
%% as the collection fetch. Ordinary interactive imports keep their normal path.
observe_rsc_import_fetch(#rsc_import_fetch{uri = Uri}, Context) ->
    case z_context:get(websub_safe_import, Context) of
        true ->
            z_websub_fetch_zotonic:fetch_json(Uri, Context);
        _ ->
            undefined
    end.

observe_rsc_import_done(#rsc_import_done{id = Id, options = Options}, Context) ->
    case proplists:get_value(is_subscribe, Options, false)
        andalso not m_rsc:p_no_acl(Id, is_authoritative, Context)
    of
        true ->
            m_websub:subscribe(Id, Context);
        false ->
            ok
    end.

%% A cheap indexed due scan each second also supports hubs granting short leases.
%% The unique sidejob prevents overlapping network/import work for this site.
observe_tick_1s(tick_1s, Context) ->
    observe_tick_1m(tick_1m, Context).


observe_rsc_update_done(#rsc_update_done{ action = Action, id = Id, post_props = Props }, Context)
    when Action =:= insert; Action =:= update ->
    case maps:get(<<"version">>, Props, undefined) of
        Version when is_integer(Version) ->
            m_websub:queue_push(Id, Version, Context);
        _ ->
            ok
    end;
observe_rsc_update_done(#rsc_update_done{}, _Context) ->
    ok.

%% Edge-only changes (including collection order) change the exported topic.
observe_edge_insert(#edge_insert{subject_id = Id}, Context) ->
    m_websub:queue_edge_update(Id, Context).

observe_edge_update(#edge_update{subject_id = Id}, Context) ->
    m_websub:queue_edge_update(Id, Context).

observe_edge_delete(#edge_delete{subject_id = Id}, Context) ->
    m_websub:queue_edge_update(Id, Context).

observe_tick_1m(tick_1m, Context) ->
    case z_sidejob:start_site_unique(?MODULE, ?MODULE, sidejob_check_queues, [], Context) of
        {ok, _Pid} ->
            ok;
        {error, already_running} ->
            ok;
        {error, overload} ->
            ok
    end.

observe_tick_24h(tick_24h, Context) ->
    ok = m_websub:cleanup_deleted_imports(Context),
    ok = m_websub:cleanup(Context).

manage_schema(Version, Context) ->
    m_websub:manage_schema(Version, Context).

sidejob_check_queues(Context) ->
    ok = z_websub_subscription:process(Context),
    ok = m_websub:process_push_queue(Context),
    ok = m_websub:process_import_queue(Context).
