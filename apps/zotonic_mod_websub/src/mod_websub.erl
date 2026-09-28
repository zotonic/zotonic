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
-moduledoc(<<"WebSub resource synchronization, following https://www.w3.org/TR/websub/.

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
queues publication. Second ticks consult the module server's dirty flag and next due
time; ten-minute ticks reconcile with the database. Enqueues signal after commit, and the
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
See README.md for deployment details and the regression suite."/utf8>>).

-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    context :: z:context(),
    dirty = true :: boolean(),
    next_due :: integer() | undefined,
    worker :: {pid(), reference(), boolean()} | undefined,
    editors = #{} :: map()
}).

-mod_title("Resource WebSub").
-mod_description("Publish and subscribe to resources between sites using WebSub.").
-mod_depends([ cron ]).
-mod_schema(3).
-mod_config([
    #{
        key => push_quiet_seconds,
        type => integer,
        default => 10,
        description => "Seconds without resource changes before pushing updates (0-86400)."
    },
    #{
        key => push_deadline_seconds,
        type => integer,
        default => 300,
        description => "Maximum batching delay from the first queued update, in seconds (1-86400)."
    }
]).

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
    pid_observe_tick_1s/3,
    observe_rsc_update_done/2,
    observe_edge_insert/2,
    observe_edge_update/2,
    observe_edge_delete/2,
    pid_observe_tick_10m/3,
    observe_tick_24h/2,
    manage_schema/2,
    sidejob_check_queues/2,
    queue_changed/1,
    is_editor_active/2,
    'mqtt:presence/status/mod_admin/+'/2
]).

%% @doc The overview has the same ACL at the menu, controller, and model boundary.
observe_admin_menu(#admin_menu{}, Acc, Context) ->
    [
        #menu_item{
            id = admin_websub,
            parent = admin_content,
            label = ?__("WebSub subscriptions", Context),
            url = {admin_websub, []},
            visiblecheck = {acl, use, mod_admin_config}
        }
        | Acc
    ].

event(#postback{message = {subscription_start, Args}}, Context) ->
    subscription_result(m_websub:subscribe(m_rsc:rid(proplists:get_value(id, Args), Context), Context), Context);
event(#postback{message = {subscription_stop, Args}}, Context) ->
    subscription_result(m_websub:unsubscribe(m_rsc:rid(proplists:get_value(id, Args), Context), Context), Context).

subscription_result(ok, Context) ->
    z_render:wire({reload, []}, Context);
subscription_result({error, _}, Context) ->
    z_render:growl_error(?__("Could not change the automatic update subscription.", Context), Context).


observe_resource_headers(#resource_headers{ id = Id }, Acc, Context) when is_integer(Id) ->
    case m_rsc:p_no_acl(Id, <<"is_authoritative">>, Context) of
        true ->
            publisher_headers(Id, Acc, Context);
        _ ->
            Acc
    end;
observe_resource_headers(#resource_headers{}, Acc, _Context) ->
    Acc.

publisher_headers(Id, Acc, Context) ->
    ContextNoLang = z_context:set_language('x-default', Context),
    HubUrl = z_context:abs_url(z_dispatcher:url_for(websub, [], ContextNoLang), ContextNoLang),
    SelfUrl = m_websub:topic_url(Id, ContextNoLang),
    % One combined header survives HTTP response maps which coalesce header names.
    [
        {<<"link">>, <<"<", HubUrl/binary, ">; rel=\"hub\", <", SelfUrl/binary, ">; rel=\"self\"">>}
        | Acc
    ].

observe_rsc_export_done(#rsc_export_done{id = Id}, Export, Context) ->
    case m_rsc:p_no_acl(Id, <<"is_authoritative">>, Context) of
        true ->
            publisher_export(Id, Export, Context);
        _ ->
            Export
    end.

publisher_export(Id, Export, Context) ->
    ContextNoLang = z_context:set_language('x-default', Context),
    Self = m_websub:topic_url(Id, ContextNoLang),
    Hub = z_context:abs_url(z_dispatcher:url_for(websub, [], ContextNoLang), ContextNoLang),
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

observe_rsc_import_fetch_result(
        #rsc_import_fetch_result{ final_url = Url, headers = Headers },
        #{ <<"result">> := Result } = JSON,
        _Context)
    when
        is_map(Result) ->
    IsSupported = case z_websub_discovery:links(Url, Headers, <<>>, maps:get(<<"links">>, Result, undefined)) of
        {ok, _} -> true;
        {error, _} -> false
    end,
    JSON#{
        <<"result">> => Result#{
            <<"import_options">> => #{
                <<"is_websub_supported">> => IsSupported
            }
        }
    };
observe_rsc_import_fetch_result(_, JSON, _) ->
    JSON.

%% Automatic imports of referenced resources use the same redirect/SSRF policy
%% as the collection fetch. Ordinary interactive imports keep their normal path.
observe_rsc_import_fetch(#rsc_import_fetch{ uri = Uri }, Context) ->
    case z_context:get(websub_safe_import, Context) of
        true ->
            z_websub_fetch_zotonic:fetch_json(Uri, Context);
        _ ->
            undefined
    end.

observe_rsc_import_done(#rsc_import_done{id = Id, options = Options}, Context) ->
    case proplists:get_value(is_subscribe, Options, false)
        andalso not m_rsc:p_no_acl(Id, <<"is_authoritative">>, Context)
    of
        true ->
            m_websub:subscribe(Id, Context);
        false ->
            ok
    end.

observe_rsc_update_done(#rsc_update_done{ action = Action, id = Id, post_props = Props }, Context)
    when
        Action =:= insert; Action =:= update ->
    case maps:get(<<"version">>, Props, undefined) of
        Version when is_integer(Version) ->
            m_websub:queue_push(Id, Version, Context);
        _ ->
            ok
    end;
observe_rsc_update_done(#rsc_update_done{}, _Context) ->
    ok.

%% @doc Edge-only changes (including collection order) change the exported topic.
observe_edge_insert(#edge_insert{subject_id = Id}, Context) ->
    m_websub:queue_edge_update(Id, Context).

%% @doc Edge-only changes (including collection order) change the exported topic.
observe_edge_update(#edge_update{subject_id = Id}, Context) ->
    m_websub:queue_edge_update(Id, Context).

%% @doc Edge-only changes (including collection order) change the exported topic.
observe_edge_delete(#edge_delete{subject_id = Id}, Context) ->
    m_websub:queue_edge_update(Id, Context).

%% @doc Frequent ticks consult memory only; due work still runs in a unique sidejob.
pid_observe_tick_1s(Pid, tick_1s, _Context) ->
    gen_server:cast(Pid, poll).

%% @doc Recover persisted work after restart, missed notifications, or remote-node writes.
pid_observe_tick_10m(Pid, tick_10m, _Context) ->
    gen_server:cast(Pid, slow_poll).

observe_tick_24h(tick_24h, Context) ->
    ok = m_websub:cleanup_deleted_imports(Context),
    ok = m_websub:cleanup(Context).

manage_schema(Version, Context) ->
    m_websub:manage_schema(Version, Context).

%% @doc Consume the optional presence module's admin-edit heartbeat. Trust the
%% publisher's authenticated context, never the payload's user_id or location.
'mqtt:presence/status/mod_admin/+'(#{retain := true}, _Context) ->
    ok;
'mqtt:presence/status/mod_admin/+'(#{
        topic := [ <<"presence">>, <<"status">>, <<"mod_admin">>, IdBin ],
        payload := #{ <<"unique_id">> := Client, <<"status">> := Status }
    }, Context)
    when is_binary(IdBin), byte_size(IdBin) =< 20,
         is_binary(Client), byte_size(Client) > 0, byte_size(Client) =< 128,
         is_integer(Status), Status >= 0, Status =< 4 ->
    Id = try binary_to_integer(IdBin) catch error:badarg -> undefined end,
    case is_integer(Id) andalso Id > 0 andalso Id =< 2147483647
        andalso z_module_manager:active(mod_presence, Context)
        andalso z_auth:is_auth(Context)
        andalso z_acl:is_allowed(use, mod_admin, Context)
        andalso z_acl:rsc_editable(Id, Context)
    of
        true ->
            case z_module_manager:whereis(?MODULE, Context) of
                {ok, Pid} ->
                    gen_server:cast(Pid, {editor_presence, Id, z_acl:user(Context), Client, Status});
                _ -> ok
            end;
        false -> ok
    end;
'mqtt:presence/status/mod_admin/+'(_Message, _Context) ->
    ok.

%% @doc Whether a fresh ACTIVE heartbeat exists for this resource. Presence is an
%% optional batching hint; an unavailable server must never block publication.
-spec is_editor_active(Id, Context) -> boolean()
    when Id :: m_rsc:resource_id(), Context :: z:context().
is_editor_active(Id, Context) ->
    try
        case z_module_manager:whereis(?MODULE, Context) of
            {ok, Pid} -> gen_server:call(Pid, {is_editor_active, Id}, 1000);
            _ -> false
        end
    catch
        exit:_ -> false
    end.

%% @doc Signal durable queue changes. Notifications are deferred until transaction
%% commit, so the worker cannot consume a wakeup before the new row is visible.
-spec queue_changed(Context) -> ok when Context :: z:context().
queue_changed(Context) ->
    z_notifier:notify(websub_queue_changed, Context),
    ok.

%% @doc Process one batch, then remember the earliest remaining retry or renewal.
sidejob_check_queues(Server, Context) ->
    ok = z_websub_subscription:process(Context),
    ok = m_websub:process_push_queue(Context),
    ok = m_websub:process_import_queue(Context),
    Delay = m_websub:next_queue_delay(Context),
    NextDue = case Delay of
        undefined -> undefined;
        Seconds -> erlang:monotonic_time(second) + Seconds
    end,
    Server ! {queues_checked, self(), NextDue},
    ok.

%% @doc Start the per-site queue scheduler. Database and network work stays in sidejobs.
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    z_notifier:observe(websub_queue_changed, self(), Context),
    {ok, #state{context = z_context:new(Context)}}.

handle_call({is_editor_active, Id}, _From, #state{editors = Editors} = State) ->
    Fresh = fresh_editors(Editors),
    Active = lists:any(fun({{RscId, _, _}, _}) -> RscId =:= Id end, maps:to_list(Fresh)),
    {reply, Active, State#state{editors = Fresh}};
handle_call(_Message, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({editor_presence, Id, UserId, Client, Status}, #state{editors = Editors} = State) ->
    Fresh = fresh_editors(Editors),
    Key = {Id, UserId, Client},
    Editors1 = case Status of
        4 -> Fresh#{Key => erlang:monotonic_time(second) + 20};
        _ -> maps:remove(Key, Fresh)
    end,
    {noreply, State#state{editors = Editors1}};
handle_cast({websub_queue_changed, _Context}, State) ->
    {noreply, State#state{dirty = true}};
handle_cast(slow_poll, State) ->
    {noreply, maybe_start_worker(State#state{dirty = true, editors = fresh_editors(State#state.editors)})};
handle_cast(poll, State) ->
    {noreply, maybe_start_worker(State)};
handle_cast(_Message, State) ->
    {noreply, State}.

handle_info({queues_checked, Pid, NextDue}, #state{worker = {Pid, Ref, false}} = State) ->
    % Do not clear dirty here: enqueues during this batch require another pass.
    {noreply, State#state{next_due = NextDue, worker = {Pid, Ref, true}}};
handle_info({'DOWN', Ref, process, Pid, _Reason},
        #state{worker = {Pid, Ref, Completed}, dirty = Dirty} = State) ->
    {noreply, State#state{worker = undefined, dirty = Dirty orelse not Completed}};
handle_info(_Message, State) ->
    {noreply, State}.

terminate(_Reason, #state{context = Context}) ->
    z_notifier:detach(websub_queue_changed, self(), Context),
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

maybe_start_worker(#state{worker = Worker} = State) when Worker =/= undefined ->
    State;
maybe_start_worker(#state{dirty = Dirty, next_due = NextDue, context = Context} = State) ->
    IsDue = is_integer(NextDue) andalso NextDue =< erlang:monotonic_time(second),
    case Dirty orelse IsDue of
        false ->
            State;
        true ->
            case z_sidejob:start_site_unique(?MODULE, ?MODULE, sidejob_check_queues, [self()], Context) of
                {ok, Pid} ->
                    Ref = erlang:monitor(process, Pid),
                    State#state{dirty = false, worker = {Pid, Ref, false}};
                {error, _} ->
                    % Keep the pending work when overloaded or an older worker is running.
                    State
            end
    end.

%% Presence publishes every seven seconds and treats missing peers as gone at 20s.
fresh_editors(Editors) ->
    Now = erlang:monotonic_time(second),
    maps:filter(fun(_Key, Expires) -> Expires > Now end, Editors).
