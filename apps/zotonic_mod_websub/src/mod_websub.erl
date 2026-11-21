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
        "reference", "integrator", "module", "export_and_syndication", "api_and_integration", "structured_data", "http"
    ]
}).
-moduledoc(<<"WebSub keeps imported resources up to date when their source changes. A publishing
site notifies subscribers instead of requiring them to poll for every update.
`mod_websub` provides both publication and subscription for Zotonic resources,
using the WebSub protocol.

## When to use it

Use this module to synchronize resources between sites while preserving their
original identity and applying the importing editor's permissions and options.
Changes to collections can also update membership and order. Subscribing to a
collection does not, by itself, keep every member's content up to date.

Both sites need reachable URLs for subscription verification and delivery; use
HTTPS for deployment. Enable `mod_websub` on both sites when both run Zotonic.
Other WebSub subscribers and hubs can participate, but Zotonic's resource importer
requires its resource-export JSON schema. This module does not import arbitrary
RSS, Atom or HTML documents as resources.

## Getting started

1. Enable `mod_websub` on the publishing and importing Zotonic sites.
2. On the importing site, import the original resource and explicitly select
   automatic updates in the import dialog.
3. Choose how deeply to import connections. Select **Also subscribe to connected
   resources** if those resources should receive their own updates as well.
4. Check the imported resource's automatic-update status. A subscription becomes
   active only after the publisher verifies the callback, not merely after it
   accepts the request.

Use the resource editor's controls to start or stop updates. Stopping disables
local automatic imports immediately and schedules remote unsubscription.
**Content → WebSub subscriptions** provides an overview of incoming subscribers
and outgoing subscriptions, including errors and a retry action.

## Configuration and usage

Settings are per site under `mod_websub` in module configuration:

| Key | Default | Valid range (seconds) | Purpose |
| --- | --- | --- | --- |
| `subscription_retry_timeout` | `604800` (one week) | 1–2147483647 | Overall retry budget for subscription discovery, verification and unsubscription. |
| `push_quiet_seconds` | `10` | 0–86400 | Quiet period after changes before publishing. |
| `push_deadline_seconds` | `300` | 1–86400 | Maximum batching delay from the first queued update. |

Missing or invalid settings use their defaults. For example:

```erlang
m_config:set_value(mod_websub, push_quiet_seconds, 10, Context).
```

Newer changes restart the quiet period, but not the original batching deadline.
Each delivery contains the latest complete resource. These settings bound batching,
not network delivery time or retry delays. When `mod_presence` is active, fresh
editing heartbeats can defer publication within the same deadline; without it,
ordinary quiet-time batching applies.

### Permissions and administration

Subscribers must be allowed to view the original resource. Public resources permit
anonymous subscriptions; private subscriptions require explicit HTTP authorization
and current resource access. Browser login cookies alone are insufficient. A private
subscription authorizes delivery of the full JSON content to the verified callback.

`use mod_websub` controls access to the subscription overview. Local status, start
and stop operations require resource edit permission. The overview can filter by
direction, local resource ID, remote hostname, state and errors. It exposes no
callback URLs, secrets or tokens. Resource edit pages show incoming subscriber
counts and automatic-update controls. See `m_websub` for the model and Erlang APIs.

### Collections and connected resources

The saved connection depth and filters control which edges are synchronized.
Publisher edge insertion, deletion and reordering trigger updates even without a
resource edit. Newly referenced resources are imported after commit; existing
imported members are reused rather than refreshed by a collection delivery.

The optional `is_subscribe_connections` setting defaults to false. When enabled,
connected resources across all predicates receive independent subscriptions within
the saved depth, including later additions. Removing a connection or stopping the
parent does not stop those subscriptions. Stop them on the individual resources.
Manual deletions remain respected unless the saved import options explicitly allow
re-importing deleted resources. The README describes the full collection lifecycle.

## Subscription and delivery behavior

The subscriber discovers the source's hub and JSON topic, records intent, and
requests a subscription. The publisher verifies a unique callback before activating
it. Deliveries carry the full JSON representation with an HMAC-SHA256 signature.
The receiver validates the callback and signature, queues import work, and
acknowledges promptly. Public imports consume the signed export; credentialed
imports refetch with the editor's source credentials. Source identity and current
permissions are checked again inside the import transaction.

Renewal rediscovers the source and rotates the callback and secret. The previous
callback remains active until the replacement verifies. Confirmation schedules a
catch-up fetch to recover missed updates. Stopping immediately disables local
imports, even while remote unsubscription is pending.

Subscription failures and unanswered verification back off from 10 seconds to
12 hours within the configured retry budget. The budget survives restarts; renewal,
unsubscription and explicit retries start a fresh budget. Exhausted attempts stop
with `retry_timeout`; a previous confirmed lease remains valid until expiry.
Settings are read when work runs, so changing the budget does not immediately wake
sleeping retries or restart exhausted attempts.

Notification delivery has a separate, fixed retry window: one week from the first
attempt, never beyond lease expiry. Delays are 5, 20, 45 minutes and so on, capped
at 12 hours. Newer queued versions preserve that window. Exhaustion discards the
notification and records `retry_timeout`, but preserves the lease; later resource
updates can start a new delivery window. Import queues and feed polling are not
controlled by `subscription_retry_timeout`.

## Identity and interoperability

A resource's identity remains its language-less `m_rsc:uri/2`, such as `/id/name`.
Imports retain it as their resource `uri` and subscription `source_uri`. The fixed
JSON WebSub topic is stored separately in `topic_url`; it never replaces the
semantic identity.

Discovery links advertise the JSON topic as `rel=self` and the hub as `rel=hub`.
Authoritative resource exports also include optional `websub.hub` and `websub.topic`
JSON convenience fields. External peers can use standard discovery and form fields
without those extensions. Deliveries are complete `application/json` representations,
not private change notices. The delivery's `result.uri` identifies the resource;
its Link self identifies the topic.

Only authoritative resources publish. Imported copies do not advertise a local
hub or relay updates as a new source. For A → B → C, C should subscribe to A rather
than B's imported copy. Local self-subscriptions are rejected. The README explains
third-party interoperability, topic migration and circular-subscription boundaries.

## Security and deployment

Outbound requests use verified TLS and explicit redirect handling. Destinations
are checked on every hop; private, loopback, link-local and reserved addresses are
rejected. Development `.test` peers may use loopback/private addresses and
self-signed TLS. Other destinations retain the normal checks. HTTPS downgrades are
refused, and cross-origin GET redirects drop credentials for the rest of the chain.

Subscriber callbacks must use DNS hostnames; IP literals are rejected by Zotonic's
local policy. Callback verification and delivery are anonymous. `mod_oauth2`
consumer tokens can authenticate source fetches and the source's same-origin hub,
using the original HTTPS URL and subscribing user. Token lookup and renewal remain
owned by `mod_oauth2`; current group restrictions are preserved for delivery access.

DNS validation is currently a preflight check, not connection-address pinning.
DNS rebinding between validation and connection remains an open limitation.

Hub admission deduplicates pending requests and bounds each site to 120 new
verification tasks per minute and 1000 queued tasks. Limits are serialized in the
database across nodes; overload returns 503 before accepting work. Imports recheck
identity and permissions under the resource lock, and disabled users cannot continue
automatic subscription work.

## Implementation and lifecycle

`controller_websub` handles hub requests, verification and deliveries;
`controller_websub_topic` serves the fixed JSON representation. Resource-header and
export observers advertise discovery links, import observers apply explicit
subscription choices, and resource/edge observers queue publication.

`m_websub` provides checked APIs and queue processing. Subscription intent, leases,
retry state and work queues persist in the database. A per-site server tracks a
dirty flag and next due time; second ticks avoid database work while idle. A unique,
monitored sidejob processes due work. Signals are sent after transaction commit,
ten-minute scans reconcile persisted work, and daily maintenance cleans up records.

Separate export revisions advance for resource and edge changes without changing
the resource's edit version for edge-only updates. Schema upgrades preserve queues
and identity. The README contains wire examples, storage details, operational limits
and regression-test commands.
"/utf8>>).

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

-mod_title("WebSub resource synchronization").
-mod_description("Publish and subscribe to resources between sites using WebSub.").
-mod_depends([ cron, mod_mqtt ]).
-mod_schema(7).
-mod_config([
    #{
        key => subscription_retry_timeout,
        type => integer,
        default => 604800,
        description => "Overall subscription and unsubscription retry time from the first attempt, in seconds (1-2147483647; default one week)."
    },
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
    observe_acl_is_allowed/2,
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
            parent = admin_import_group,
            label = ?__("WebSub subscriptions", Context),
            url = {admin_websub, []},
            visiblecheck = {acl, use, mod_websub}
        }
        | Acc
    ].

%% Subscription status is private to editors; browsers may only subscribe.
observe_acl_is_allowed(#acl_is_allowed{
    action = subscribe,
    object = #acl_mqtt{topic = [<<"model">>, <<"websub">>, <<"event">>, <<"rsc">>, Id]}
}, Context) ->
    z_acl:rsc_editable(m_rsc:rid(Id, Context), Context);
observe_acl_is_allowed(#acl_is_allowed{
    object = #acl_mqtt{topic = [<<"model">>, <<"websub">>, <<"event">> | _]}
}, _Context) ->
    false;
observe_acl_is_allowed(#acl_is_allowed{}, _Context) ->
    undefined.

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
    case z_context:get(websub_export_revision, Context) of
        {Id, Revision} ->
            publisher_export(Id, Export#{
                <<"export_revision">> => Revision,
                % Older Zotonic subscribers already prefer the top-level version.
                % The editable resource.version retains its original meaning.
                <<"version">> => Revision
            }, Context);
        _ ->
            publisher_snapshot(Id, Export, Context)
    end.

%% Rebuild after taking the revision snapshot, not before: otherwise an export
%% could attach a new revision to stale content. Content updates and edge-log
%% observers take this same lock. Edges committed while exporting may be included
%% early, but their observer will still publish a later revision.
publisher_snapshot(Id, Export, Context) ->
    z_db:transaction(fun(Ctx) ->
        case z_db:q1("select is_authoritative from rsc where id = $1 for update", [Id], Ctx) of
            true ->
                Revision = m_websub:export_revision(Id, Ctx),
                z_depcache:flush(Id, Ctx),
                SnapshotContext = z_context:set(websub_export_revision, {Id, Revision}, Ctx),
                case m_rsc_export:full(Id, SnapshotContext) of
                    {ok, Snapshot} -> Snapshot;
                    {error, _} -> Export
                end;
            _ ->
                Export
        end
    end, Context).

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
