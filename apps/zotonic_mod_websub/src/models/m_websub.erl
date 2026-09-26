%% @copyright 2021-2026 Marc Worrell
%% @author Marc Worrell <marc@worrell.nl>
%% @doc Model for administrating WebSub subscriptions for import and export.
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

-module(m_websub).
-moduledoc(#{
    zotonic_keywords => [
        "reference", "integrator", "model", "export_and_syndication", "api_and_integration", "authorization_and_access_control", "websub"
    ]
}).
-moduledoc("""
Model for WebSub resource subscriptions, delivery queues, and automatic imports.

## Available model API paths

| Method | Path | Result |
| --- | --- | --- |
| `get` | `/subscriptions` | Paginated overview; requires `use mod_admin_config`. Payload: `type` (`all`, `export`, `import`), numeric local `rsc_id`, `hostname`, `status`, `errors` (`yes`/`no`/`all`), and `page`. |
| `get` | `/subscriber_count/+id` | Number of unexpired incoming subscriptions; requires resource edit permission. |
| `get` | `/status/+id` | Latest import subscription status for an editable resource, or `undefined` when none exists. |

`+id` is resolved through `m_rsc:rid/2`. The template equivalent is
`m.websub.status[id]`; the model checks resource edit permission independently of
the template or controller. Unauthorized reads return `eacces`. There are no
public model POST or DELETE paths for starting or stopping subscriptions.

The overview includes retained expired/stopped subscriptions and renewal generations,
50 rows per page. Hostname is an exact, case-insensitive match on the incoming
callback host or outgoing source host, ignoring ports and a trailing DNS dot.
Status accepts `active`, `pending`, `expired`, or `stopped`; empty/`all` means all
states. The errors filter selects rows with or without the table’s error indicator.
All filters apply before pagination. It returns remote hostnames, never callback URLs, tokens, secrets,
or authentication data. Invalid filters return `is_invalid` without broadening the query.

Status includes desired state (`is_enabled`), confirmation state
(`is_unsubscribed`, `is_active`), `pending_mode`, `lease`, `last_received`,
`last_error`, and `credential_error`. During renewal, `is_active` also accounts
for a still-active predecessor callback. Status never exposes callback tokens,
HMAC secrets, or source OAuth2 tokens.

## Erlang API

* `subscribe/2` starts a durable subscription for an editable, non-authoritative
  resource with a remote URI. It requires a logged-in user and is idempotent for
  an already-enabled subscription to that source. Discovery and verification run
  asynchronously; `ok` means intent was recorded, not that the hub confirmed it.
* `unsubscribe/2` requires edit permission, immediately disables automatic imports,
  discards queued updates, and schedules remote unsubscription.
* `topic_url/2` generates the language-independent JSON topic URL. Use
  `m_rsc:uri/2` for the resource's semantic `/id` identity instead.

```erlang
ok = m_websub:subscribe(Id, Context).
ok = m_websub:unsubscribe(Id, Context).
```

Admin postbacks call these checked functions. The remaining exported queue,
verification, export-registration, schema, and maintenance functions are internal
integration APIs. They are not exposed through model paths. In particular,
`update_export/6` and `delete_export/3` are called after the hub has verified intent;
callers must not use them to bypass authorization or callback verification.

## Delivery and import processing

Publisher queues coalesce resource versions, enforce lease expiry, and recheck
current access within the original authentication's group restrictions. Delivery
contains the complete JSON topic representation and a signature when a secret was
supplied. Exhausted retries discard that notification, allowing later updates to
retry delivery for the remaining lease.

Subscriber callbacks identify the subscription by capability token and validate
the signed resource URI. Public imports consume the pushed JSON; credentialed
imports refetch through `z_fetch` using the subscribing user's source credentials.
Before applying content, the import transaction checks current edit permission,
non-authoritative status, source identity, and version ordering. Saved import
options are reused without allowing automatic updates to restart a stopped
subscription. Resource identity is stored separately from the discovered WebSub
`topic_url`, so topic migration does not change the semantic resource identifier.

See `mod_websub` for the two-site protocol flow and `z_websub_subscription` for
renewal, pending intent, callback rotation, and subscriber state persistence.
""").
-behaviour(zotonic_model).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    m_get/3,
    topic_url/2,
    subscribe/2,
    unsubscribe/2,
    verify_push_signature/3,
    update_export/6,
    subscriber_context/3,
    delete_export/3,

    queue_push/3,
    queue_edge_update/2,
    task_import_referred/4,
    queue_import/4,
    handle_push_notification/4,

    process_push_queue/1,
    process_import_queue/1,
    cleanup_deleted_imports/1,
    cleanup/1,

    manage_schema/2,
    install/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(PUSH_BATCH_SIZE, 100).
-define(IMPORT_BATCH_SIZE, 100).
-define(MAX_PUSH_RETRIES, 8).
-define(MAX_FETCH_RETRIES, 8).
-define(ERROR_RETENTION_DAYS, 30).
-define(ERROR_TEXT_MAX, 200).


m_get([<<"subscriptions">> | Rest], #{payload := Filters}, Context) ->
    case z_acl:is_allowed(use, mod_admin_config, Context) of
        true ->
            {ok, {subscriptions(Filters, Context), Rest}};
        false ->
            {error, eacces}
    end;
m_get([<<"subscriber_count">>, Rsc | Rest], _Msg, Context) ->
    Id = m_rsc:rid(Rsc, Context),
    case z_acl:rsc_editable(Id, Context) of
        true ->
            Count = z_db:q1("select count(*) from websub_export where local_rsc_id=$1 and lease > now()", [Id], Context),
            {ok, {Count, Rest}};
        false ->
            {error, eacces}
    end;
m_get([<<"status">>, Rsc | Rest], _Msg, Context) ->
    case z_websub_subscription:status(m_rsc:rid(Rsc, Context), Context) of
        {error, _} = Error ->
            Error;
        Status ->
            {ok, {Status, Rest}}
    end;
m_get(_, _, _) ->
    {error, unknown_path}.

%% Only called after the admin ACL check. Select an explicit safe projection;
%% peer URLs are used internally to extract the hostname and are then discarded.
subscriptions(Filters, Context) when is_map(Filters) ->
    Type = maps:get(<<"type">>, Filters, undefined),
    RscId = positive_integer(maps:get(<<"rsc_id">>, Filters, undefined), undefined),
    Page = positive_integer(maps:get(<<"page">>, Filters, undefined), 1),
    Hostname = subscription_hostname(maps:get(<<"hostname">>, Filters, undefined)),
    Status = subscription_status(maps:get(<<"status">>, Filters, undefined)),
    Errors = subscription_errors(maps:get(<<"errors">>, Filters, undefined)),
    case valid_subscription_type(Type) andalso RscId =/= invalid
            andalso Page =/= invalid andalso Page =< 10000
            andalso Hostname =/= invalid andalso Status =/= invalid andalso Errors =/= invalid of
        true ->
            subscriptions(Type, RscId, Page, Hostname, Status, Errors, Context);
        false ->
            #{is_invalid => true}
    end;
subscriptions(_, _Context) ->
    #{is_invalid => true}.

valid_subscription_type(undefined) ->
    true;
valid_subscription_type(<<>>) ->
    true;
valid_subscription_type(<<"all">>) ->
    true;
valid_subscription_type(<<"export">>) ->
    true;
valid_subscription_type(<<"import">>) ->
    true;
valid_subscription_type(_) ->
    false.

%% Exact, case-insensitive hostname matching, without a scheme, port or path.
%% Accept brackets around IPv6 literals and normalize a DNS trailing dot.
subscription_hostname(undefined) ->
    undefined;
subscription_hostname(B) when is_binary(B), byte_size(B) =< 255 ->
    try
        case string:trim(B) of
            <<>> ->
                undefined;
            Host ->
                case re:run(Host, <<"^[a-zA-Z0-9.:[\\]\\-]+$">>, [{capture, none}]) of
                    match ->
                        case string:lowercase(string:trim(string:trim(Host, both, "[]"), trailing, ".")) of
                            <<>> ->
                                invalid;
                            Normalized ->
                                Normalized
                        end;
                    nomatch ->
                        invalid
                end
        end
    catch _:_ ->
        invalid end;

subscription_hostname(_) ->
    invalid.

subscription_status(undefined) ->
    undefined;
subscription_status(<<>>) ->
    undefined;
subscription_status(<<"all">>) ->
    undefined;
subscription_status(<<"active">> = S) ->
    S;
subscription_status(<<"pending">> = S) ->
    S;
subscription_status(<<"expired">> = S) ->
    S;
subscription_status(<<"stopped">> = S) ->
    S;
subscription_status(_) ->
    invalid.

subscription_errors(undefined) ->
    undefined;
subscription_errors(<<>>) ->
    undefined;
subscription_errors(<<"all">>) ->
    undefined;
subscription_errors(<<"yes">>) ->
    true;
subscription_errors(<<"no">>) ->
    false;
subscription_errors(_) ->
    invalid.

positive_integer(undefined, Default) ->
    Default;
positive_integer(<<>>, Default) ->
    Default;
positive_integer(N, _) when is_integer(N), N > 0, N =< 2147483647 ->
    N;
positive_integer(B, Default) when is_binary(B), byte_size(B) =< 10 ->
    try positive_integer(binary_to_integer(B), Default)
    catch error:badarg ->
        invalid end;
positive_integer(_, _) ->
    invalid.

subscriptions(Type, RscId, Page, Hostname, Status, Errors, Context) ->
    Exports = "select id, 'export'::text as type, local_rsc_id, callback_url as peer_url,
        lease, pushed as last_activity, is_error as has_error,
        case when lease > now() then 'active' else 'expired' end as status
        from websub_export where ($1::integer is null or local_rsc_id=$1)",
    Imports = "select id, 'import'::text as type, local_rsc_id, source_uri as peer_url,
        lease, last_received as last_activity,
        (last_error is not null or credential_error is not null) as has_error,
        case when not is_enabled then 'stopped'
             when not is_unsubscribed and lease > now() then 'active'
             when pending_mode='subscribe' then 'pending'
             when lease <= now() then 'expired' else 'pending' end as status
        from websub_import where ($1::integer is null or local_rsc_id=$1)",
    Query = case Type of
        <<"export">> ->
            Exports;
        <<"import">> ->
            Imports;
        _ ->
            [Exports, " union all ", Imports]
    end,
    Rows = z_db:assoc(iolist_to_binary([
        "select * from (", Query, ") s
        where ($3::text is null or
            rtrim(lower(trim(both '[]' from substring(peer_url from
                '^[A-Za-z][A-Za-z0-9+.-]*://(?:[^/?#]*@)?([[][^]]+[]]|[^:/?#]+)'))), '.') = $3)
        and ($4::text is null or status = $4)
        and ($5::boolean is null or has_error = $5)
        order by id desc, type limit 51 offset $2"
    ]), [RscId, (Page - 1) * 50, Hostname, Status, Errors], Context),
    SafeRows = [subscription_row(maps:from_list(R)) || R <- lists:sublist(Rows, 50)],
    #{rows => SafeRows, page => Page, previous_page => Page - 1,
      next_page => Page + 1, has_next => length(Rows) > 50 andalso Page < 10000,
      type => Type, rsc_id => RscId, hostname => Hostname, status => Status,
      errors => case Errors of true -> <<"yes">>; false -> <<"no">>; _ -> <<"all">> end}.

subscription_row(#{peer_url := Url} = Row) ->
    Host = case catch uri_string:parse(Url) of
        #{host := H} ->
            H;
        _ ->
            undefined
    end,
    (maps:remove(peer_url, Row))#{peer_host => Host}.

%% Stable JSON delivery topic; the exported resource URI remains the semantic /id URI.
-spec topic_url(Id, Context) -> binary() | undefined when
    Id :: integer(),
    Context :: z:context().
topic_url(Id, Context) ->
    Ctx = z_context:set_language('x-default', Context),
    z_context:abs_url(z_dispatcher:url_for(websub_topic, [{id, Id}], Ctx), Ctx).

-spec subscribe(Id, Context) -> ok | {error, term()} when
    Id :: integer(),
    Context :: z:context().
subscribe(Id, Context) ->
    case z_websub_subscription:start(Id, Context) of
        ok ->
            case subscribe_parts_option(Id, Context) of
                true ->
                    ImportId = z_db:q1("select id from websub_import where local_rsc_id = $1 "
                        "and is_enabled order by id desc limit 1", [Id], Context),
                    RefIds = maps:from_list([
                        {m_rsc:uri(PartId, Context), PartId}
                        || PartId <- m_edge:objects(Id, haspart, Context)
                    ]),
                    queue_referred_import(ImportId, RefIds, z_acl:user(Context), Context);
                false ->
                    ok
            end;
        {error, _} = Error ->
            Error
    end.

subscribe_parts_option(Id, Context) ->
    case m_rsc_import:get_import_status(Id, Context) of
        {ok, Status} ->
            proplists:get_value(is_subscribe_haspart, maps:get(<<"options">>, Status, []), false) =:= true;
        _ ->
            false
    end.

-spec unsubscribe(Id, Context) -> ok | {error, term()} when
    Id :: integer(),
    Context :: z:context().
unsubscribe(Id, Context) ->
    z_websub_subscription:stop(Id, Context).


%% @doc Update or insert a subscriber to a topic. The subscription is valid
%% for LeaseSecs seconds and can be deleted afterwards.
update_export(Callback, Topic, RscId, LeaseSecs, OptSecret, Context) ->
    case m_rsc:p_no_acl(RscId, is_authoritative, Context) of
        true ->
            z_db:transaction(fun(Ctx) ->
                % Serialize registration with authority changes and their cleanup.
                case z_db:q1("select is_authoritative from rsc where id = $1 for update", [RscId], Ctx) of
                    true ->
                        update_export_authoritative(Callback, Topic, RscId, LeaseSecs, OptSecret, Ctx);
                    _ ->
                        {error, not_authoritative}
                end
            end, Context);
        _ ->
            {error, not_authoritative}
    end.

update_export_authoritative(Callback, Topic, RscId, LeaseSecs, OptSecret, Context) ->
    UserId = z_acl:user(Context),
    Secret = OptSecret,
    case z_db:q1("
        insert into websub_export
            (local_rsc_id, user_id, callback_url, topic_url, secret, lease, auth_user_groups)
        select $1, $2, $3, $4, $5, now() + ($6 * interval '1 second'), $7
        from rsc where id = $1 and is_authoritative
        on conflict (callback_url, topic_url)
        do update
           set local_rsc_id = excluded.local_rsc_id,
               user_id = excluded.user_id,
               auth_user_groups = excluded.auth_user_groups,
               secret = excluded.secret,
               lease = excluded.lease,
               is_error = false,
               error_at = null,
               error_reason = null,
               error_count = 0,
               modified = now()
        returning id
        ",
        [ RscId, UserId, Callback, Topic, Secret, LeaseSecs, term_to_binary(z_acl:user_groups(Context)) ],
        Context)
    of
        Id when is_integer(Id) ->
            ok;
        _ ->
            {error, update_failed}
    end.

%% @doc Delete a topic subscriber.
delete_export(Callback, Topic, Context) ->
    _ = z_db:q("
        delete from websub_export
        where callback_url = $1
          and topic_url = $2
        ",
        [ Callback, Topic ],
        Context),
    ok.


%% @doc Publish changes to outgoing edges. Called by trusted edge-log observers
%% after the original edge mutation has committed and passed its ACL checks.
%% A new resource version is needed because subscribers reject stale versions.
-spec queue_edge_update(RscId, Context) -> ok when
    RscId :: integer(),
    Context :: z:context().
queue_edge_update(RscId, Context) ->
    Result = z_db:transaction(fun(Ctx) ->
        case z_db:q1("select is_authoritative from rsc where id = $1 for update", [RscId], Ctx) of
            true ->
                {ok, RscId} = m_rsc:touch(RscId, Ctx),
                Version = z_db:q1("select version from rsc where id = $1", [RscId], Ctx),
                queue_push_authoritative(RscId, Version, Ctx);
            _ ->
                ok
        end
    end, Context),
    % Other readers can repopulate the cache while the transaction is open.
    z_depcache:flush(RscId, Context),
    Result.

%% @doc Populate newly referenced placeholders after the parent import commits.
%% Recheck the subscription and editor rights; never inherit pivot-worker rights.
-spec task_import_referred(ImportId, RefIds, UserId, Context) -> ok when
    ImportId :: integer(),
    RefIds :: map(),
    UserId :: integer(),
    Context :: z:context().
task_import_referred(ImportId, RefIds, UserId, Context) ->
    case z_db:q_row("select local_rsc_id, source_uri from websub_import "
            "where id = $1 and is_enabled", [ImportId], Context) of
        {LocalId, SourceUri} ->
            case can_import(LocalId, UserId, Context)
                andalso m_rsc:p_no_acl(LocalId, uri, Context) =:= SourceUri
            of
                true ->
                    UserContext = z_context:set(websub_safe_import, true, user_context(UserId, Context)),
                    ok = m_rsc_import:import_referred_ids_task(
                        RefIds, #{LocalId => true}, saved, UserContext),
                    case z_db:q1("select is_enabled from websub_import where id = $1", [ImportId], Context) of
                        true ->
                            subscribe_imported_parts(LocalId, UserContext);
                        _ ->
                            ok
                    end;
                false ->
                    ok
            end;
        undefined ->
            ok
    end.

%% Insert in the caller's transaction: the public pivot API uses another
%% connection and would expose the task before the parent import commits.
queue_referred_import(_ImportId, RefIds, _UserId, _Context) when map_size(RefIds) =:= 0 ->
    ok;
queue_referred_import(ImportId, RefIds, UserId, Context) ->
    {ok, _} = z_db:insert(pivot_task_queue, #{
        <<"module">> => ?MODULE,
        <<"function">> => task_import_referred,
        <<"key">> => z_ids:id(),
        <<"args">> => [ImportId, RefIds, UserId]
    }, Context),
    ok.

subscribe_imported_parts(LocalId, Context) ->
    case subscribe_parts_option(LocalId, Context) of
        true ->
            lists:foreach(
                fun(PartId) ->
                    case m_rsc_import:is_imported(PartId, Context) of
                        true ->
                            % Each item gets its own ACL-checked subscription.
                            % Do not recursively inherit the collection option.
                            z_websub_subscription:start(PartId, Context);
                        false ->
                            ok
                    end
                end,
                m_edge:objects(LocalId, haspart, Context));
        false ->
            ok
    end.

queue_push(RscId, Version, Context) when is_integer(RscId), is_integer(Version) ->
    z_db:transaction(fun(Ctx) ->
        % Read the locked row, not a cached flag: a delayed update notification
        % must not delete subscriptions created after authority was restored.
        case z_db:q1("select is_authoritative from rsc where id = $1 for update", [RscId], Ctx) of
            true ->
                queue_push_authoritative(RscId, Version, Ctx);
            _ ->
                z_db:q("delete from websub_export where local_rsc_id = $1", [RscId], Ctx),
                ok
        end
    end, Context).

queue_push_authoritative(RscId, Version, Context) ->
    % Fan out a resource change to all active export subscriptions; per-subscription
    % queue rows are later coalesced on version so only the newest push survives.
    Subs = z_db:q("
        select id
        from websub_export
        where local_rsc_id = $1
          and lease > now()
        ",
        [ RscId ],
        Context),
    lists:foreach(
        fun({ExportId}) ->
            queue_push_subscription(ExportId, RscId, Version, Context)
        end,
        Subs),
    ok.

queue_import(ImportId, Version, Payload, Context) when is_integer(ImportId), is_integer(Version) ->
    % Import queue rows are single-row per subscription; newer versions overwrite older
    % work so we never fetch or import content that is already superseded.
    PayloadBin = encode_payload(Payload),
    _ = z_db:q("
        insert into websub_import_queue
            (import_id, version, payload, due)
        values
            ($1, $2, $3, now())
        on conflict (import_id)
        do update
           set version = greatest(websub_import_queue.version, excluded.version),
               payload = case
                    when excluded.version >= websub_import_queue.version then excluded.payload
                    else websub_import_queue.payload
               end,
               due = case
                    when excluded.version > websub_import_queue.version then now()
                    else websub_import_queue.due
               end,
               retry_count = case
                    when excluded.version > websub_import_queue.version then 0
                    else websub_import_queue.retry_count
               end,
               modified = now()
        ",
        [ ImportId, Version, PayloadBin ],
        Context),
    ok.


handle_push_notification(Payload0, RawBody, Signature, Context) ->
    % The capability callback identifies the subscription. The signed resource URI
    % must also match, so an unrelated resource cannot be imported through it.
    Payload = normalize_payload(Payload0),
    case resource_uri(Payload) of
        undefined ->
            {error, missing_uri};
        TopicUrl ->
            case resource_version(Payload) of
                undefined ->
                    {error, missing_version};
                Version ->
                    Imports = z_db:assoc("
                        select *
                        from websub_import
                        where source_uri = $1
                          and is_unsubscribed = false
                          and is_enabled and lease > now()
                          and callback_token = $2
                        ",
                        [ TopicUrl, z_context:get_q(<<"token">>, Context) ],
                        Context),
                    ValidImports = lists:filter(
                        fun(Import) ->
                            verify_push_signature(Signature, proplists:get_value(secret, Import), RawBody)
                        end,
                        Imports),
                    case ValidImports of
                        [] ->
                            {error, no_subscription};
                        _ ->
                            lists:foreach(
                                fun(Import) ->
                                    ImportId = proplists:get_value(id, Import),
                                    case proplists:get_value(local_rsc_id, Import) of
                                        undefined ->
                                            cleanup_deleted_import(Import, Context);
                                        null ->
                                            cleanup_deleted_import(Import, Context);
                                        _ ->
                                            ok
                                    end,
                                    _ = z_db:q("
                                        update websub_import
                                        set last_received = now(),
                                            last_received_version = greatest(last_received_version, $2),
                                            modified = now()
                                        where id = $1
                                        ",
                                        [ ImportId, Version ],
                                        Context),
                                    case proplists:get_value(is_use_credentials, Import) of
                                        true ->
                                            queue_import(ImportId, Version, undefined, Context);
                                        false ->
                                            queue_import(ImportId, Version, Payload, Context)
                                    end
                                end,
                                ValidImports),
                            ok
                    end
            end
    end.


process_push_queue(Context) ->
    % Workers only pick rows that are due; retry scheduling happens by moving `due`
    % forward with backoff rather than spinning in the tick handler.
    Rows = z_db:q("
        select q.id, q.export_id, q.local_rsc_id, q.version, q.retry_count,
               e.callback_url, e.topic_url, e.secret, e.user_id,
               e.last_push_version, (e.lease is null or e.lease <= now()), e.auth_user_groups
        from websub_push_queue q
        join websub_export e on e.id = q.export_id
        where q.due <= now()
        order by q.due asc, q.id asc
        limit $1
        ",
        [ ?PUSH_BATCH_SIZE ],
        Context),
    lists:foreach(fun(Row) ->
        process_push_row(Row, Context) end, Rows),
    ok.

process_import_queue(Context) ->
    % Imports follow the same due-time pattern as pushes so fetch/import retries stay
    % in the database and can survive process restarts.
    Rows = z_db:q("
        select q.id, q.import_id, q.version, q.payload, q.retry_count,
               i.source_uri, i.hub_url, i.secret, i.user_id, i.local_rsc_id,
               i.last_import_version, i.is_use_credentials, (i.is_unsubscribed or not i.is_enabled)
        from websub_import_queue q
        join websub_import i on i.id = q.import_id
        where q.due <= now()
        order by q.due asc, q.id asc
        limit $1
        ",
        [ ?IMPORT_BATCH_SIZE ],
        Context),
    lists:foreach(fun(Row) ->
        process_import_row(Row, Context) end, Rows),
    ok.

cleanup_deleted_imports(Context) ->
    Imports = z_db:assoc("
        select *
        from websub_import
        where local_rsc_id is null
          and is_unsubscribed = false
        ",
        Context),
    lists:foreach(
        fun(Import) ->
            cleanup_deleted_import(Import, Context)
        end,
        Imports),
    ok.

cleanup(Context) ->
    _ = z_db:q("
        delete from websub_export
        where lease <= now()
          and lease < now() - ($1 * interval '1 day')
        ",
        [ ?ERROR_RETENTION_DAYS ],
        Context),
    ok.


process_push_row({QueueId, ExportId, RscId, Version, RetryCount, Callback, Topic, Secret, UserId, LastPushVersion, IsError, AuthGroups}, Context) ->
    % Re-checking visibility and current version here keeps the queue conservative:
    % work is dropped if access disappeared; an older notification sends current content.
    case IsError orelse LastPushVersion >= Version orelse
        z_db:q1("select lease > now() from websub_export where id = $1", [ExportId], Context) =/= true of
        true ->
            delete_push_queue(QueueId, Version, Context);
        false ->
            UserContext = subscriber_context(UserId, AuthGroups, Context),
            case z_acl:rsc_visible(RscId, UserContext)
                andalso m_rsc:p_no_acl(RscId, is_authoritative, UserContext) of
                false ->
                    ?LOG_WARNING(#{
                        in => zotonic_mod_websub,
                        text => <<"WebSub topic unavailable to subscriber, deleting subscription">>,
                        result => error,
                        reason => eacces,
                        export_id => ExportId,
                        rsc_id => RscId,
                        user_id => UserId
                    }),
                    delete_export_subscription(ExportId, Context),
                    delete_push_queue(QueueId, Version, Context);
                true ->
                    case push_payload(RscId, UserContext) of
                        {ok, Payload} ->
                            deliver_push(Callback, Secret, Payload, Topic,
                            QueueId, ExportId, Version, RetryCount, Context);
                        {error, Reason} ->
                            retry_or_flag_push(QueueId, ExportId, Version, RetryCount, Reason, Context)
                    end
            end
    end.

deliver_push(Callback, Secret, Payload, Topic, QueueId, ExportId, Version, RetryCount, Context) ->
    case post_json_callback(Callback, Secret, Payload, Topic, Context) of
        {ok, Status} when Status >= 200, Status < 300 ->
            _ = z_db:q("
                update websub_export
                set last_push_version = greatest(last_push_version, $2),
                    pushed = now(),
                    is_error = false,
                    error_at = null,
                    error_reason = null,
                    error_count = 0,
                    modified = now()
                where id = $1
                ",
                [ ExportId, Version ],
                Context),
            delete_push_queue(QueueId, Version, Context);
        {ok, Status} ->
            retry_or_flag_push(QueueId, ExportId, Version, RetryCount, {http_status, Status}, Context);
        {error, Reason} ->
            retry_or_flag_push(QueueId, ExportId, Version, RetryCount, Reason, Context)
    end.

process_import_row({QueueId, ImportId, Version, PayloadBin, RetryCount, TopicUrl, _HubUrl, _Secret, UserId, LocalRscId, LastImportVersion, IsUseCredentials, IsUnsubscribed}, Context) ->
    % Credential-backed imports always refetch from the source on push so subscribers
    % can see non-anonymous data; anonymous-only imports consume the pushed payload.
    case IsUnsubscribed orelse (Version > 0 andalso LastImportVersion >= Version)
        orelse not can_import(LocalRscId, UserId, Context) of
        true ->
            delete_import_queue(QueueId, Version, Context);
        false ->
            UserContext = user_context(UserId, Context),
            case IsUseCredentials orelse PayloadBin =:= undefined of
                true ->
                    case z_websub_fetch_zotonic:fetch_json(TopicUrl, UserContext) of
                        {ok, FetchedPayload} ->
                            import_payload(ImportId, QueueId, Version, FetchedPayload, Context);
                        {error, eacces} ->
                            mark_import_credentials_error(ImportId, eacces, Context),
                            delete_import_queue(QueueId, Version, Context),
                            z_websub_subscription:stop_import(ImportId, Context);
                        {error, Reason} ->
                            retry_import_queue(QueueId, Version, RetryCount, Reason, Context)
                    end;
                false ->
                    case decode_payload(PayloadBin) of
                        undefined ->
                            retry_import_queue(QueueId, Version, RetryCount, no_payload, Context);
                        PushedPayload ->
                            import_payload(ImportId, QueueId, Version, PushedPayload, Context)
                    end
            end
    end.

import_payload(ImportId, QueueId, QueuedVersion, Payload0, Context) ->
    z_db:transaction(fun(Ctx) ->
        LocalId = z_db:q1("select local_rsc_id from websub_import where id = $1", [ImportId], Ctx),
        z_db:q1("select id from rsc where id = $1 for update", [LocalId], Ctx),
        case z_db:q1("select is_enabled from websub_import where id = $1 for update", [ImportId], Ctx) of
            true ->
                Source = z_db:q1("select source_uri from websub_import where id = $1", [ImportId], Ctx),
                case m_rsc:p_no_acl(LocalId, uri, Ctx) =:= Source
                    andalso resource_uri(normalize_payload(Payload0)) =:= Source of
                    true ->
                        import_payload_locked(ImportId, QueueId, QueuedVersion, Payload0, Ctx);
                    false ->
                        z_websub_subscription:stop_import(ImportId, Ctx),
                        z_db:q("update websub_import set last_error = 'resource_identity_changed' where id = $1",
                            [ImportId], Ctx)
                end;
            _ ->
                delete_import_queue(QueueId, Ctx)
        end
    end, Context).

import_payload_locked(ImportId, QueueId, QueuedVersion, Payload0, Context) ->
    % The payload can come either from a fresh fetch or from the push itself. In both
    % cases, the stored import version is the gate that prevents stale imports.
    Payload = normalize_payload(Payload0),
    Version = case resource_version(Payload) of
        undefined ->
            QueuedVersion;
        V ->
            V
    end,
    case z_db:q1("select max(last_import_version) from websub_import
            where local_rsc_id = (select local_rsc_id from websub_import where id = $1)
            and source_uri = (select source_uri from websub_import where id = $1)", [ImportId], Context) of
        LastVersion when is_integer(LastVersion), LastVersion >= Version ->
            delete_import_queue(QueueId, Version, Context);
        _ ->
            UserId = z_db:q1("select user_id from websub_import where id = $1", [ImportId], Context),
            % Preserve the transaction holding the resource/subscription locks.
            ImportContext = z_acl:logon(UserId, Context),
            LocalId = z_db:q1("select local_rsc_id from websub_import where id = $1 and is_enabled", [ImportId], Context),
            SavedOptions = case m_rsc_import:get_import_status(LocalId, ImportContext) of
                {ok, Status} ->
                    maps:get(<<"options">>, Status, []);
                _ ->
                    []
            end,
            % Never resurrect subscriptions from saved preferences during automatic updates.
            Options = [{is_subscribe, false} | proplists:delete(is_subscribe, SavedOptions)],
            Result = case can_import(LocalId, UserId, Context) of
                true ->
                    m_rsc_import:import(LocalId, Payload, Options, ImportContext);
                false ->
                    {error, eacces}
            end,
            case Result of
                {ok, {LocalRscId, Imported}} ->
                    ok = queue_referred_import(ImportId,
                        maps:remove(resource_uri(Payload), Imported), UserId, Context),
                    _ = z_db:q("
                        update websub_import
                        set local_rsc_id = $2,
                            last_import_version = greatest(last_import_version, $3),
                            credential_error_at = null,
                            credential_error = null,
                            modified = now()
                        where id = $1
                        ",
                        [ ImportId, LocalRscId, Version ],
                        Context),
                    delete_import_queue(QueueId, Version, Context);
                {error, Reason} ->
                    RetryCount = z_db:q1("select retry_count from websub_import_queue where id = $1", [QueueId], Context),
                    retry_import_queue(QueueId, Version, RetryCount, Reason, Context)
            end
    end.


retry_or_flag_push(QueueId, _ExportId, Version, RetryCount, Reason, Context) when RetryCount < ?MAX_PUSH_RETRIES ->
    % Push failures stay on the queue until the retry budget is exhausted.
    Delay = retry_delay_seconds(RetryCount),
    _ = z_db:q("
        update websub_push_queue
        set retry_count = retry_count + 1,
            due = now() + ($2 * interval '1 second'),
            last_error = $3,
            last_error_at = now(),
            modified = now()
        where id = $1 and version <= $4
        ",
        [ QueueId, Delay, error_text(Reason), Version ],
        Context),
    ok;
retry_or_flag_push(QueueId, ExportId, Version, _RetryCount, Reason, Context) ->
    _ = z_db:q("
        update websub_export
        set is_error = true,
            error_at = now(),
            error_reason = $2,
            error_count = error_count + 1,
            modified = now()
        where id = $1
        ",
        [ ExportId, error_text(Reason) ],
        Context),
    delete_push_queue(QueueId, Version, Context).

retry_import_queue(QueueId, Version, RetryCount, Reason, Context) when RetryCount < ?MAX_FETCH_RETRIES ->
    % Import retries use the same persisted backoff strategy as pushes.
    Delay = retry_delay_seconds(RetryCount),
    _ = z_db:q("
        update websub_import_queue
        set retry_count = retry_count + 1,
            due = now() + ($2 * interval '1 second'),
            last_error = $3,
            last_error_at = now(),
            modified = now()
        where id = $1 and version <= $4
        ",
        [ QueueId, Delay, error_text(Reason), Version ],
        Context),
    ok;
retry_import_queue(QueueId, Version, _RetryCount, Reason, Context) ->
    _ = z_db:q("
        update websub_import
        set credential_error = $2,
            modified = now()
        where id = (
            select import_id
            from websub_import_queue
            where id = $1
        )
        ",
        [ QueueId, error_text(Reason) ],
        Context),
    delete_import_queue(QueueId, Version, Context).


%% Send the full representation with the subscriber's currently authorized access.
%% No private notification-only payload is sent to standard WebSub subscribers.
push_payload(RscId, Context) ->
    case m_rsc_export:full(RscId, z_context:set_language('x-default', Context)) of
        {ok, Export} ->
            {ok, #{<<"status">> => <<"ok">>, <<"result">> => Export}};
        Error ->
            Error
    end.

verify_push_signature(_Signature, undefined, _Body) ->
    true;
verify_push_signature(undefined, Secret, _Body) when Secret =/= undefined ->
    false;
verify_push_signature(Signature, Secret, Body) when is_binary(Signature) ->
    case binary:split(Signature, <<"=">>) of
        [Method, Hex] ->
            Algorithm = case Method of
                <<"sha1">> ->
                    sha;
                <<"sha256">> ->
                    sha256;
                <<"sha384">> ->
                    sha384;
                <<"sha512">> ->
                    sha512;
                _ ->
                    undefined
            end,
            try
                Algorithm =/= undefined andalso
                    crypto:hash_equals(crypto:mac(hmac, Algorithm, Secret, Body), binary:decode_hex(Hex))
            catch _:_ ->
                false end;
        _ ->
            false
    end;
verify_push_signature(_, _, _) ->
    false.


post_json_callback(Callback, OptSecret, Payload, Topic, Context0) ->
    Context = callback_context(Context0),
    Body = jsxrecord:encode(Payload),
    Ctx = z_context:set_language('x-default', Context),
    Hub = z_context:abs_url(z_dispatcher:url_for(websub, [], Ctx), Ctx),
    Link = <<"<", Hub/binary, ">; rel=\"hub\", <", Topic/binary, ">; rel=\"self\"">>,
    Headers = [{<<"link">>, Link} | proplists:get_value(headers, signature_headers(OptSecret, Body), [])],
    Options = [{autoredirect, false}, {timeout, 10000}, {max_length, 65536}, {headers, Headers}],
    case z_websub_http:fetch(post, Callback, Body, [{content_type, <<"application/json">>} | Options], Context) of
        {ok, {_FinalUrl, _Hs, _Size, _RespBody}} ->
            {ok, 200};
        {error, {Status, _Url, _Hs, _Size, _RespBody}} ->
            {ok, Status};
        {error, Reason} = Error ->
            ?LOG_WARNING(#{
                in => zotonic_mod_websub,
                text => <<"WebSub push callback failed">>,
                result => error,
                reason => Reason,
                export_callback_failed => true
            }),
            Error
    end.

signature_headers(undefined, _Body) ->
    [];
signature_headers(Secret, Body) ->
    Hmac = crypto:mac(hmac, sha256, Secret, Body),
    HmacHex = z_string:to_lower(iolist_to_binary([ z_utils:hex_encode(Hmac) ])),
    [
        {headers, [{<<"x-hub-signature">>, <<"sha256=", HmacHex/binary>>}]}
    ].

mark_import_credentials_error(ImportId, Reason, Context) ->
    _ = z_db:q("
        update websub_import
        set credential_error_at = now(),
            credential_error = $2,
            modified = now()
        where id = $1
        ",
        [ ImportId, error_text(Reason) ],
        Context),
    ok.

queue_push_subscription(ExportId, RscId, Version, Context) ->
    _ = z_db:q("
        insert into websub_push_queue
            (export_id, local_rsc_id, version, due)
        values
            ($1, $2, $3, now())
        on conflict (export_id)
        do update
           set local_rsc_id = excluded.local_rsc_id,
               version = greatest(websub_push_queue.version, excluded.version),
               due = case
                    when excluded.version > websub_push_queue.version then now()
                    else websub_push_queue.due
               end,
               retry_count = case
                    when excluded.version > websub_push_queue.version then 0
                    else websub_push_queue.retry_count
               end,
               modified = now()
        ",
        [ ExportId, RscId, Version ],
        Context),
    ok.

delete_push_queue(QueueId, Version, Context) ->
    z_db:q("delete from websub_push_queue where id = $1 and version <= $2", [QueueId, Version], Context),
    ok.

delete_import_queue(QueueId, Version, Context) ->
    z_db:q("delete from websub_import_queue where id = $1 and version <= $2", [QueueId, Version], Context),
    ok.

delete_import_queue(QueueId, Context) ->
    _ = z_db:q("delete from websub_import_queue where id = $1", [QueueId], Context),
    ok.

delete_export_subscription(ExportId, Context) ->
    _ = z_db:q("delete from websub_export where id = $1", [ExportId], Context),
    ok.

user_context(undefined, Context) ->
    z_acl:anondo(z_context:new(Context));
user_context(UserId, Context) when is_integer(UserId) ->
    z_acl:logon(UserId, z_context:new(Context)).

%% Recompute current ACL while retaining the original authentication's upper bound.
%% Legacy subscriptions without a bound may only receive anonymous content.
-spec subscriber_context(UserId, Groups, Context) -> z:context() when
    UserId :: integer() | undefined, Groups :: binary() | undefined, Context :: z:context().
subscriber_context(UserId, Groups, Context) when is_integer(UserId), is_binary(Groups) ->
    Ctx = z_context:new(Context),
    case z_auth:is_enabled(UserId, Ctx) of
        true ->
            z_acl:logon(UserId, #{user_groups => binary_to_term(Groups, [safe]), is_read_only => true}, Ctx);
        false ->
            z_acl:anondo(Ctx)
    end;
subscriber_context(_, _, Context) ->
    z_acl:anondo(z_context:new(Context)).

callback_context(Context) ->
    z_acl:anondo(z_context:new(Context)).

cleanup_deleted_import(Import, Context) ->
    z_websub_subscription:stop_import(proplists:get_value(id, Import), Context).

can_import(Id, UserId, Context) when is_integer(Id), is_integer(UserId) ->
    UserContext = user_context(UserId, Context),
    z_auth:is_enabled(UserId, Context) andalso z_acl:rsc_editable(Id, UserContext)
        andalso not m_rsc:p_no_acl(Id, is_authoritative, UserContext);
can_import(_, _, _) ->
    false.

resource_uri(#{ <<"uri">> := Uri }) when is_binary(Uri) ->
    Uri;
resource_uri(#{ <<"result">> := Result }) ->
    resource_uri(Result);
resource_uri(_) ->
    undefined.

resource_version(#{ <<"version">> := Version }) ->
    valid_version(Version);
resource_version(#{ <<"resource">> := Resource }) when is_map(Resource) ->
    valid_version(maps:get(<<"version">>, Resource, undefined));
resource_version(#{ <<"result">> := Result }) ->
    resource_version(Result);
resource_version(_) ->
    undefined.

valid_version(V) when is_integer(V), V > 0, V =< 2147483647 ->
    V;
valid_version(V) when is_binary(V), byte_size(V) > 0, byte_size(V) =< 10 ->
    case z_utils:only_digits(V) of
        true ->
            valid_version(binary_to_integer(V));
        false ->
            undefined
    end;
valid_version(_) ->
    undefined.

normalize_payload(#{ <<"status">> := <<"ok">>, <<"result">> := Result }) ->
    Result;
normalize_payload(Payload) ->
    Payload.

encode_payload(undefined) ->
    undefined;
encode_payload(Payload) ->
    term_to_binary(Payload).

decode_payload(undefined) ->
    undefined;
decode_payload(Bin) when is_binary(Bin) ->
    binary_to_term(Bin).

retry_delay_seconds(RetryCount) ->
    erlang:min(24 * 60 * 60, (RetryCount + 1) * (RetryCount + 1) * 300).

error_text(Reason) ->
    truncate_binary(
        z_convert:to_binary(io_lib:format("~p", [Reason])),
        ?ERROR_TEXT_MAX).

truncate_binary(Bin, Max) when is_binary(Bin), byte_size(Bin) =< Max ->
    Bin;
truncate_binary(Bin, Max) when is_binary(Bin) ->
    binary:part(Bin, 0, Max).


manage_schema(_Version, Context) ->
    install(Context).

install(Context) ->
    ok = install_export(Context),
    ok = install_import(Context),
    ok = install_push_queue(Context),
    ok = install_import_queue(Context),
    z_websub_subscription:install(Context).

install_export(Context) ->
    case z_db:table_exists(websub_export, Context) of
        false ->
            [] = z_db:q("
                create table websub_export (
                    id serial not null,
                    local_rsc_id int not null,
                    user_id int,
                    callback_url character varying(500) not null,
                    topic_url character varying(500) not null,
                    secret character varying(200),
                    lease timestamp with time zone,
                    last_push_version int not null default 0,
                    error_count int not null default 0,
                    error_reason character varying(200),
                    is_error boolean not null default false,
                    error_at timestamp with time zone,
                    created timestamp with time zone not null default now(),
                    modified timestamp with time zone not null default now(),
                    pushed timestamp with time zone,

                    constraint websub_export_pkey primary key (id),
                    constraint websub_export_callback_topic_key unique (callback_url, topic_url),
                    constraint fk_websub_export_rsc_id foreign key (local_rsc_id)
                        references rsc(id)
                        on delete cascade on update cascade,
                    constraint fk_websub_export_user_id foreign key (user_id)
                        references rsc(id)
                        on delete set null on update cascade
                )
                ",
                Context),
            [] = z_db:q("create index websub_export_rsc_id_key on websub_export(local_rsc_id)", Context),
            [] = z_db:q("create index websub_export_error_key on websub_export(is_error, error_at)", Context),
            z_db:flush(Context),
            ok;
        true ->
            ok
    end.

install_import(Context) ->
    case z_db:table_exists(websub_import, Context) of
        false ->
            [] = z_db:q("
                create table websub_import (
                    id serial not null,
                    local_rsc_id int,
                    user_id int,
                    hub_url character varying(500),
                    callback_url character varying(500) not null,
                    topic_url character varying(500) not null,
                    secret character varying(200),
                    lease timestamp with time zone,
                    is_use_credentials boolean not null default false,
                    is_unsubscribed boolean not null default false,
                    last_import_version int not null default 0,
                    last_received_version int not null default 0,
                    last_received timestamp with time zone,
                    credential_error character varying(200),
                    credential_error_at timestamp with time zone,
                    created timestamp with time zone not null default now(),
                    modified timestamp with time zone not null default now(),

                    constraint websub_import_pkey primary key (id),
                    constraint websub_import_callback_topic_key unique (callback_url, topic_url),
                    constraint fk_websub_import_rsc_id foreign key (local_rsc_id)
                        references rsc(id)
                        on delete set null on update cascade,
                    constraint fk_websub_import_user_id foreign key (user_id)
                        references rsc(id)
                        on delete set null on update cascade
                )
                ",
                Context),
            [] = z_db:q("create index websub_import_topic_key on websub_import(topic_url)", Context),
            [] = z_db:q("create index websub_import_unsubscribed_key on websub_import(is_unsubscribed)", Context),
            z_db:flush(Context),
            ok;
        true ->
            ok
    end.

install_push_queue(Context) ->
    case z_db:table_exists(websub_push_queue, Context) of
        false ->
            [] = z_db:q("
                create table websub_push_queue (
                    id serial not null,
                    export_id int not null,
                    local_rsc_id int not null,
                    version int not null,
                    retry_count int not null default 0,
                    last_error character varying(200),
                    last_error_at timestamp with time zone,
                    due timestamp with time zone not null default now(),
                    created timestamp with time zone not null default now(),
                    modified timestamp with time zone not null default now(),

                    constraint websub_push_queue_pkey primary key (id),
                    constraint websub_push_queue_export_key unique (export_id),
                    constraint fk_websub_push_queue_export_id foreign key (export_id)
                        references websub_export(id)
                        on delete cascade on update cascade,
                    constraint fk_websub_push_queue_rsc_id foreign key (local_rsc_id)
                        references rsc(id)
                        on delete cascade on update cascade
                )
                ",
                Context),
            [] = z_db:q("create index websub_push_queue_due_key on websub_push_queue(due)", Context),
            z_db:flush(Context),
            ok;
        true ->
            ok
    end.

install_import_queue(Context) ->
    case z_db:table_exists(websub_import_queue, Context) of
        false ->
            [] = z_db:q("
                create table websub_import_queue (
                    id serial not null,
                    import_id int not null,
                    version int not null,
                    payload bytea,
                    retry_count int not null default 0,
                    last_error character varying(200),
                    last_error_at timestamp with time zone,
                    due timestamp with time zone not null default now(),
                    created timestamp with time zone not null default now(),
                    modified timestamp with time zone not null default now(),

                    constraint websub_import_queue_pkey primary key (id),
                    constraint websub_import_queue_import_key unique (import_id),
                    constraint fk_websub_import_queue_import_id foreign key (import_id)
                        references websub_import(id)
                        on delete cascade on update cascade
                )
                ",
                Context),
            [] = z_db:q("create index websub_import_queue_due_key on websub_import_queue(due)", Context),
            z_db:flush(Context),
            ok;
        true ->
            ok
    end.
