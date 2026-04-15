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
-author("Marc Worrell <marc@worrell.nl>").

-export([
    update_export/6,
    delete_export/3,

    update_import/6,
    queue_push/3,
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


%% @doc Update or insert a subscriber to a topic. The subscription is valid
%% for LeaseSecs seconds and can be deleted afterwards.
update_export(Callback, Topic, RscId, LeaseSecs, OptSecret, Context) ->
    UserId = z_acl:user(Context),
    Secret = empty_to_undefined(OptSecret),
    case z_db:q1("
        insert into websub_export
            (local_rsc_id, user_id, callback_url, topic_url, secret, lease)
        values
            ($1, $2, $3, $4, $5, now() + ($6 * interval '1 second'))
        on conflict (callback_url, topic_url)
        do update
           set local_rsc_id = excluded.local_rsc_id,
               user_id = excluded.user_id,
               secret = excluded.secret,
               lease = excluded.lease,
               is_error = false,
               error_at = null,
               error_reason = null,
               error_count = 0,
               modified = now()
        returning id
        ",
        [ RscId, UserId, Callback, Topic, Secret, LeaseSecs ],
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


%% @doc Register a subscription to a remote WebSub topic.
update_import(HubUrl, Topic, LocalRscId, LeaseSecs, OptSecret, Context) ->
    UserId = z_acl:user(Context),
    Secret = empty_to_undefined(OptSecret),
    Callback = callback_url(Context),
    UsesCredentials = uses_credentials(UserId, Topic, Context),
    case z_db:q1("
        insert into websub_import
            (local_rsc_id, user_id, hub_url, callback_url, topic_url, secret, lease, is_use_credentials)
        values
            ($1, $2, $3, $4, $5, $6, now() + ($7 * interval '1 second'), $8)
        on conflict (callback_url, topic_url)
        do update
           set local_rsc_id = excluded.local_rsc_id,
               user_id = excluded.user_id,
               hub_url = excluded.hub_url,
               secret = excluded.secret,
               lease = excluded.lease,
               is_use_credentials = excluded.is_use_credentials,
               is_unsubscribed = false,
               credential_error_at = null,
               credential_error = null,
               modified = now()
        returning id
        ",
        [ LocalRscId, UserId, HubUrl, Callback, Topic, Secret, LeaseSecs, UsesCredentials ],
        Context)
    of
        Id when is_integer(Id) ->
            ok;
        _ ->
            {error, update_failed}
    end.


queue_push(RscId, Version, Context) when is_integer(RscId), is_integer(Version) ->
    % Fan out a resource change to all active export subscriptions; per-subscription
    % queue rows are later coalesced on version so only the newest push survives.
    Subs = z_db:q("
        select id
        from websub_export
        where local_rsc_id = $1
          and is_error = false
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
                    else coalesce(websub_import_queue.payload, excluded.payload)
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
    % A single push can target multiple local subscriptions to the same topic. Each
    % subscription decides independently whether to queue a credentialed fetch or use
    % the pushed anonymous payload directly.
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
                        where topic_url = $1
                          and is_unsubscribed = false
                        ",
                        [ TopicUrl ],
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
               e.last_push_version, e.is_error
        from websub_push_queue q
        join websub_export e on e.id = q.export_id
        where q.due <= now()
        order by q.due asc, q.id asc
        limit $1
        ",
        [ ?PUSH_BATCH_SIZE ],
        Context),
    lists:foreach(fun(Row) -> process_push_row(Row, Context) end, Rows),
    ok.

process_import_queue(Context) ->
    % Imports follow the same due-time pattern as pushes so fetch/import retries stay
    % in the database and can survive process restarts.
    Rows = z_db:q("
        select q.id, q.import_id, q.version, q.payload, q.retry_count,
               i.topic_url, i.hub_url, i.secret, i.user_id, i.local_rsc_id,
               i.last_import_version, i.is_use_credentials, i.is_unsubscribed
        from websub_import_queue q
        join websub_import i on i.id = q.import_id
        where q.due <= now()
        order by q.due asc, q.id asc
        limit $1
        ",
        [ ?IMPORT_BATCH_SIZE ],
        Context),
    lists:foreach(fun(Row) -> process_import_row(Row, Context) end, Rows),
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
        where is_error = true
          and error_at < now() - ($1 * interval '1 day')
        ",
        [ ?ERROR_RETENTION_DAYS ],
        Context),
    ok.


process_push_row({QueueId, ExportId, RscId, Version, RetryCount, Callback, _Topic, Secret, UserId, LastPushVersion, IsError}, Context) ->
    % Re-checking visibility and current version here keeps the queue conservative:
    % work is dropped if access disappeared or a newer local version already exists.
    case IsError orelse LastPushVersion >= Version of
        true ->
            delete_push_queue(QueueId, Context);
        false ->
            case current_rsc_version(RscId, Context) of
                Current when is_integer(Current), Current > Version ->
                    delete_push_queue(QueueId, Context);
                _ ->
                    UserContext = user_context(UserId, Context),
                    case z_acl:rsc_visible(RscId, UserContext) of
                        false ->
                            ?LOG_WARNING(#{
                                in => zotonic_mod_websub,
                                text => <<"WebSub subscriber lost access to resource, deleting subscription">>,
                                result => error,
                                reason => eacces,
                                export_id => ExportId,
                                rsc_id => RscId,
                                user_id => UserId
                            }),
                            delete_export_subscription(ExportId, Context),
                            delete_push_queue(QueueId, Context);
                        true ->
                            Payload = push_payload(RscId, Version, Context),
                            case post_json_callback(Callback, Secret, Payload, Context) of
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
                                    delete_push_queue(QueueId, Context);
                                {ok, Status} ->
                                    retry_or_flag_push(QueueId, ExportId, RetryCount, {http_status, Status}, Context);
                                {error, Reason} ->
                                    retry_or_flag_push(QueueId, ExportId, RetryCount, Reason, Context)
                            end
                    end
            end
    end.

process_import_row({QueueId, ImportId, Version, PayloadBin, RetryCount, TopicUrl, HubUrl, Secret, UserId, _LocalRscId, LastImportVersion, IsUseCredentials, IsUnsubscribed}, Context) ->
    % Credential-backed imports always refetch from the source on push so subscribers
    % can see non-anonymous data; anonymous-only imports consume the pushed payload.
    case IsUnsubscribed orelse LastImportVersion >= Version of
        true ->
            delete_import_queue(QueueId, Context);
        false ->
            UserContext = user_context(UserId, Context),
            case IsUseCredentials of
                true ->
                    case z_websub_fetch_zotonic:fetch_json(TopicUrl, UserContext) of
                        {ok, FetchedPayload} ->
                            import_payload(ImportId, QueueId, Version, FetchedPayload, Context);
                        {error, eacces} ->
                            mark_import_credentials_error(ImportId, eacces, Context),
                            delete_import_queue(QueueId, Context),
                            unsubscribe_import(HubUrl, TopicUrl, Secret, Context),
                            _ = z_db:q("
                                update websub_import
                                set is_unsubscribed = true,
                                    modified = now()
                                where id = $1
                                ",
                                [ ImportId ],
                                Context),
                            ok;
                        {error, Reason} ->
                            retry_import_queue(QueueId, RetryCount, Reason, Context)
                    end;
                false ->
                    case decode_payload(PayloadBin) of
                        undefined ->
                            retry_import_queue(QueueId, RetryCount, no_payload, Context);
                        PushedPayload ->
                            import_payload(ImportId, QueueId, Version, PushedPayload, Context)
                    end
            end
    end.

import_payload(ImportId, QueueId, QueuedVersion, Payload0, Context) ->
    % The payload can come either from a fresh fetch or from the push itself. In both
    % cases, the stored import version is the gate that prevents stale imports.
    Payload = normalize_payload(Payload0),
    Version = case resource_version(Payload) of
        undefined -> QueuedVersion;
        V -> V
    end,
    case z_db:q1("select last_import_version from websub_import where id = $1", [ImportId], Context) of
        LastVersion when is_integer(LastVersion), LastVersion >= Version ->
            delete_import_queue(QueueId, Context);
        _ ->
            UserId = z_db:q1("select user_id from websub_import where id = $1", [ImportId], Context),
            ImportContext = user_context(UserId, Context),
            case m_rsc_import:import(Payload, [], ImportContext) of
                {ok, {LocalRscId, _Imported}} ->
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
                    delete_import_queue(QueueId, Context);
                {error, Reason} ->
                    RetryCount = z_db:q1("select retry_count from websub_import_queue where id = $1", [QueueId], Context),
                    retry_import_queue(QueueId, RetryCount, Reason, Context)
            end
    end.


retry_or_flag_push(QueueId, _ExportId, RetryCount, Reason, Context) when RetryCount < ?MAX_PUSH_RETRIES ->
    % Push failures stay on the queue until the retry budget is exhausted.
    Delay = retry_delay_seconds(RetryCount),
    _ = z_db:q("
        update websub_push_queue
        set retry_count = retry_count + 1,
            due = now() + ($2 * interval '1 second'),
            last_error = $3,
            last_error_at = now(),
            modified = now()
        where id = $1
        ",
        [ QueueId, Delay, error_text(Reason) ],
        Context),
    ok;
retry_or_flag_push(QueueId, ExportId, _RetryCount, Reason, Context) ->
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
    delete_push_queue(QueueId, Context).

retry_import_queue(QueueId, RetryCount, Reason, Context) when RetryCount < ?MAX_FETCH_RETRIES ->
    % Import retries use the same persisted backoff strategy as pushes.
    Delay = retry_delay_seconds(RetryCount),
    _ = z_db:q("
        update websub_import_queue
        set retry_count = retry_count + 1,
            due = now() + ($2 * interval '1 second'),
            last_error = $3,
            last_error_at = now(),
            modified = now()
        where id = $1
        ",
        [ QueueId, Delay, error_text(Reason) ],
        Context),
    ok;
retry_import_queue(QueueId, _RetryCount, Reason, Context) ->
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
    delete_import_queue(QueueId, Context).


push_payload(RscId, Version, Context) ->
    % Export through an anonymous context so the pushed body never contains privileged
    % resource data; authenticated subscribers must refetch when they need more.
    AnonContext = z_acl:anondo(Context),
    Uri = m_rsc:uri(RscId, Context),
    case m_rsc_export:full(RscId, AnonContext) of
        {ok, Export} ->
            maps:merge(
                maps:without([ <<"id">>, <<"name">> ], Export),
                #{
                    <<"uri">> => Uri,
                    <<"version">> => Version
                });
        {error, _} ->
            #{
                <<"uri">> => Uri,
                <<"version">> => Version
            }
    end.

verify_push_signature(_Signature, undefined, _Body) ->
    true;
verify_push_signature(undefined, Secret, _Body) when Secret =/= undefined ->
    false;
verify_push_signature(Signature, Secret, Body) ->
    Hmac = crypto:mac(hmac, sha, Secret, Body),
    HmacHex = z_string:to_lower(iolist_to_binary([ z_utils:hex_encode(Hmac) ])),
    Signature =:= <<"sha1=", HmacHex/binary>>.

post_json_callback(Callback, OptSecret, Payload, Context0) ->
    Context = callback_context(Context0),
    Body = jsxrecord:encode(Payload),
    Options = signature_headers(OptSecret, Body),
    case z_fetch:fetch(post, Callback, Body, [{content_type, <<"application/json">>} | Options], Context) of
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
                callback => Callback
            }),
            Error
    end.

signature_headers(undefined, _Body) ->
    [];
signature_headers(<<>>, _Body) ->
    [];
signature_headers(Secret, Body) ->
    Hmac = crypto:mac(hmac, sha, Secret, Body),
    HmacHex = z_string:to_lower(iolist_to_binary([ z_utils:hex_encode(Hmac) ])),
    [
        {headers, [{<<"x-hub-signature">>, <<"sha1=", HmacHex/binary>>}]}
    ].

unsubscribe_import(undefined, _TopicUrl, _Secret, _Context) ->
    ok;
unsubscribe_import(<<>>, _TopicUrl, _Secret, _Context) ->
    ok;
unsubscribe_import(HubUrl, TopicUrl, Secret, Context) ->
    Payload = [
        {<<"hub.mode">>, <<"unsubscribe">>},
        {<<"hub.topic">>, TopicUrl},
        {<<"hub.callback">>, callback_url(Context)}
    ],
    _ = post_form_callback(HubUrl, Secret, Payload, Context),
    ok.

post_form_callback(Url, OptSecret, Payload, Context0) ->
    Context = callback_context(Context0),
    Body = iolist_to_binary(uri_string:compose_query(Payload)),
    Options = signature_headers(OptSecret, Body),
    z_fetch:fetch(post, Url, Payload, Options, Context).

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

delete_push_queue(QueueId, Context) ->
    _ = z_db:q("delete from websub_push_queue where id = $1", [QueueId], Context),
    ok.

delete_import_queue(QueueId, Context) ->
    _ = z_db:q("delete from websub_import_queue where id = $1", [QueueId], Context),
    ok.

delete_export_subscription(ExportId, Context) ->
    _ = z_db:q("delete from websub_export where id = $1", [ExportId], Context),
    ok.

current_rsc_version(RscId, Context) ->
    z_convert:to_integer(m_rsc:p_no_acl(RscId, <<"version">>, Context)).

callback_url(Context) ->
    DefaultContext = z_context:set_language(<<"x-default">>, Context),
    z_context:abs_url(z_dispatcher:url_for(websub, [], DefaultContext), DefaultContext).

user_context(undefined, Context) ->
    z_acl:anondo(z_context:new(Context));
user_context(UserId, Context) when is_integer(UserId) ->
    z_acl:logon(UserId, z_context:new(Context)).

callback_context(Context) ->
    z_acl:anondo(z_context:new(Context)).

cleanup_deleted_import(Import, Context) ->
    % When the mapped local resource is gone, the subscription no longer has anywhere
    % to import into, so proactively unsubscribe the remote hub.
    HubUrl = proplists:get_value(hub_url, Import),
    TopicUrl = proplists:get_value(topic_url, Import),
    Secret = proplists:get_value(secret, Import),
    ImportId = proplists:get_value(id, Import),
    unsubscribe_import(HubUrl, TopicUrl, Secret, Context),
    _ = z_db:q("
        update websub_import
        set is_unsubscribed = true,
            modified = now()
        where id = $1
        ",
        [ ImportId ],
        Context),
    ok.

uses_credentials(undefined, _Url, _Context) ->
    false;
uses_credentials(UserId, Url, Context) ->
    % This mirrors the fetch pipeline by probing the url_fetch_options observers for an
    % authorization header, without storing any credential material ourselves.
    UserContext = user_context(UserId, Context),
    case uri_string:parse(Url) of
        #{ host := Host } = Parts ->
            HostPort = case maps:find(port, Parts) of
                {ok, Port} -> <<Host/binary, $:, (integer_to_binary(Port))/binary>>;
                error -> Host
            end,
            case z_notifier:first(#url_fetch_options{
                    method = get,
                    url = Url,
                    host = HostPort,
                    options = []
                }, UserContext)
            of
                Options when is_list(Options) ->
                    proplists:is_defined(authorization, Options);
                _ ->
                    false
            end;
        _ ->
            false
    end.

resource_uri(#{ <<"uri">> := Uri }) when is_binary(Uri) ->
    Uri;
resource_uri(#{ <<"result">> := Result }) ->
    resource_uri(Result);
resource_uri(_) ->
    undefined.

resource_version(#{ <<"version">> := Version }) ->
    z_convert:to_integer(Version);
resource_version(#{ <<"resource">> := Resource }) when is_map(Resource) ->
    case maps:get(<<"version">>, Resource, undefined) of
        undefined -> undefined;
        V -> z_convert:to_integer(V)
    end;
resource_version(#{ <<"result">> := Result }) ->
    resource_version(Result);
resource_version(_) ->
    undefined.

normalize_payload(#{ <<"status">> := <<"ok">>, <<"result">> := Result }) ->
    Result;
normalize_payload(Payload) ->
    Payload.

empty_to_undefined(undefined) ->
    undefined;
empty_to_undefined(<<>>) ->
    undefined;
empty_to_undefined("") ->
    undefined;
empty_to_undefined(V) ->
    z_convert:to_binary(V).

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
    ok = install_import_queue(Context).

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
