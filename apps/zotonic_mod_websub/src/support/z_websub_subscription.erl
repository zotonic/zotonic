%% @copyright 2026 Marc Worrell
%% @doc Durable subscriber intent and lease management. All network work runs in
%% the site's unique queue worker. Tokens and secrets never leave this boundary.
-module(z_websub_subscription).
-moduledoc("Persist desired subscription state separately from hub verification.").
-export([start/2, stop/2, stop_import/2, status/2, process/1, verify/5, denied/4,
         callback/2, install/1, renewal_seconds/1]).
-include_lib("zotonic_core/include/zotonic.hrl").

-spec start(Id, Context) -> ok | {error, term()} when
    Id :: integer(),
    Context :: z:context().
start(Id, Context) ->
    z_db:transaction(fun(Ctx) ->
        % Recheck eligibility and source identity after acquiring the resource lock.
        z_db:q1("select id from rsc where id = $1 for update", [Id], Ctx),
        case eligible(Id, Ctx) of
            false ->
                {error, eacces};
            true ->
                Uri = m_rsc:p_no_acl(Id, uri, Ctx),
                case z_db:q_row("select id, source_uri, is_enabled from websub_import
                        where local_rsc_id = $1 order by id desc limit 1", [Id], Ctx) of
                    {_, Uri, true} ->
                        ok;
                    {OldId, _, _} ->
                        stop_import(OldId, Ctx),
                        new_subscription(Id, Uri, OldId, Ctx);
                    undefined ->
                        new_subscription(Id, Uri, undefined, Ctx)
                end
        end
    end, Context).

new_subscription(Id, Uri, ReplacesId, Context) ->
    Token = token(),
    {ok, _} = z_db:insert(websub_import, #{
        local_rsc_id => Id,
        user_id => z_acl:user(Context),
        topic_url => Uri,
        source_uri => Uri,
        callback_url => callback(Token, Context),
        callback_token => Token,
        secret => token(),
        replaces_id => ReplacesId,
        is_enabled => true,
        is_unsubscribed => true,
        next_check => calendar:universal_time()
    }, Context),
    ok.

-spec stop(Id, Context) -> ok | {error, term()} when
    Id :: integer(),
    Context :: z:context().
stop(Id, Context) ->
    case z_acl:rsc_editable(Id, Context) of
        false ->
            {error, eacces};
        true ->
            z_db:transaction(fun(Ctx) ->
                z_db:q1("select id from rsc where id = $1 for update", [Id], Ctx),
                Rows = z_db:q("select id from websub_import where local_rsc_id = $1 and is_enabled", [Id], Ctx),
                lists:foreach(
                    fun({ImportId}) -> stop_import(ImportId, Ctx) end,
                    Rows),
                ok
            end, Context)
    end.

%% Internal operation: callers have checked ACL, or are the lifecycle worker.
-spec stop_import(Id, Context) -> ok when
    Id :: integer(),
    Context :: z:context().
stop_import(Id, Context) ->
    z_db:q("update websub_import set is_enabled = false, is_unsubscribed = true,
        pending_mode = 'unsubscribe', pending_until = null, next_check = now(),
        retry_count = 0, modified = now() where id = $1", [Id], Context),
    z_db:q("delete from websub_import_queue where import_id = $1", [Id], Context),
    ok.

-spec status(Id, Context) -> map() | undefined | {error, eacces} when
    Id :: integer(), Context :: z:context().
status(Id, Context) ->
    case z_acl:rsc_editable(Id, Context) of
        false ->
            {error, eacces};
        true ->
            case z_db:assoc("select is_enabled, is_unsubscribed, pending_mode, lease,
                    last_received, last_error, credential_error,
                    ((lease > now() and not is_unsubscribed) or exists (
                        select 1 from websub_import old where old.id = websub_import.replaces_id
                        and old.is_enabled and old.lease > now() and not old.is_unsubscribed)) as is_active
                from websub_import where local_rsc_id = $1 order by id desc limit 1", [Id], Context) of
                [Row] ->
                    maps:from_list(Row);
                [] ->
                    undefined
            end
    end.

-spec process(Context) -> ok when
    Context :: z:context().
process(Context) ->
    % No remote cleanup is needed once the confirmed remote lease has expired.
    z_db:q("update websub_import set next_check = null, pending_mode = null,
        pending_until = null where not is_enabled and lease <= now() and next_check is not null", Context),
    % Record an unanswered verification instead of silently retrying forever.
    z_db:q("update websub_import set last_error = 'verification_timeout',
        retry_count = retry_count + 1, pending_until = null
        where next_check <= now() and pending_until <= now()", Context),
    Rows = z_db:assoc("select * from websub_import where next_check <= now()
        order by next_check, id limit 100", Context),
    lists:foreach(fun(Row) ->
        process_row(maps:from_list(Row), Context) end, Rows),
    ok.

process_row(#{is_enabled := true} = Row, Context) ->
    case is_own_resource(maps:get(source_uri, Row), Context)
        orelse is_own_resource(maps:get(topic_url, Row), Context) of
        true ->
            stop_self_subscription(Row, Context);
        false ->
            process_enabled_row(Row, Context)
    end;
process_row(#{hub_url := undefined, id := Id}, Context) ->
    z_db:q("update websub_import set next_check = null, pending_mode = null where id = $1", [Id], Context);
process_row(Row, Context) ->
    request(<<"unsubscribe">>, Row, Context).

process_enabled_row(#{id := Id, local_rsc_id := RscId, user_id := UserId} = Row, Context) ->
    UserContext = case UserId of
        undefined ->
            z_acl:anondo(z_context:new(Context));
        _ ->
            z_acl:logon(UserId, z_context:new(Context))
    end,
    case eligible(RscId, UserContext) andalso m_rsc:p_no_acl(RscId, uri, UserContext) =:= maps:get(source_uri, Row) of
        false ->
            stop_import(Id, Context);
        true ->
            case maps:get(hub_url, Row) of
                undefined ->
                    discover(Row, UserContext, Context);
                _ ->
                    case Row of
                        #{is_unsubscribed := false, pending_mode := undefined} ->
                            discover(replacement(Row, Context), UserContext, Context);
                        _ ->
                            request(<<"subscribe">>, Row, Context)
                    end
            end
    end.

discover(#{id := Id, source_uri := Uri} = Row, UserContext, Context) ->
    case z_websub_discovery:discover(Uri, UserContext) of
        {ok, #{topic := Topic, hubs := Hubs}} ->
            Hub = preferred_hub(Hubs),
            UsesCredentials = uses_credentials(Uri, UserContext),
            z_db:q("update websub_import set hub_url = $2, is_use_credentials = $3, topic_url = $4 where id = $1",
                [Id, Hub, UsesCredentials, Topic], Context),
            request(<<"subscribe">>, Row#{hub_url => Hub, topic_url => Topic}, Context);
        {error, Reason} ->
            retry(Row, Reason, Context)
    end.

%% Keep the old callback live until a new capability URL has been verified.
%% This also avoids changing secrets on an in-flight delivery during renewal.
replacement(#{id := OldId, local_rsc_id := RscId} = Row, Context) ->
    z_db:transaction(fun(Ctx) ->
        z_db:q1("select id from rsc where id = $1 for update", [RscId], Ctx),
        case z_db:q1("select is_enabled from websub_import where id = $1 for update", [OldId], Ctx) of
            true ->
                replacement_locked(Row, Ctx);
            _ ->
                Row
        end
    end, Context).

replacement_locked(#{id := OldId} = Row, Ctx) ->
    Token = token(),
    Props = maps:with([local_rsc_id, user_id, topic_url, source_uri,
        is_use_credentials, last_import_version, last_received_version], Row),
    {ok, NewId} = z_db:insert(websub_import, Props#{
        replaces_id => OldId,
        callback_token => Token,
        callback_url => callback(Token, Ctx),
        secret => token(),
        is_enabled => true,
        is_unsubscribed => true
    }, Ctx),
    z_db:q("update websub_import set next_check = null where id = $1", [OldId], Ctx),
    [NewRow] = z_db:assoc("select * from websub_import where id = $1", [NewId], Ctx),
    maps:from_list(NewRow).

request(<<"subscribe">> = Mode, #{topic_url := Topic} = Row, Context) ->
    case is_own_resource(Topic, Context) of
        true ->
            stop_self_subscription(Row, Context);
        false ->
            request_1(Mode, Row, Context)
    end;
request(Mode, Row, Context) ->
    request_1(Mode, Row, Context).

%% Stop the predecessor too: renewal keeps its callback active until replacement.
%% Lock in the same order as start/stop, and do not affect a newer unrelated row.
stop_self_subscription(#{id := Id, local_rsc_id := RscId} = Row, Context) ->
    z_db:transaction(fun(Ctx) ->
        z_db:q1("select id from rsc where id = $1 for update", [RscId], Ctx),
        case z_db:q1("select is_enabled from websub_import where id = $1 for update", [Id], Ctx) of
            true ->
                stop_import(Id, Ctx),
                case maps:get(replaces_id, Row, undefined) of
                    OldId when is_integer(OldId) ->
                        stop_import(OldId, Ctx);
                    _ ->
                        ok
                end,
                z_db:q("update websub_import set last_error = 'self_subscription' where id = $1", [Id], Ctx),
                ok;
            _ ->
                ok
        end
    end, Context).

request_1(Mode, #{id := Id} = Row0, Context) ->
    Row = case Mode of
        <<"subscribe">> ->
            ensure_callback(Row0, Context);
        <<"unsubscribe">> ->
            Row0
    end,
    % Persist intent before POST: the hub can verify before the POST returns.
    Enabled = Mode =:= <<"subscribe">>,
    Changed = z_db:q("update websub_import set pending_mode = $2,
        pending_until = now() + interval '2 minutes', next_check = now() + interval '2 minutes'
        where id = $1 and is_enabled = $3", [Id, Mode, Enabled], Context),
    case Changed of
        1 ->
            Form0 = [{<<"hub.mode">>, Mode}, {<<"hub.topic">>, maps:get(topic_url, Row)},
                     {<<"hub.callback">>, maps:get(callback_url, Row)}],
            Form = case Mode of
                <<"subscribe">> ->
                    [{<<"hub.lease_seconds">>, <<"864000">>},
                                    {<<"hub.secret">>, maps:get(secret, Row)} | Form0];
                <<"unsubscribe">> ->
                    Form0
            end,
            RequestContext = hub_context(Row, Context),
            case z_websub_http:post_form(maps:get(hub_url, Row), Form, RequestContext) of
                {ok, _} ->
                    ok;
                {error, Reason} ->
                    retry(Row, Reason, Context)
            end;
        _ ->
            ok
    end.

%% Source credentials can authorize its own hub, but never a third-party hub.
hub_context(#{hub_url := Hub, source_uri := Topic, user_id := UserId}, Context) ->
    case is_integer(UserId) andalso origin(Hub) =:= origin(Topic) of
        true ->
            z_acl:logon(UserId, z_context:new(Context));
        false ->
            z_acl:anondo(z_context:new(Context))
    end.

origin(Url) ->
    #{scheme := Scheme, host := Host} = Parts = uri_string:parse(Url),
    Port = maps:get(port, Parts, case Scheme of <<"https">> -> 443; _ -> 80 end),
    {Scheme, Host, Port}.

ensure_callback(#{callback_token := undefined, id := Id} = Row, Context) ->
    Token = token(),
    Callback = callback(Token, Context),
    Secret = token(),
    z_db:q("update websub_import set callback_token = $2, callback_url = $3, secret = $4 where id = $1",
        [Id, Token, Callback, Secret], Context),
    Row#{callback_token => Token, callback_url => Callback, secret => Secret};
ensure_callback(Row, _) ->
    Row.

retry(#{id := Id, retry_count := Count} = Row, Reason, Context) ->
    Delay = erlang:min(3600, 10 * (1 bsl erlang:min(Count, 8))),
    % Do not overwrite a verification which completed while POST was in flight.
    z_db:q("update websub_import set last_error = $2, retry_count = retry_count + 1,
        next_check = now() + (case when lease > now() then
            least($3, greatest(1, extract(epoch from (lease - now())) / 2))
            else $3 end * interval '1 second') where id = $1
        and is_enabled = $4 and (pending_mode is not null or hub_url is null)",
        [Id, error_text(Reason), Delay, maps:get(is_enabled, Row)], Context),
    ok.

-spec verify(Token, Topic, Mode, Lease, Context) -> ok | {error, term()} when
    Token :: binary(), Topic :: binary(), Mode :: binary(), Lease :: integer() | undefined,
    Context :: z:context().
verify(Token, Topic, <<"subscribe">>, Lease, Context) when is_integer(Lease), Lease > 0 ->
    Renew = renewal_seconds(Lease),
    case z_db:q1("update websub_import set is_unsubscribed = false,
        lease = now() + ($3 * interval '1 second'),
        next_check = now() + ($4 * interval '1 second'), pending_mode = null,
        pending_until = null, retry_count = 0, last_error = null, modified = now()
        where callback_token = $1 and topic_url = $2 and is_enabled
        and pending_mode = 'subscribe' and pending_until > now() returning id",
        [Token, Topic, Lease, Renew], Context) of
        Id when is_integer(Id) ->
            % Version zero is a catch-up fetch, including updates missed during a gap.
            case z_db:q1("select replaces_id from websub_import where id = $1", [Id], Context) of
                OldId when is_integer(OldId) ->
                    stop_import(OldId, Context);
                _ ->
                    ok
            end,
            m_websub:queue_import(Id, 0, undefined, Context),
            ok;
        _ ->
            {error, no_intent}
    end;
verify(Token, Topic, <<"unsubscribe">>, _Lease, Context) ->
    case z_db:q1("update websub_import set is_unsubscribed = true,
        lease = null, next_check = null, pending_mode = null, pending_until = null,
        last_error = null, modified = now()
        where callback_token is not distinct from $1 and topic_url = $2 and not is_enabled
        and pending_mode = 'unsubscribe' and pending_until > now() returning id", [Token, Topic], Context) of
        Id when is_integer(Id) ->
            ok;
        _ ->
            {error, no_intent}
    end;
verify(_, _, _, _, _) ->
    {error, no_intent}.

-spec denied(Token, Topic, Reason, Context) -> ok when
    Token :: binary(), Topic :: binary(), Reason :: term(), Context :: z:context().
denied(Token, Topic, Reason, Context) ->
    % A late denial must not revoke a verified lease or a stopped subscription.
    case z_db:q_row("update websub_import set is_enabled = false, is_unsubscribed = true,
        next_check = null, pending_mode = null, pending_until = null,
        last_error = $3, modified = now()
        where callback_token = $1 and topic_url = $2 and is_enabled
        and pending_mode = 'subscribe' and pending_until > now()
        returning id, replaces_id", [Token, Topic, error_text(Reason)], Context) of
        {Id, OldId} ->
            z_db:q("delete from websub_import_queue where import_id = $1", [Id], Context),
            case OldId of
                N when is_integer(N) ->
                    stop_import(N, Context);
                _ ->
                    ok
            end;
        undefined ->
            ok
    end,
    ok.

-spec renewal_seconds(Lease) -> pos_integer() when
    Lease :: pos_integer().
renewal_seconds(Lease) ->
    erlang:max(1, Lease * (70 + rand:uniform(10)) div 100).

-spec callback(Token, Context) -> binary() when
    Token :: binary(),
    Context :: z:context().
callback(Token, Context) ->
    Ctx = z_context:set_language('x-default', Context),
    z_context:abs_url(z_dispatcher:url_for(websub_callback, [{token, Token}], Ctx), Ctx).

token() ->
    binary:encode_hex(crypto:strong_rand_bytes(24)).

eligible(Id, Context) when is_integer(Id) ->
    z_acl:user(Context) =/= undefined andalso z_auth:is_enabled(z_acl:user(Context), Context)
        andalso z_acl:rsc_editable(Id, Context)
        andalso not m_rsc:p_no_acl(Id, is_authoritative, Context)
        andalso z_websub_discovery:is_url(m_rsc:p_no_acl(Id, uri, Context))
        andalso not is_own_resource(m_rsc:p_no_acl(Id, uri, Context), Context);
eligible(_, _) ->
    false.

%% Resolve through the site's dispatcher, including aliases, named /id URLs,
%% and JSON topic URLs. Another Zotonic site on this server is still external.
is_own_resource(Url, Context) when is_binary(Url) ->
    Site = z_context:site(Context),
    Normalized = z_websub_discovery:normalize_url(Url),
    case z_sites_dispatcher:get_site_for_url(Normalized) of
        {ok, Site} ->
            case m_rsc:uri_lookup(Normalized, Context) of
                Id when is_integer(Id) ->
                    m_rsc:p_no_acl(Id, is_authoritative, Context) =:= true;
                _ ->
                    false
            end;
        _ ->
            false
    end;
is_own_resource(_, _) ->
    false.

preferred_hub(Hubs) ->
    case [H || <<"https:", _/binary>> = H <- Hubs] of
        [H | _] ->
            H;
        [] ->
            hd(Hubs)
    end.

uses_credentials(Url, Context) ->
    #{host := Host} = Parts = uri_string:parse(Url),
    HostPort = case maps:find(port, Parts) of
        {ok, Port} ->
            <<Host/binary, $:, (integer_to_binary(Port))/binary>>;
        error ->
            Host
    end,
    case z_notifier:first(#url_fetch_options{method = get, url = Url, host = HostPort, options = []}, Context) of
        Options when is_list(Options) ->
            proplists:is_defined(authorization, Options);
        _ ->
            false
    end.

error_text(Reason) ->
    z_string:truncate(z_convert:to_binary(io_lib:format("~p", [Reason])), 200).

-spec install(Context) -> ok when
    Context :: z:context().
install(Context) ->
    z_db:q("alter table websub_export add column if not exists auth_user_groups bytea", Context),
    z_db:q("create table if not exists websub_request_limit (
        id integer primary key, window_start timestamptz not null default now(), requests integer not null default 0)", Context),
    z_db:q("insert into websub_request_limit (id) values (1) on conflict do nothing", Context),
    IsUpgrade = not z_db:column_exists(websub_import, is_enabled, Context),
    z_db:q("alter table websub_import
        add column if not exists source_uri varchar(500),
        add column if not exists is_enabled boolean not null default true,
        add column if not exists callback_token varchar(64),
        add column if not exists replaces_id integer references websub_import(id) on delete set null,
        add column if not exists pending_mode varchar(16),
        add column if not exists pending_until timestamptz,
        add column if not exists next_check timestamptz default now(),
        add column if not exists retry_count integer not null default 0,
        add column if not exists last_error varchar(200)", Context),
    z_db:q("create unique index if not exists websub_import_token_key on websub_import(callback_token)", Context),
    z_db:q("create index if not exists websub_import_due_key on websub_import(next_check)", Context),
    z_db:q("create index if not exists websub_import_rsc_key on websub_import(local_rsc_id)", Context),
    case IsUpgrade of
        true ->
            z_db:q("update websub_import set is_enabled = not is_unsubscribed,
            next_check = case when is_unsubscribed then null else now() end", Context);
        false ->
            ok
    end,
    z_db:q("update websub_import set source_uri = topic_url where source_uri is null", Context),
    z_db:flush(Context),
    ok.
