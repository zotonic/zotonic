%% @copyright 2021-2026 Marc Worrell
%% @author Marc Worrell <marc@worrell.nl>
%% @doc Controller for handling WebSub requests.
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

%% This controller follows the spec at https://www.w3.org/TR/websub/

-module(controller_websub).
-moduledoc(#{
    zotonic_keywords => [
        "reference", "integrator", "controller", "export_and_syndication", "api_and_integration", "authorization_and_access_control", "websub", "http"
    ]
}).
-moduledoc("""
HTTP hub and subscriber callback controller for `mod_websub`.

## Endpoints

| Dispatch | Method | Purpose |
| --- | --- | --- |
| `websub` (`/.zotonic/websub`) | POST, form encoded | Request subscription or unsubscription at this site's hub. |
| `websub_callback` (`/.zotonic/websub/:token`) | GET | Confirm pending subscriber intent or receive a denial. |
| `websub_callback` | POST, JSON | Receive a signed resource-export delivery from a remote hub. |

Subscription forms use the standard `hub.mode`, `hub.topic`, `hub.callback`,
optional `hub.secret`, and optional `hub.lease_seconds` fields. The topic must
match a local resource's advertised JSON topic URL. Lease seconds are ignored
for unsubscription. Accepted requests receive 202 after durable queue admission;
overload receives 503. Verification and authorization run independently in
`task_verify/7`, so accepting a request does not activate the subscription.

The hub checks permission to use `mod_websub` and visibility of the resource,
then verifies the callback by GET with a random challenge. It activates or removes
the subscription only after a successful response with the exact challenge body.
Private subscriptions require explicit HTTP authorization; browser cookies alone
are insufficient. The worker retains the authentication's effective group limits.

Subscriber callbacks match the capability token, discovered topic, pending action,
and verification deadline. Valid intent returns the challenge as plain text with
`nosniff`; unexpected intent returns 404. A denial stops the matching subscription.
Delivery accepts resource-export JSON, verifies the configured HMAC signature in
`m_websub:handle_push_notification/4`, and queues import work. Invalid signatures
are ignored locally while the callback can still acknowledge receipt with 2xx.

Outbound callback requests use `z_websub_http` and `z_fetch`, with destination
checks and automatic redirects disabled. Callback verification and delivery do
not use the initiating user's OAuth2 credentials. Request admission is deduplicated
and bounded per site; inbound delivery bodies are limited to 1 MiB.

See `mod_websub` for the complete two-site flow, resource identity versus topic,
OAuth2 integration, and the open DNS connection-pinning TODO.
""").
-author("Marc Worrell <marc@worrell.nl>").

-export([
    allowed_methods/1,
    malformed_request/1,
    content_types_provided/1,
    content_types_accepted/1,
    process/4
]).

-export([
    task_verify/7,
    topic_id/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

% Max lease seconds for a subscription is 90 days.
-define(MAX_LEASE_SECONDS, 90*24*60*60).
% Accept up to 1 MiB for pushed resource payloads.
-define(MAX_BODY_LENGTH, 1024*1024).


allowed_methods(Context) ->
    {[<<"GET">>, <<"POST">>], Context}.

malformed_request(Context) ->
    Context1 = z_context:ensure_qs(Context),
    case cowmachine_req:method(Context1) of
        <<"GET">> ->
            case z_context:get_q(<<"hub.mode">>, Context1) of
                undefined ->
                    {false, Context1};
                <<"subscribe">> ->
                    {not is_valid_callback_verification_request(Context1), Context1};
                <<"unsubscribe">> ->
                    {not is_valid_callback_verification_request(Context1), Context1};
                <<"denied">> ->
                    {not is_valid_denied_request(Context1), Context1};
                _ ->
                    {true, Context1}
            end;
        _ ->
            case z_context:get_q(<<"hub.mode">>, Context1) of
                undefined ->
                    {false, Context1};
                _ ->
                    {not is_valid_subscribe_request(Context1), Context1}
            end
    end.

content_types_provided(Context) ->
    {[
        {<<"text">>, <<"plain">>, []}
    ], Context}.

content_types_accepted(Context) ->
    {[
        {<<"application">>, <<"x-www-form-urlencoded">>, []},
        {<<"application">>, <<"json">>, []}
    ], Context}.

process(<<"GET">>, _AcceptedCT, _ProvidedCT, Context0) ->
    Context = z_context:ensure_qs(Context0),
    handle_verification(Context);
process(<<"POST">>, _AcceptedCT, _ProvidedCT, Context0) ->
    Context = z_context:ensure_qs(Context0),
    case z_context:get_q(<<"hub.mode">>, Context) of
        undefined ->
            handle_push(Context);
        HubMode ->
            HubCallback = z_websub_discovery:normalize_url(z_context:get_q(<<"hub.callback">>, Context)),
            HubTopic = z_websub_discovery:normalize_url(z_context:get_q(<<"hub.topic">>, Context)),
            OptHubSecret = z_context:get_q(<<"hub.secret">>, Context),
            Lease = case HubMode of
                <<"subscribe">> ->
                    z_convert:to_integer(z_context:get_q(<<"hub.lease_seconds">>, Context));
                _ ->
                    undefined
            end,
            % The durable task runs independently of the request's 202 response.
            case enqueue_verification(
                    [HubMode, HubCallback, HubTopic, OptHubSecret, Lease, subscriber_identity(Context)], Context) of
                {ok, _} ->
                    {{halt, 202}, Context};
                {error, _} ->
                    {{halt, 503}, Context}
            end
    end.


%% Browser cookies alone must not authorize private content delivery to a callback.
%% Preserve the effective group restriction established by the authentication module.
subscriber_identity(#context{cowreq = undefined}) ->
    undefined;
subscriber_identity(Context) ->
    % The request may already be logged in via a cookie, in which case the
    % OAuth2 observer normally skips the Authorization header. Authenticate again
    % from anonymous context so a dummy header cannot borrow cookie privileges.
    case cowmachine_req:get_req_header(<<"authorization">>, Context) of
        Header when is_binary(Header), byte_size(Header) > 0 ->
            case z_module_manager:active(mod_oauth2, Context) of
                true ->
                    Fresh = z_acl:anondo(Context),
                    Auth = mod_oauth2:observe_request_context(#request_context{phase = init}, Fresh, Fresh),
                    case z_acl:user(Auth) of
                        Id when is_integer(Id) ->
                            {Id, z_acl:user_groups(Auth)};
                        _ ->
                            undefined
                    end;
                false ->
                    undefined
            end;
        _ ->
            undefined
    end.

%% Bound durable work independently of callback verification or authorization.
%% Serialize admission across nodes; duplicate requests reuse the existing task.
enqueue_verification(Args, Context) ->
    Key = binary:encode_hex(crypto:hash(sha256, term_to_binary(Args))),
    z_db:transaction(fun(Ctx) ->
        z_db:q("select pg_advisory_xact_lock(hashtext(current_schema()), 2736)", Ctx),
        case z_db:q1("select id from pivot_task_queue where module = $1 and function = $2 and key = $3",
                [?MODULE, task_verify, Key], Ctx) of
            Id when is_integer(Id) ->
                {ok, Id};
            _ ->
                Count = z_db:q1("select count(*) from pivot_task_queue where module = $1 and function = $2",
                    [?MODULE, task_verify], Ctx),
                Allowed = z_db:q1("update websub_request_limit set
                    requests = case when window_start <= now() - interval '1 minute' then 1 else requests + 1 end,
                    window_start = case when window_start <= now() - interval '1 minute' then now() else window_start end
                    where id = 1 and (requests < 120 or window_start <= now() - interval '1 minute') returning id", Ctx),
                case Count < 1000 andalso Allowed =:= 1 of
                    true ->
                        z_pivot_rsc:insert_task(?MODULE, task_verify, Key, Args, Ctx);
                    false ->
                        {error, capacity}
                end
        end
    end, Context).


-spec task_verify(Mode, Callback, Topic, Secret, Lease, UserId, Context) -> ok when
    Mode :: binary(), Callback :: binary(), Topic :: binary(), Secret :: binary() | undefined,
    Lease :: integer() | undefined, UserId :: {integer(), list()} | integer() | undefined, Context :: z:context().
task_verify(Mode, Callback, Topic, Secret, Lease, UserId, Context) ->
    UserContext = case UserId of
        {Id, Groups} ->
            m_websub:subscriber_context(Id, term_to_binary(Groups), Context);
        _ ->
            z_acl:anondo(z_context:new(Context))
    end,
    handle(Mode, Callback, Topic, Secret, z_context:set(websub_lease, Lease, UserContext)),
    ok.


handle_verification(Context) ->
    HubMode = z_context:get_q(<<"hub.mode">>, Context),
    HubTopic = z_websub_discovery:normalize_url(z_context:get_q(<<"hub.topic">>, Context)),
    case HubMode of
        <<"subscribe">> ->
            verify_callback_intent(HubTopic, Context);
        <<"unsubscribe">> ->
            verify_callback_intent(HubTopic, Context);
        <<"denied">> ->
            handle_denied_callback(HubTopic, z_context:get_q(<<"hub.reason">>, Context), Context),
            {<<>>, Context};
        _ ->
            {{halt, 400}, Context}
    end.


handle(<<"subscribe">>, HubCallback, HubTopic, OptHubSecret, Context) ->
    case z_acl:is_allowed(use, mod_websub, Context) of
        false ->
            refused(HubCallback, HubTopic, OptHubSecret, <<"access-denied-websub">>, Context);
        true ->
            case topic_id(HubTopic, Context) of
                undefined ->
                    refused(HubCallback, HubTopic, OptHubSecret, <<"invalid-topic">>, Context);
                RscId ->
                    case z_acl:rsc_visible(RscId, Context)
                        andalso m_rsc:p_no_acl(RscId, is_authoritative, Context) of
                        false ->
                            refused(HubCallback, HubTopic, OptHubSecret, <<"access-denied-rsc">>, Context);
                        true ->
                            subscribe(HubCallback, HubTopic, OptHubSecret, RscId, Context)
                    end
            end
    end,
    {true, Context};
handle(<<"unsubscribe">>, HubCallback, HubTopic, OptHubSecret, Context) ->
    unsubscribe(HubCallback, HubTopic, OptHubSecret, Context),
    {true, Context}.

handle_push(Context) ->
    case req_body(Context) of
        {<<>>, Context1} ->
            {true, Context1};
        {Body, Context1} ->
            try z_json:decode(Body) of
                Payload ->
                    Signature = cowmachine_req:get_req_header(<<"x-hub-signature">>, Context1),
                    case m_websub:handle_push_notification(Payload, Body, Signature, Context1) of
                        ok ->
                            {true, Context1};
                        {error, Reason} ->
                            ?LOG_WARNING(#{
                                in => zotonic_mod_websub,
                                text => <<"WebSub push rejected">>,
                                result => error,
                                reason => Reason
                            }),
                            {true, Context1}
                    end
            catch
                error:badarg:Stack ->
                    ?LOG_WARNING(#{
                        in => zotonic_mod_websub,
                        text => <<"WebSub push contains invalid JSON">>,
                        result => error,
                        reason => json,
                        stack => Stack
                    }),
                    {true, Context1}
            end
    end.


%% @doc Verify intent with the callback and add a subscription.
subscribe(HubCallback, HubTopic, OptHubSecret, RscId, Context) ->
    LeaseSecs = case z_context:get(websub_lease, Context) of
        undefined ->
            10*24*60*60;
        Secs ->
            erlang:max(0, erlang:min(Secs, ?MAX_LEASE_SECONDS))
    end,
    Challenge = z_ids:id(),
    Payload = [
        {<<"hub.mode">>, <<"subscribe">>},
        {<<"hub.topic">>, HubTopic},
        {<<"hub.challenge">>, Challenge},
        {<<"hub.lease_seconds">>, integer_to_binary(LeaseSecs)}
    ],
    case get_callback(HubCallback, OptHubSecret, Payload, Context) of
        {ok, {Status, Challenge}} when Status >= 200, Status =< 299 ->
            m_websub:update_export(HubCallback, HubTopic, RscId, LeaseSecs, OptHubSecret, Context);
        {ok, {Status, _OtherChallenge}} when Status >= 200, Status =< 299 ->
            ?LOG_WARNING(#{
                in => zotonic_mod_websub,
                text => <<"WebSub subscribe callback returned unexpected challenge">>,
                result => error,
                reason => denied,
                topic => HubTopic
            }),
            {error, denied};
        _Other ->
            ?LOG_WARNING(#{
                in => zotonic_mod_websub,
                text => <<"WebSub subscribe callback returned non-2xx">>,
                result => error,
                reason => denied,
                topic => HubTopic
            }),
            {error, denied}
    end.


%% @doc Verify intent with the callback and remove the subscription.
unsubscribe(HubCallback, HubTopic, OptHubSecret, Context) ->
    Challenge = z_ids:id(),
    Payload = [
        {<<"hub.mode">>, <<"unsubscribe">>},
        {<<"hub.topic">>, HubTopic},
        {<<"hub.challenge">>, Challenge}
    ],
    case get_callback(HubCallback, OptHubSecret, Payload, Context) of
        {ok, {Status, Challenge}} when Status >= 200, Status =< 299 ->
            m_websub:delete_export(HubCallback, HubTopic, Context);
        {ok, {Status, _OtherChallenge}} when Status >= 200, Status =< 299 ->
            ?LOG_WARNING(#{
                in => zotonic_mod_websub,
                text => <<"WebSub unsubscribe callback returned unexpected challenge">>,
                result => error,
                reason => denied,
                topic => HubTopic
            }),
            {error, denied};
        _Other ->
            ?LOG_WARNING(#{
                in => zotonic_mod_websub,
                text => <<"WebSub unsubscribe callback returned non-2xx">>,
                result => error,
                reason => denied,
                topic => HubTopic
            }),
            {error, denied}
    end.


%% @doc Tell the subscriber the subscription has been refused.
refused(HubCallback, HubTopic, OptHubSecret, Reason, Context) ->
    Payload = [
        {<<"hub.mode">>, <<"denied">>},
        {<<"hub.topic">>, HubTopic},
        {<<"hub.reason">>, Reason}
    ],
    get_callback(HubCallback, OptHubSecret, Payload, Context).


%% @doc Send an intent verification or denial callback, return the status code or an error.
-spec get_callback(binary() | string(), binary() | undefined, list(), z:context()) -> {ok, {integer(), binary()}} | {error, term()}.
get_callback(HubCallback, _OptHubSecret, Payload, Context0) ->
    AnonContext = z_acl:anondo(z_context:new(Context0)),
    case z_websub_http:fetch(get, HubCallback, Payload, [{autoredirect, false}, {timeout, 10000}, {max_length, 4096}], AnonContext) of
        {ok, {_FinalUrl, _Hs, _Size, Body}} ->
            {ok, {200, Body}};
        {error, {Status, _Url, _Hs, _Size, RespBody}} ->
            {ok, {Status, RespBody}};
        {error, Reason} = Error ->
            ?LOG_ERROR(#{
                in => zotonic_mod_websub,
                text => <<"WebSub error fetching callback">>,
                result => error,
                reason => Reason,
                callback_failed => true
            }),
            Error
    end.


is_valid_subscribe_request(Context) ->
    HubCallback = z_websub_discovery:normalize_url(z_context:get_q(<<"hub.callback">>, Context)),
    HubMode = z_context:get_q(<<"hub.mode">>, Context),
    HubTopic = z_websub_discovery:normalize_url(z_context:get_q(<<"hub.topic">>, Context)),
    OptHubSecret = z_context:get_q(<<"hub.secret">>, Context),
    OptHubLease = z_context:get_q(<<"hub.lease_seconds">>, Context),
    is_url(HubCallback)
        andalso (HubMode =:= <<"subscribe">> orelse HubMode =:= <<"unsubscribe">>)
        andalso case HubMode of
            <<"unsubscribe">> ->
                is_url(HubTopic);
            _ ->
                is_valid_topic(HubTopic, Context)
        end
        andalso (HubMode =:= <<"unsubscribe">> orelse OptHubLease =:= undefined orelse valid_lease(OptHubLease))
        andalso (OptHubSecret =:= undefined orelse (is_binary(OptHubSecret) andalso size(OptHubSecret) < 200)).

is_valid_callback_verification_request(Context) ->
    HubMode = z_context:get_q(<<"hub.mode">>, Context),
    HubTopic = z_websub_discovery:normalize_url(z_context:get_q(<<"hub.topic">>, Context)),
    HubChallenge = z_context:get_q(<<"hub.challenge">>, Context),
    OptHubLease = z_context:get_q(<<"hub.lease_seconds">>, Context),
    (HubMode =:= <<"subscribe">> orelse HubMode =:= <<"unsubscribe">>)
        andalso is_binary(HubChallenge)
        andalso byte_size(HubChallenge) > 0 andalso byte_size(HubChallenge) =< 1024
        andalso re:run(HubChallenge, <<"[^+./0-9=A-Z_a-z-]">>, [{capture, none}]) =:= nomatch
        andalso is_url(HubTopic)
        andalso (HubMode =:= <<"unsubscribe">> orelse valid_lease(OptHubLease)).

is_valid_denied_request(Context) ->
    is_url(z_context:get_q(<<"hub.topic">>, Context)).

verify_callback_intent(HubTopic, Context) ->
    Token = z_context:get_q(<<"token">>, Context),
    Mode = z_context:get_q(<<"hub.mode">>, Context),
    Lease = case Mode of
        <<"subscribe">> ->
            z_convert:to_integer(z_context:get_q(<<"hub.lease_seconds">>, Context));
        _ ->
            undefined
    end,
    case z_websub_subscription:verify(Token, HubTopic, Mode, Lease, Context) of
        ok ->
            Context1 = z_context:set_resp_header(<<"content-type">>, <<"text/plain; charset=utf-8">>, Context),
            Context2 = z_context:set_resp_header(<<"x-content-type-options">>, <<"nosniff">>, Context1),
            {z_context:get_q(<<"hub.challenge">>, Context), Context2};
        {error, _} ->
            {{halt, 404}, Context}
    end.

handle_denied_callback(HubTopic, Reason, Context) ->
    z_websub_subscription:denied(z_context:get_q(<<"token">>, Context), HubTopic, Reason, Context).

valid_lease(Value) when is_binary(Value), byte_size(Value) > 0, byte_size(Value) =< 10 ->
    z_utils:only_digits(Value) andalso binary_to_integer(Value) > 0;
valid_lease(_) ->
    false.

%% Check if the URL is for the current site and has a known resource id in the url.
is_valid_topic(Topic0, Context) ->
    Topic = z_websub_discovery:normalize_url(Topic0),
    case is_url(Topic) of
        true ->
            case topic_id(Topic, Context) of
                undefined ->
                    false;
                Id ->
                    m_rsc:p_no_acl(Id, is_authoritative, Context) =:= true
                        andalso Topic =:= m_websub:topic_url(Id, Context)
            end;
        false ->
            false
    end.

topic_id(Topic, Context) ->
    Site = z_context:site(Context),
    case z_sites_dispatcher:dispatch_url(Topic) of
        {ok, #{
            site := DispatchSite,
            controller_options := Options,
            bindings := Bindings
        }} when DispatchSite =:= Site ->
            Id = maps:get(id, Bindings, proplists:get_value(id, Options)),
            m_rsc:rid(Id, Context);
        _ ->
            ?LOG_INFO(#{
                in => zotonic_mod_websub,
                text => <<"WebSub for non matching topic URL">>,
                result => error,
                reason => unknown_url,
                topic_url => Topic
            }),
            undefined
    end.

is_url(Url) ->
    z_websub_discovery:is_url(Url).

req_body(Context) ->
    case cowmachine_req:req_body(?MAX_BODY_LENGTH, Context) of
        {undefined, Context1} ->
            {<<>>, Context1};
        {Body, Context1} ->
            {Body, Context1}
    end.
