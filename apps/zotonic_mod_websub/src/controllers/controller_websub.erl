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
-author("Marc Worrell <marc@worrell.nl>").

-export([
    allowed_methods/1,
    malformed_request/1,
    content_types_provided/1,
    content_types_accepted/1,
    process/4
]).

-export([
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
            HubCallback = z_context:get_q(<<"hub.callback">>, Context),
            HubTopic = z_context:get_q(<<"hub.topic">>, Context),
            OptHubSecret = z_context:get_q(<<"hub.secret">>, Context),
            handle(HubMode, HubCallback, HubTopic, OptHubSecret, Context)
    end.


handle_verification(Context) ->
    HubMode = z_context:get_q(<<"hub.mode">>, Context),
    HubTopic = z_context:get_q(<<"hub.topic">>, Context),
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
                    case z_acl:rsc_visible(RscId, Context) of
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
    LeaseSecs = case z_convert:to_integer(z_context:get_q(<<"hub.lease_seconds">>, Context)) of
        undefined -> ?MAX_LEASE_SECONDS;
        Secs -> erlang:max(0, erlang:min(Secs, ?MAX_LEASE_SECONDS))
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
        {ok, {Status, OtherChallenge}} when Status >= 200, Status =< 299 ->
            ?LOG_WARNING(#{
                in => zotonic_mod_websub,
                text => <<"WebSub subscribe callback returned unexpected challenge">>,
                result => error,
                reason => denied,
                challenge_expected => Challenge,
                challenge_received => OtherChallenge,
                callback => HubCallback
            }),
            {error, denied};
        Other ->
            ?LOG_WARNING(#{
                in => zotonic_mod_websub,
                text => <<"WebSub subscribe callback returned non-2xx">>,
                result => error,
                reason => denied,
                response => Other,
                callback => HubCallback
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
        {ok, {Status, OtherChallenge}} when Status >= 200, Status =< 299 ->
            ?LOG_WARNING(#{
                in => zotonic_mod_websub,
                text => <<"WebSub unsubscribe callback returned unexpected challenge">>,
                result => error,
                reason => denied,
                challenge_expected => Challenge,
                challenge_received => OtherChallenge,
                callback => HubCallback
            }),
            {error, denied};
        Other ->
            ?LOG_WARNING(#{
                in => zotonic_mod_websub,
                text => <<"WebSub unsubscribe callback returned non-2xx">>,
                result => error,
                reason => denied,
                response => Other,
                callback => HubCallback
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
    case z_fetch:fetch(get, HubCallback, Payload, [], AnonContext) of
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
                callback => HubCallback
            }),
            Error
    end.


is_valid_subscribe_request(Context) ->
    HubCallback = z_context:get_q(<<"hub.callback">>, Context),
    HubMode = z_context:get_q(<<"hub.mode">>, Context),
    HubTopic = z_context:get_q(<<"hub.topic">>, Context),
    OptHubSecret = z_context:get_q(<<"hub.secret">>, Context),
    OptHubLease = z_context:get_q(<<"hub.lease_seconds">>, Context),
    is_url(HubCallback)
        andalso (HubMode =:= <<"subscribe">> orelse HubMode =:= <<"unsubscribe">>)
        andalso is_valid_topic(HubTopic, Context)
        andalso (OptHubLease =:= undefined orelse z_utils:only_digits(OptHubLease))
        andalso (OptHubSecret =:= undefined orelse size(OptHubSecret) =< 200).

is_valid_callback_verification_request(Context) ->
    HubMode = z_context:get_q(<<"hub.mode">>, Context),
    HubTopic = z_context:get_q(<<"hub.topic">>, Context),
    HubChallenge = z_context:get_q(<<"hub.challenge">>, Context),
    OptHubLease = z_context:get_q(<<"hub.lease_seconds">>, Context),
    (HubMode =:= <<"subscribe">> orelse HubMode =:= <<"unsubscribe">>)
        andalso is_binary(HubChallenge)
        andalso HubChallenge =/= <<>>
        andalso is_url(HubTopic)
        andalso (OptHubLease =:= undefined orelse z_utils:only_digits(OptHubLease)).

is_valid_denied_request(Context) ->
    is_url(z_context:get_q(<<"hub.topic">>, Context)).

verify_callback_intent(HubTopic, Context) ->
    Callback = callback_url(Context),
    Challenge = z_context:get_q(<<"hub.challenge">>, Context),
    case z_db:q1("
        select id
        from websub_import
        where topic_url = $1
          and callback_url = $2
          and is_unsubscribed = false
        ",
        [ HubTopic, Callback ],
        Context)
    of
        Id when is_integer(Id) ->
            Context1 = z_context:set_resp_header(<<"content-type">>, <<"text/plain">>, Context),
            {Challenge, Context1};
        _ ->
            {{halt, 404}, Context}
    end.

handle_denied_callback(HubTopic, Reason, Context) ->
    Callback = callback_url(Context),
    _ = z_db:q("
        update websub_import
        set is_unsubscribed = true,
            modified = now()
        where topic_url = $1
          and callback_url = $2
          and is_unsubscribed = false
        ",
        [ HubTopic, Callback ],
        Context),
    ?LOG_WARNING(#{
        in => zotonic_mod_websub,
        text => <<"WebSub hub denied import subscription">>,
        result => error,
        reason => Reason,
        topic => HubTopic,
        callback => Callback
    }),
    ok.

%% Check if the URL is for the current site and has a known resource id in the url.
is_valid_topic(Topic, Context) ->
    case is_url(Topic) of
        true ->
            case topic_id(Topic, Context) of
                undefined -> false;
                _ -> true
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

is_url(undefined) ->
    false;
is_url(Url) ->
    case uri_string:parse(z_convert:to_binary(Url)) of
        #{ scheme := _Scheme, host := _Host } ->
            true;
        _ ->
            false
    end.

req_body(Context) ->
    case cowmachine_req:req_body(?MAX_BODY_LENGTH, Context) of
        {undefined, Context1} -> {<<>>, Context1};
        {Body, Context1} -> {Body, Context1}
    end.

callback_url(Context) ->
    ContextNoLanguage = z_context:set_language('x-default', Context),
    z_context:abs_url(z_dispatcher:url_for(websub, [], ContextNoLanguage), ContextNoLanguage).
