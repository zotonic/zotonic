%% @doc Controller for handling WebSub requests
%% @copyright 2021 Marc Worrell
%% @author Marc Worrell <marc@worrell.nl>

%% Copyright 2021 Marc Worrell
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
    content_types_accepted/1,
    process/4
    ]).

-export([
    topic_id/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

% Max lease seconds for a subscription is 90 days.
-define(MAX_LEASE_SECONDS, 90*24*60*60).


allowed_methods(Context) ->
    {<<"POST">>, Context}.

malformed_request(Context) ->
    Context1 = z_context:ensure_qs(Context),
    HubCallback = z_context:get_q(<<"hub.callback">>, Context1),
    HubMode = z_context:get_q(<<"hub.mode">>, Context1),
    HubTopic = z_context:get_q(<<"hub.topic">>, Context1),
    OptHubSecret = z_context:get_q(<<"hub.secret">>, Context1),
    OptHubLease = z_context:get_q(<<"hub.lease_seconds">>, Context1),
    IsValid = is_url(HubCallback)
            andalso (HubMode =:= <<"subscribe">> orelse HubMode =:= <<"unsubscribe">>)
            andalso is_valid_topic(HubTopic, Context1)
            andalso (OptHubLease =:= undefined orelse z_utils:only_digits(OptHubLease))
            andalso (OptHubSecret =:= undefined orelse size(OptHubSecret) =< 200),
    {IsValid, Context1}.

content_types_accepted(Context) ->
    {[ {<<"application">>, <<"x-www-form-urlencoded">>, []} ], Context}.

process(<<"POST">>, _AcceptedCT, _ProvidedCT, Context) ->
    HubCallback = z_context:get_q(<<"hub.callback">>, Context),
    HubMode = z_context:get_q(<<"hub.mode">>, Context),
    HubTopic = z_context:get_q(<<"hub.topic">>, Context),
    OptHubSecret = z_context:get_q(<<"hub.secret">>, Context),
    handle(HubMode, HubCallback, HubTopic, OptHubSecret, Context).


handle(<<"subscribe">>, HubCallback, HubTopic, OptHubSecret, Context) ->
    case z_acl:is_allowed(use, mod_websub, Context) of
        false ->
            refused(HubCallback, HubTopic, OptHubSecret, <<"access-denied-websub">>);
        true ->
            case topic_id(HubTopic, Context) of
                undefined ->
                    refused(HubCallback, HubTopic, OptHubSecret, <<"invalid-topic">>);
                RscId ->
                    case z_acl:rsc_visible(RscId, Context) of
                        false ->
                            refused(HubCallback, HubTopic, OptHubSecret, <<"access-denied-rsc">>);
                        true ->
                            % Verify intent
                            subscribe(HubCallback, HubTopic, OptHubSecret, RscId, Context)
                    end
            end
    end,
    {true, Context};
handle(<<"unsubscribe">>, HubCallback, HubTopic, OptHubSecret, Context) ->
    unsubscribe(HubCallback, HubTopic, OptHubSecret, Context),
    {true, Context}.


%% @doc Verify intent with the callback and add a subscription
subscribe(HubCallback, HubTopic, OptHubSecret, RscId, Context) ->
    % Verify intent
    LeaseSecs = case z_convert:to_integer(z_context:get_q(<<"hub.lease_seconds">>, Context)) of
        undefined -> ?MAX_LEASE_SECONDS;
        Secs -> erlang:max(0, erlang:min(Secs, ?MAX_LEASE_SECONDS))
    end,
    Challenge = z_ids:id(),
    Payload = [
        {"hub.mode", "subscribe"},
        {"hub.topic", HubTopic},
        {"hub.challenge", Challenge},
        {"hub.lease_seconds", integer_to_list(LeaseSecs)}
    ],
    case post_callback(HubCallback, OptHubSecret, Payload) of
        {ok, {Status, Challenge}} when Status >= 200, Status =< 299 ->
            % Add subscription (overwriting existing subscription)
            % TODO
            ok;
        {ok, {Status, OtherChallenge}} when Status >= 200, Status =< 299 ->
            lager:warning("WebSub Callback returned unexpected challenge ~p expected ~p (~s)",
                        [ OtherChallenge, Challenge, HubCallback ]),
            {error, denied};
        Other ->
            lager:warning("WebSub Callback returned non 2xx ~p (~s)",
                          [ Other, HubCallback ]),
            {error, denied}
    end.


%% @doc Verify intent with the callback and remove the subscription
unsubscribe(HubCallback, HubTopic, OptHubSecret, Context) ->
    %% 1. Check if we have a subscription
    %% 2. Verify intent
    Challenge = z_ids:id(),
    Payload = [
        {"hub.mode", "unsubscribe"},
        {"hub.topic", HubTopic},
        {"hub.challenge", Challenge}
    ],
    case post_callback(HubCallback, OptHubSecret, Payload) of
        {ok, {Status, Challenge}} when Status >= 200, Status =< 299 ->
            % Delete subscription
            % TODO
            ok;
        {ok, {Status, OtherChallenge}} when Status >= 200, Status =< 299 ->
            lager:warning("WebSub Callback returned unexpected challenge ~p expected ~p (~s)",
                        [ OtherChallenge, Challenge, HubCallback ]),
            {error, denied};
        Other ->
            lager:warning("WebSub Callback returned non 2xx ~p (~s)",
                          [ Other, HubCallback ]),
            {error, denied}
    end.


%% @doc Tell the subscriber the subscription has been refused
refused(HubCallback, HubTopic, OptHubSecret, Reason) ->
    Payload = [
        {"hub.mode", "denied"},
        {"hub.topic", HubTopic},
        {"hub.reason", Reason}
    ],
    post_callback(HubCallback, OptHubSecret, Payload).


%% @doc Post a result to the callback, return the status code or an error.
-spec post_callback( binary() | string(), binary(), list() ) -> {ok, {integer(), binary()}} | {error, term()}.
post_callback(HubCallback, OptHubSecret, Payload) ->
    Body = iolist_to_binary( uri_string:compose_query(Payload) ),
    HttpOptions = [
        {timeout, 10000},
        {autoredirect, false}
    ],
    Options = [
        {body_format, binary}
    ],
    Headers = case OptHubSecret of
        undefined ->
            [];
        <<>> ->
            [];
        Key when is_binary(Key) ->
            Hmac = crypto:mac(hmac, sha, Key, Body),
            HmacHex = z_string:to_lower( iolist_to_binary([ z_utils:hex_encode(Hmac)]) ),
            XHubSig = <<"sha1=", HmacHex/binary>>,
            [
                {"X-Hub-Signature", z_convert:to_list(XHubSig)}
            ]
    end,
    Request = {HubCallback, Headers, "application/x-www-form-urlencoded", Body},
    case httpc:request(post, Request, HttpOptions, Options) of
        {ok, {{_, Status, _}, _Hs, _Body}} ->
            {ok, {Status, Body}};
        {ok, {Status, _Body}} ->
            {ok, {Status, Body}};
        {error, Reason} = Error ->
            lager:error("WebSub error ~p posting to callback: ~s", [ Reason, HubCallback ]),
            Error
    end.

% Check if the URL is for the current site and has a known resource id in the url
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
            site := Site,
            controller_options := Options,
            bindings := Bindings
        }} ->
            Id = maps:get(id, Bindings, proplists:get_value(id, Options)),
            m_rsc:rid(Id, Context);
        _ ->
            % Non matching sites and illegal urls are rejected
            lager:info("WebSub for non matching URL: ~s", [ Topic ]),
            undefined
    end.

is_url(<<"http://", _/binary>>) -> true;
is_url(<<"https://", _/binary>>) -> true;
is_url(_) -> false.
