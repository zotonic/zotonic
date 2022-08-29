%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019 Driebit BV
%% @doc Rate limiting of authentication tries and other types of requests
%%      This follows https://www.owasp.org/index.php/Slow_Down_Online_Guessing_Attacks_with_Device_Cookies

%% Copyright 2019 Driebit BV
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

-module(mod_ratelimit).

-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Rate Limiting").
-mod_description("Rate limiting of authentication tries and other types of requests.").
-mod_prio(500).

-export([
    observe_auth_precheck/2,
    observe_auth_checked/2,
    observe_auth_logon/3,
    observe_auth_reset/2,
    observe_tick_6h/2,
    init/1
]).

-include("zotonic.hrl").

% Length of the secret for signing the device cookie
-define(DEVICE_SECRET_LENGTH, 32).

% Validity period of a device cookie
-define(DEVICE_MAX_AGE, 3600*24*180).

% Name of the cookie storing the device id for rate limiting.
-define(DEVICE_COOKIE, "z_rldid").


-record(rldid, {
        username :: binary(),
        device_id :: binary()
    }).

%% @doc Setup the mnesia tables for registering the event counters.
init(Context) ->
    m_ratelimit:init(Context).

%% @doc Check if rate limiting applies to this authentication request
observe_auth_precheck( #auth_precheck{ username = Username }, Context ) ->
    DeviceId = case validate_device_cookie(Context) of
        {ok, #rldid{ username = Username, device_id = DId }} -> DId;
        {ok, _} -> undefined;
        {error, _} -> undefined
    end,
    case m_ratelimit:is_event_limited(auth, Username, DeviceId, Context) of
        true ->
            z:warning(
                "Rate limit on auth hit for username '~s' (from ~p)",
                [ Username, m_req:get(peer, Context) ],
                [ {module, ?MODULE}, {line, ?LINE} ],
                Context),
            {error, ratelimit};
        false ->
            m_ratelimit:insert_event(auth, Username, DeviceId, Context),
            undefined
    end.

%% @doc Handle the result of the password authentication, register all failures
observe_auth_checked( #auth_checked{ username = _Username, is_accepted = false }, _Context ) ->
    % We already registered a try at the precheck.
    ok;
observe_auth_checked( #auth_checked{ username = Username, is_accepted = true }, Context ) ->
    % Store the authenticated username for later retrieval in observe_auth_logon.
    z_context:set_session(ratelimit_event_username, Username, Context).


%% @doc Authentication succeeded, set the device id cookie (if we have an username from auth_checked)
observe_auth_logon(auth_logon, Context, _Context) ->
    case auth_username(Context) of
        undefined ->
            Context;
        Username ->
            z_context:set_session(ratelimit_event_username, undefined, Context),
            RId = #rldid{
                username = Username,
                device_id = z_ids:id(16)
            },
            set_device_cookie(RId, Context)
    end.

auth_username(Context) ->
    case z_context:get_session(ratelimit_event_username, Context) of
        undefined ->
            m_identity:get_username(Context);
        Username ->
            Username
    end.

%% @doc Auth reset requested, register it against the device cookie
observe_auth_reset(#auth_reset{ username = Username }, Context) ->
    DeviceId = case validate_device_cookie(Context) of
        {ok, #rldid{ username = Username, device_id = DId }} -> DId;
        {ok, _} -> undefined;
        {error, _} -> undefined
    end,
    case m_ratelimit:is_event_limited(reset, Username, DeviceId, Context) of
        true ->
            z:warning(
                "Rate limit on reset hit for username '~s' (from ~p)",
                [ Username, m_req:get(peer, Context) ],
                [ {module, ?MODULE}, {line, ?LINE} ],
                Context),
            {error, ratelimit};
        false ->
            m_ratelimit:insert_event(reset, Username, DeviceId, Context),
            undefined
    end.

%% @doc Prune logged auth events
observe_tick_6h(tick_6h, Context) ->
    m_ratelimit:prune(Context).


%% @doc Validate if the request has a device cookie and if it is valid return the decoded term.
-spec validate_device_cookie( z:context() ) -> {ok, term()} | {error, none|badarg|forged|expired}.
validate_device_cookie(Context) ->
    case z_context:get_cookie(?DEVICE_COOKIE, Context) of
        undefined ->
            {error, none};
        Cookie ->
            CookieB = z_convert:to_binary(Cookie),
            Secret = device_secret(Context),
            case termit:decode_base64(CookieB, Secret) of
                {ok, Term} -> termit:check_expired(Term);
                {error, _} = Error -> Error
            end
    end.

%% @doc Set a device cookie with the given identity/term.
-spec set_device_cookie( term(), z:context() ) -> z:context().
set_device_cookie(Term, Context) ->
    Secret = device_secret(Context),
    ExpTerm = termit:expiring(Term, ?DEVICE_MAX_AGE),
    Cookie = termit:encode_base64(ExpTerm, Secret),
    Options = [
         {max_age, ?DEVICE_MAX_AGE},
         {path, "/"},
         {same_site, strict},
         {http_only, true}
    ],
    Options1 = case z_context:site_protocol(Context) of
        <<"https">> -> [ {secure, true} | Options ];
        _ -> Options
    end,
    z_context:set_cookie(?DEVICE_COOKIE, Cookie, Options1, Context).

-spec device_secret( z:context() ) -> binary().
device_secret(Context) ->
    case m_config:get_value(mod_ratelimit, device_secret, Context) of
        <<>> -> generate_device_secret(Context);
        undefined -> generate_device_secret(Context);
        Secret -> Secret
    end.

generate_device_secret(Context) ->
    Secret = z_ids:id(?DEVICE_SECRET_LENGTH),
    m_config:set_value(mod_ratelimit, device_secret, Secret, Context),
    Secret.

