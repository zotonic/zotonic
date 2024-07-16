%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019-2024 Driebit BV
%% @doc Rate limiting of authentication tries and other types of requests
%% This follows https://www.owasp.org/index.php/Slow_Down_Online_Guessing_Attacks_with_Device_Cookies
%% @end

%% Copyright 2019-2024 Driebit BV
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
-mod_depends([ cron ]).

-export([
    event/2,
    observe_auth_precheck/2,
    observe_auth_checked/2,
    observe_auth_logon/3,
    observe_auth_reset/2,
    observe_tick_6h/2,
    init/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

% Length of the secret for signing the device cookie
-define(DEVICE_SECRET_LENGTH, 32).

% Validity period of a device cookie
-define(DEVICE_MAX_AGE, 3600*24*180).

% Name of the cookie storing the device id for rate limiting.
-define(DEVICE_COOKIE, <<"z_rldid">>).


-record(rldid, {
        username :: binary(),
        device_id :: binary()
    }).


event(#postback{ message = {reset_ratelimit, _Args} }, Context) ->
    case z_acl:is_admin(Context) orelse z_acl:is_allowed(use, ?MODULE, Context) of
        true ->
            m_ratelimit:reset(Context),
            z_render:growl(?__("The counters have been reset.", Context), Context);
        false ->
            z_render:growl_error(?__("You are not allowed to do this.", Context), Context)
    end.

%% @doc Setup the mnesia tables for registering the event counters.
init(Context) ->
    m_ratelimit:init(Context).

%% @doc Check if rate limiting applies to this authentication request
observe_auth_precheck( #auth_precheck{ username = Username }, Context ) ->
    DeviceId = device_id(Username, Context),
    case m_ratelimit:is_event_limited(auth, Username, DeviceId, Context) of
        true ->
            z:warning(
                "Ratelimit on auth hit for username '~s' (from ~p)",
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
    DeviceId = device_id(Username, Context),
    m_ratelimit:delete_event(auth, Username, DeviceId, Context),
    erlang:put(ratelimit_event_username, Username).


%% @doc Authentication succeeded, set the device id cookie (if we have a username from auth_checked)
observe_auth_logon( #auth_logon{}, Context, _Context ) ->
    case auth_username(Context) of
        undefined ->
            Context;
        Username ->
            erlang:erase(ratelimit_event_username),
            RId = #rldid{
                username = Username,
                device_id = z_ids:id(16)
            },
            set_device_cookie(RId, Context)
    end.

auth_username(Context) ->
    case erlang:get(ratelimit_event_username) of
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
                "Ratelimit on reset hit for username '~s' (from ~p)",
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


%% @doc Return the device id cookie of the current requestor.
-spec device_id(Username, Context) -> DeviceId | undefined when
    Username :: binary(),
    DeviceId :: binary(),
    Context :: z:context().
device_id(Username, Context) ->
    case validate_device_cookie(Context) of
        {ok, #rldid{ username = Username, device_id = DId }} -> DId;
        {ok, _} -> undefined;
        {error, _} -> undefined
    end.


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
    case z_context:is_request(Context) of
        true ->
            Secret = device_secret(Context),
            ExpTerm = termit:expiring(Term, ?DEVICE_MAX_AGE),
            Cookie = termit:encode_base64(ExpTerm, Secret),
            Options = [
                {max_age, ?DEVICE_MAX_AGE},
                {path, "/"},
                {same_site, strict},
                {http_only, true},
                {secure, true}
            ],
            z_context:set_cookie(?DEVICE_COOKIE, Cookie, Options, Context);
        false ->
            Context
    end.

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

