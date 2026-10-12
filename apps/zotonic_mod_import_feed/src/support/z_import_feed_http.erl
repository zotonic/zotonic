%% @copyright 2026 Marc Worrell
%% @doc Bounded fetches with optional host-matched OAuth2 credentials.
%% Development sites allow local destinations and self-signed certificates.

%% Copyright 2026 Marc Worrell
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

-module(z_import_feed_http).
-export([get/2, get/3, get_json/2, url/1, resolve/2, context/2]).

-define(MAX_URL_LENGTH, 2048).
-define(MAX_RESPONSE_BYTES, 4 * 1024 * 1024).
-define(MAX_REDIRECTS, 5).
-define(FETCH_TIMEOUT, 15000).

%% Saved feeds always fetch with their configured account, including previews.
%% Only discovery (before a feed/account is configured) uses the current user.
%% Keep the caller's resource ACLs separate from credential selection.
-spec context(map(), z:context()) -> z:context().
context(Settings, Context) ->
    User = case Settings of
        #{user_id := ConfiguredUser} -> ConfiguredUser;
        #{id := _} -> undefined;
        _ -> z_acl:user(Context)
    end,
    z_context:set(import_feed_use_credentials, maps:get(use_credentials, Settings, false),
        z_context:set(import_feed_fetch_user, User, Context)).

development(undefined) -> false;
development(Context) -> m_site:environment(Context) =:= development.

credentials(_, undefined) -> [];
credentials(<<"https:", _/binary>> = Url, Context) ->
    case z_context:get(import_feed_use_credentials, Context) =:= true
        andalso z_module_manager:active(mod_oauth2, Context)
    of
        true ->
            #{host := Host} = Parts = uri_string:parse(Url),
            HostPort = case maps:find(port, Parts) of
                {ok, Port} -> <<Host/binary, $:, (integer_to_binary(Port))/binary>>;
                error -> Host
            end,
            case z_context:get(import_feed_fetch_user, Context) of
                User when is_integer(User) ->
                    case m_oauth2_consumer:find_token(User, HostPort, Context) of
                        {ok, Token} -> [{authorization, <<"Bearer ", Token/binary>>}];
                        _ -> []
                    end;
                _ -> []
            end;
        false -> []
    end;
credentials(_, _) -> [].

-spec url(Value) -> boolean() when Value :: term().
url(Value) when is_binary(Value), byte_size(Value) > 0, byte_size(Value) =< ?MAX_URL_LENGTH ->
    try uri_string:parse(Value) of
        #{scheme := Scheme, host := Host} = Parts ->
            (Scheme =:= <<"http">> orelse Scheme =:= <<"https">>) andalso Host =/= <<>>
                andalso not maps:is_key(userinfo, Parts);
        _ -> false
    catch _:_ -> false end;
url(_) -> false.

-spec resolve(Value, Base) -> binary() when Value :: term(), Base :: binary().
resolve(Value, Base) when is_binary(Value), Value =/= <<>> ->
    try uri_string:resolve(Value, Base) of
        ResolvedUrl when is_binary(ResolvedUrl) -> case url(ResolvedUrl) of true -> ResolvedUrl; false -> <<>> end;
        _ -> <<>>
    catch _:_ -> <<>> end;
resolve(_, _) -> <<>>.

-spec get(Url, Context) -> {ok, tuple()} | {error, term()} when
    Url :: binary(), Context :: z:context() | undefined.
get(Url, Context) -> get(Url, ?MAX_RESPONSE_BYTES, Context).

-spec get(Url, Limit, Context) -> {ok, tuple()} | {error, term()} when
    Url :: binary(), Limit :: pos_integer(), Context :: z:context() | undefined.
get(Url, Limit, Context) -> get(Url, Limit, 0, [], Context).

-spec get_json(binary(), z:context() | undefined) -> {ok, tuple()} | {error, term()}.
get_json(Url, Context) -> get(Url, ?MAX_RESPONSE_BYTES, 0, [{accept, "application/json"}], Context).

get(_, _, ?MAX_REDIRECTS, _, _) -> {error, too_many_redirects};
get(Url, Limit, RedirectCount, Extra, Context) ->
    IsDevelopment = development(Context),
    case url(Url) andalso (IsDevelopment orelse public_destination(Url)) of
        true ->
            Options = [{autoredirect, false}, {insecure, IsDevelopment},
                {timeout, ?FETCH_TIMEOUT}, {max_length, Limit} | credentials(Url, Context) ++ Extra],
            %% Resolve credentials afresh for every redirect; never forward a token.
            %% No context prevents unrelated fetch observers from adding credentials.
            case z_fetch:fetch(Url, Options, undefined) of
                {ok, {_, _, Length, Body}} when Length >= Limit; byte_size(Body) >= Limit ->
                    {error, response_too_large};
                {ok, _} = Response -> Response;
                {error, {Code, _, Headers, _, _}} when Code =:= 301; Code =:= 302;
                        Code =:= 303; Code =:= 307; Code =:= 308 ->
                    Next = resolve(z_convert:to_binary(proplists:get_value("location", Headers)), Url),
                    case {Url, Next} of
                        {<<"https:", _/binary>>, <<"http:", _/binary>>} when not IsDevelopment -> {error, insecure_redirect};
                        _ -> get(Next, Limit, RedirectCount + 1, Extra, Context)
                    end;
                {error, {Code, _, _, _, _}} -> {error, {http_status, Code}};
                {error, _} -> {error, fetch_failed}
            end;
        false -> {error, unsafe_destination}
    end.

%% As with other outbound Zotonic clients, DNS preflight cannot prevent DNS
%% rebinding between validation and connection. Address pinning belongs in z_fetch.
public_destination(Url) ->
    case url(Url) of
        false -> false;
        true ->
            #{host := Host} = uri_string:parse(Url),
            Name = string:trim(binary_to_list(Host), both, "[]"),
            IpAddresses = case inet:parse_address(Name) of
                {ok, IpAddress} -> [IpAddress];
                _ -> lists:append([addresses(Name, Family) || Family <- [inet, inet6]])
            end,
            IpAddresses =/= [] andalso lists:all(fun z_ip_address:is_public/1, IpAddresses)
    end.

addresses(Name, Family) ->
    case inet:getaddrs(Name, Family) of {ok, IpAddresses} -> IpAddresses; _ -> [] end.
