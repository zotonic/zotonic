%% @copyright 2026 Marc Worrell
%% @doc Bounded feed fetches using z_fetch and the configured import account.
%% @end

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

%% Only the fetch gets this context; preview authorization keeps the caller's ACLs.
fetch_context(undefined) -> undefined;
fetch_context(Context) ->
    case z_context:get(import_feed_fetch_user, Context) of
        User when is_integer(User) -> z_acl:logon(User, Context);
        _ -> z_acl:anondo(Context)
    end.

fetch_options(undefined) -> [{authorization, none}];
fetch_options(Context) ->
    case z_context:get(import_feed_use_credentials, Context) of
        true -> [];
        _ -> [{authorization, none}]
    end.

-spec url(Value) -> boolean() when Value :: term().
url(Value) when is_binary(Value), byte_size(Value) > 0, byte_size(Value) =< ?MAX_URL_LENGTH ->
    try uri_string:parse(Value) of
        #{scheme := Scheme, host := Host} = UrlParts ->
            (Scheme =:= <<"http">> orelse Scheme =:= <<"https">>) andalso Host =/= <<>>
                andalso not maps:is_key(userinfo, UrlParts);
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
get(Url, Limit, Context) -> get(Url, Limit, [], Context).

-spec get_json(binary(), z:context() | undefined) -> {ok, tuple()} | {error, term()}.
get_json(Url, Context) -> get(Url, ?MAX_RESPONSE_BYTES, [{accept, "application/json"}], Context).

get(Url, Limit, Extra, Context) ->
    case url(Url) of
        true ->
            Options = [
                {timeout, ?FETCH_TIMEOUT},
                {max_length, Limit}
                | fetch_options(Context) ++ Extra
            ],
            try z_fetch:fetch(Url, Options, fetch_context(Context)) of
                {ok, {_, _, Length, Body}} when Length >= Limit; byte_size(Body) >= Limit ->
                    {error, response_too_large};
                {ok, _} = Ok -> Ok;
                {error, {Code, _, _, _, _}} -> {error, {http_status, Code}};
                {error, _} = Error -> Error
            after
                %% z_acl:logon/2 also updates the process-local memo identity.
                case Context of
                    undefined -> ok;
                    _ -> z_memo:set_userid(Context)
                end
            end;
        false ->
            {error, unsafe_destination}
    end.
