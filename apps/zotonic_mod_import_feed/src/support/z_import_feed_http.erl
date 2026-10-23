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
-export([get_feed/2, validators/1, get/2, get/3, get_json/2, url/1, resolve/2, context/2, retry_after/2, error_delay/1]).

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

%% Conditional requests are only used for a successfully imported settings version.
-spec get_feed(map(), z:context() | undefined) -> {ok, tuple()} | {not_modified, tuple()} | {error, term()}.
get_feed(Settings, Context) ->
    Url = maps:get(url, Settings),
    Version = maps:get(settings_version, Settings, undefined),
    CanValidate = Version =/= undefined
        andalso Version =:= maps:get(http_settings_version, Settings, undefined)
        andalso not maps:get(refetch_deleted_once, Settings, false),
    Headers = case CanValidate of
        true -> [{Name, Value} || {Name, Key} <- [
                {<<"if-none-match">>, http_etag}, {<<"if-modified-since">>, http_last_modified}],
            Value <- [safe_header(maps:get(Key, Settings, undefined))], Value =/= undefined];
        false -> []
    end,
    case get(Url, ?MAX_RESPONSE_BYTES, [{headers, Headers}], Context) of
        {not_modified, {FinalUrl, _}} = Result when Headers =/= [] ->
            case z_convert:to_binary(FinalUrl) =:= maps:get(http_url, Settings, undefined) of
                true -> Result;
                false -> {error, unexpected_not_modified}
            end;
        {not_modified, _} -> {error, unexpected_not_modified};
        Result -> Result
    end.

%% Persist only bounded validator values, never arbitrary response headers.
-spec validators(list()) -> map().
validators(Headers) ->
    maps:from_list([{Key, safe_header(Value)} || {Name, Value} <- Headers,
        {Header, Key} <- [{<<"etag">>, http_etag}, {<<"last-modified">>, http_last_modified}],
        string:lowercase(z_convert:to_binary(Name)) =:= Header]).

safe_header(undefined) -> undefined;
safe_header(Value) ->
    Bin = z_convert:to_binary(Value),
    case byte_size(Bin) > 0 andalso byte_size(Bin) =< 4096
        andalso binary:match(Bin, [<<"\r">>, <<"\n">>]) =:= nomatch of
        true -> Bin;
        false -> undefined
    end.

-spec get_json(binary(), z:context() | undefined) -> {ok, tuple()} | {error, term()}.
get_json(Url, Context) -> get(Url, ?MAX_RESPONSE_BYTES, [{accept, "application/json"}], Context).

get(Url, Limit, Extra, Context) ->
    case url(Url) of
        true ->
            Options = [
                {timeout, ?FETCH_TIMEOUT},
                {max_length, Limit},
                %% Let stdlib strip credentials when a feed changes origin.
                %% Destination callbacks can explicitly supply new credentials.
                {authorization_redirect, none}
                | fetch_options(Context) ++ Extra
            ],
            try z_fetch:fetch(Url, Options, fetch_context(Context)) of
                {ok, {_, _, Length, Body}} when Length >= Limit; byte_size(Body) >= Limit ->
                    {error, response_too_large};
                {ok, _} = Ok -> Ok;
                {error, {304, FinalUrl, Headers, _, _}} ->
                    case lists:keymember(headers, 1, Extra) of
                        true -> {not_modified, {FinalUrl, Headers}};
                        false -> {error, {http_status, 304}}
                    end;
                {error, {Code, _, Headers, _, _}} when Code =:= 429; Code =:= 503 ->
                    case retry_after(Headers, calendar:universal_time()) of
                        undefined -> {error, {http_status, Code}};
                        Seconds -> {error, {http_status, Code, {retry_after, Seconds}}}
                    end;
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

%% @doc Parse Retry-After delta-seconds or HTTP-date without retaining headers.
-spec retry_after(Headers, Now) -> non_neg_integer() | undefined when
    Headers :: list(), Now :: calendar:datetime().
retry_after(Headers, Now) ->
    Values = [V || {K, V} <- Headers,
        string:lowercase(z_convert:to_binary(K)) =:= <<"retry-after">>],
    case Values of
        [Value] ->
            try
                Bin = string:trim(z_convert:to_binary(Value)),
                Seconds = case re:run(Bin, <<"^[0-9]{1,10}$">>, [{capture, none}]) of
                    match -> binary_to_integer(Bin);
                    nomatch -> max(0, calendar:datetime_to_gregorian_seconds(cow_date:parse_date(Bin))
                        - calendar:datetime_to_gregorian_seconds(Now))
                end,
                case Seconds =< 2147483647 of true -> Seconds; false -> undefined end
            catch _:_ -> undefined end;
        _ -> undefined
    end.

%% @doc Additional delay requested by a throttled/unavailable HTTP endpoint.
-spec error_delay(Reason) -> non_neg_integer() when Reason :: term().
error_delay({http_status, Code, {retry_after, Seconds}})
    when (Code =:= 429 orelse Code =:= 503), is_integer(Seconds), Seconds >= 0 -> Seconds;
error_delay(_) -> 0.
