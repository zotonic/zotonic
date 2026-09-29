%% @copyright 2026 Marc Worrell
%% @author Marc Worrell <marc@worrell.nl>
%% @doc Outbound WebSub HTTP requests using z_fetch, with public-address checks,
%% explicit redirect handling, and credential removal across origins. Subscriber
%% callbacks additionally require DNS hostnames under Zotonic's local policy.
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

-module(z_websub_http).
-moduledoc("WebSub form requests with explicit redirect and credential policy.").

-export([post_form/3, fetch/5, get/3, destination/1, destination/2, is_callback_url/1, callback/5]).

%% @doc Zotonic local policy: subscriber callbacks must use DNS hostnames.
%% WebSub section 5.1.2 permits local URL policies; IP literals are valid URLs
%% in the protocol itself. Internationalized names must use their ASCII form.
-spec is_callback_url(term()) -> boolean().
is_callback_url(Url) ->
    case z_websub_discovery:is_url(Url) of
        true ->
            #{host := Host} = uri_string:parse(Url),
            Name = case Host of
                <<Prefix:(byte_size(Host)-1)/binary, ".">> -> Prefix;
                _ -> Host
            end,
            Labels = binary:split(Name, <<".">>, [global]),
            byte_size(Name) =< 253
                andalso lists:all(fun is_hostname_label/1, Labels)
                andalso re:run(lists:last(Labels), <<"^[A-Za-z]">>, [{capture, none}]) =:= match;
        false ->
            false
    end.

is_hostname_label(Label) ->
    byte_size(Label) >= 1
        andalso byte_size(Label) =< 63
        andalso re:run(Label, <<"^[A-Za-z0-9](?:[A-Za-z0-9-]*[A-Za-z0-9])?$">>, [{capture, none}]) =:= match.

%% @doc Apply the local callback policy also to queued and stored subscriptions.
%% Public-address checks and redirect restrictions still apply in fetch/5.
-spec callback(atom(), binary(), term(), list(), z:context()) -> term().
callback(Method, Url, Payload, Options, Context) ->
    case is_callback_url(Url) of
        true ->
            ?MODULE:fetch(Method, Url, Payload, Options, Context);
        false ->
            {error, callback_hostname_required}
    end.

-spec post_form(Url, Form, Context) -> {ok, term()} | {error, term()} when
    Url :: binary(), Form :: list(), Context :: z:context().
post_form(Url, Form, Context) ->
    post_form(Url, Form, 0, Context).

post_form(_, _, 5, _) ->
    {error, too_many_redirects};
post_form(Url, Form, N, Context) ->
    case z_websub_discovery:is_url(Url) of
        false ->
            {error, invalid_url};
        true ->
            Body = iolist_to_binary(uri_string:compose_query(Form)),
            Options = [{content_type, <<"application/x-www-form-urlencoded">>},
                {autoredirect, false}, {timeout, 10000}, {max_length, 65536}],
            case ?MODULE:fetch(post, Url, Body, Options, Context) of
                {error, {Code, _, Headers, _, _}} when Code =:= 307; Code =:= 308 ->
                    Location = proplists:get_value("location", Headers,
                        proplists:get_value(<<"location">>, Headers)),
                    case Location of
                        undefined ->
                            {error, missing_location};
                        _ ->
                            try
                                z_convert:to_binary(uri_string:resolve(z_convert:to_binary(Location), Url))
                            of
                                Next ->
                                    case {Url, Next} of
                                        {<<"https:", _/binary>>, <<"http:", _/binary>>} ->
                                            {error, insecure_redirect};
                                        _ ->
                                            NextContext = case origin(Url) =:= origin(Next) of
                                                true ->
                                                    Context;
                                                false ->
                                                    anonymous(Context)
                                            end,
                                            post_form(Next, Form, N + 1, NextContext)
                                    end
                            catch
                                _:_ -> {error, invalid_redirect}
                            end
                    end;
                {error, {Code, _, _, _, _}} ->
                    {error, {http_status, Code}};
                Result ->
                    Result
            end
    end.

%% @doc GET with a fresh destination check on every hop. Once an origin changes,
%% credentials remain disabled for the rest of the chain, including redirects back.
-spec get(Url, Options, Context) -> term() when
    Url :: binary(), Options :: list(), Context :: z:context().
get(Url, Options, Context) ->
    get(Url, Options, Context, 0).
get(_, _, _, 5) ->
    {error, too_many_redirects};
get(Url, Options, Context, N) ->
    case ?MODULE:fetch(get, Url, <<>>, Options, Context) of
        {error, {Code, _, Headers, _, _}}
            when
                Code =:= 301; Code =:= 302; Code =:= 303;
                Code =:= 307; Code =:= 308 ->
            case proplists:get_value("location", Headers) of
                undefined ->
                    {error, missing_location};
                Location ->
                    try z_convert:to_binary(uri_string:resolve(Location, Url)) of
                        Next ->
                            case {Url, Next} of
                                {<<"https:", _/binary>>, <<"http:", _/binary>>} ->
                                    {error, insecure_redirect};
                                _ ->
                                    {NextOptions, Ctx} = case origin(Url) =:= origin(Next) of
                                        true ->
                                            {Options, Context};
                                        false ->
                                            {anonymous_options(Options), anonymous(Context)}
                                    end,
                                    get(Next, NextOptions, Ctx, N + 1)
                            end
                    catch _:_ -> {error, invalid_redirect} end
            end;
        Result ->
            Result
    end.

anonymous_options(Options) ->
    Headers = proplists:get_value(headers, Options, []),
    CredentialHeaders = [<<"authorization">>, <<"proxy-authorization">>, <<"cookie">>],
    AnonymousHeaders = [
        {Name, Value}
        || {Name, Value} <- Headers,
           not lists:member(z_string:to_lower(z_convert:to_binary(Name)), CredentialHeaders)
    ],
    % Remove the authorization option as well as credentials in explicit headers.
    OptionsWithoutAuthorization = proplists:delete(authorization, Options),
    OptionsWithoutHeaders = proplists:delete(headers, OptionsWithoutAuthorization),
    [{headers, AnonymousHeaders} | OptionsWithoutHeaders].

origin(Url) ->
    #{scheme := S, host := H} = P = uri_string:parse(Url),
    {S, H, maps:get(port, P, case S of <<"https">> -> 443; _ -> 80 end)}.
anonymous(undefined) ->
    undefined;
anonymous(Context) ->
    z_acl:anondo(z_context:new(Context)).

%% @doc Reject destinations resolving to non-public addresses before fetching.
%% TODO: pin the validated address in z_fetch/z_url_fetch while preserving the
%% original Host header and TLS hostname. DNS preflight alone does not prevent
%% rebinding between this lookup and the fetch library's connection lookup.
-spec destination(Url) -> {ok, map(), inet:ip_address()} | {error, term()} when
    Url :: binary().
destination(Url) ->
    destination_checked(Url, false).

%% Development exceptions require both a development site and a .test hostname.
-spec destination(binary(), z:context() | undefined) ->
    {ok, map(), inet:ip_address()} | {error, term()}.
destination(Url, Context) ->
    destination_checked(Url, is_development_destination(Url, Context)).

destination_checked(Url, IsDevelopment) ->
    % Verification appends topic/challenge parameters to the stored callback.
    Valid = try
        byte_size(Url) =< 8192 andalso z_websub_discovery:is_url(
            z_convert:to_binary(uri_string:recompose(maps:remove('query', uri_string:parse(Url)))))
    catch _:_ ->
        false end,
    case Valid of
        false ->
            {error, invalid_url};
        true ->
            #{host := Host} = Parts = uri_string:parse(Url),
            Name = string:trim(binary_to_list(Host), both, "[]"),
            IPs = case inet:parse_address(Name) of
                {ok, IP} ->
                    [IP];
                _ ->
                    lists:append([
                        case inet:getaddrs(Name, Family) of
                            {ok, As} -> As;
                            _ -> []
                        end
                        || Family <- [inet, inet6]
                    ])
            end,
            case IPs =/= [] andalso lists:all(
                fun(IP) -> z_ip_address:is_public(IP) orelse
                    (IsDevelopment andalso is_development_address(IP)) end,
                IPs) of
                true ->
                    {ok, Parts, hd(IPs)};
                false ->
                    {error, unsafe_destination}
            end
    end.

is_development_destination(_Url, undefined) ->
    false;
is_development_destination(Url, Context) ->
    try uri_string:parse(Url) of
        #{host := Host} ->
            Name = string:lowercase(string:trim(Host, trailing, ".")),
            re:run(Name, <<"\\.test$">>, [{capture, none}]) =:= match
                andalso m_site:environment(Context) =:= development;
        _ ->
            false
    catch _:_ ->
        false
    end.

%% Limit the exception to loopback/private networks, excluding link-local
%% metadata services, multicast, unspecified and other reserved destinations.
is_development_address({127, _, _, _}) -> true;
is_development_address({10, _, _, _}) -> true;
is_development_address({172, N, _, _}) when N >= 16, N =< 31 -> true;
is_development_address({192, 168, _, _}) -> true;
is_development_address({0, 0, 0, 0, 0, 0, 0, 1}) -> true;
is_development_address({N, _, _, _, _, _, _, _}) when N >= 16#fc00, N =< 16#fdff -> true;
is_development_address({0, 0, 0, 0, 0, 16#ffff, High, Low}) ->
    is_development_address({High bsr 8, High band 255, Low bsr 8, Low band 255});
is_development_address(_) -> false.

%% @doc Fixed-endpoint request using Zotonic's fetch-options/OAuth2 integration.
%% Redirect policy is handled by the callers, with a new destination check and
%% credential context for each hop. Self-signed TLS is accepted only for .test
%% peers in development, consistently with the destination exception.
-spec fetch(Method, Url, Body, Options, Context) -> term() when
    Method :: get | post, Url :: binary(), Body :: binary() | list(),
    Options :: list(), Context :: z:context() | undefined.
fetch(Method, Url, Body, Options, Context) ->
    case ?MODULE:destination(Url, Context) of
        {ok, _, _} ->
            SafeOptions = [
                {autoredirect, false},
                {insecure, is_development_destination(Url, Context)}
                | proplists:delete(autoredirect, proplists:delete(insecure, Options))
            ],
            case z_fetch:fetch(Method, Url, Body, SafeOptions, Context) of
                {ok, _} = Ok ->
                    Ok;
                {error, {Code, Final, Headers, Size, Response}} when is_integer(Code) ->
                    {error, {Code, Final, Headers, Size, Response}};
                {error, _} ->
                    {error, fetch_failed}
            end;
        Error ->
            Error
    end.
