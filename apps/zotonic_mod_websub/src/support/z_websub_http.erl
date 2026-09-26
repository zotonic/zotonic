%% @copyright 2026 Marc Worrell
-module(z_websub_http).
-moduledoc("WebSub form requests with explicit redirect and credential policy.").
-export([post_form/3, fetch/5, get/3, is_public/1, destination/1]).
-include_lib("zotonic_core/include/zotonic.hrl").

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
                            try z_convert:to_binary(uri_string:resolve(z_convert:to_binary(Location), Url)) of
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
                            catch _:_ -> {error, invalid_redirect} end
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
            when Code =:= 301; Code =:= 302; Code =:= 303; Code =:= 307; Code =:= 308 ->
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
    Headers = [{K,V} || {K,V} <- proplists:get_value(headers, Options, []),
        not lists:member(z_string:to_lower(z_convert:to_binary(K)),
            [<<"authorization">>, <<"proxy-authorization">>, <<"cookie">>])],
    [{headers, Headers} | proplists:delete(headers, proplists:delete(authorization, Options))].

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
                    lists:append([case inet:getaddrs(Name, Family) of
                    {ok, As} ->
                        As;
                    _ ->
                        []
                end || Family <- [inet, inet6]])
            end,
            case IPs =/= [] andalso lists:all(fun is_public/1, IPs) of
                true ->
                    {ok, Parts, hd(IPs)};
                false ->
                    {error, unsafe_destination}
            end
    end.

-spec is_public(IP) -> boolean() when
    IP :: inet:ip_address().
is_public({A,B,C,D} = IP) ->
    lists:all(fun(N) -> N >= 0 andalso N =< 255 end, [A,B,C,D]) andalso
    not z_ip_address:ip_match(IP, ["0.0.0.0/8", "10.0.0.0/8", "100.64.0.0/10",
        "127.0.0.0/8", "169.254.0.0/16", "172.16.0.0/12", "192.0.0.0/24",
        "192.0.2.0/24", "192.88.99.0/24", "192.168.0.0/16", "198.18.0.0/15", "198.51.100.0/24",
        "203.0.113.0/24", "224.0.0.0/3"]);
is_public({_,_,_,_,_,_,_,_} = IP) ->
    z_ip_address:ip_match(IP, ["2000::/3"]) andalso
    not z_ip_address:ip_match(IP, ["2001::/23", "2001:db8::/32", "2002::/16", "3fff::/20"]);
is_public(_) ->
    false.

%% @doc Fixed-endpoint request using Zotonic's fetch-options/OAuth2 integration.
%% Redirect policy is handled by the callers, with a new destination check and
%% credential context for each hop. Never inherit development-mode insecure TLS.
-spec fetch(Method, Url, Body, Options, Context) -> term() when
    Method :: get | post, Url :: binary(), Body :: binary() | list(),
    Options :: list(), Context :: z:context() | undefined.
fetch(Method, Url, Body, Options, Context) ->
    case ?MODULE:destination(Url) of
        {ok, _, _} ->
            SafeOptions = [{autoredirect, false}, {insecure, false}
                | proplists:delete(autoredirect, proplists:delete(insecure, Options))],
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
