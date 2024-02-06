%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011-2024 Arjan Scherpenisse
%% @doc OEmbed client
%% @end

%% Copyright 2011-2024 Arjan Scherpenisse <arjan@scherpenisse.net>
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

-module(oembed_client).

-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

%% interface functions
-export([discover/2, providers/2]).

-include_lib("../include/oembed.hrl").

-define(HTTP_GET_TIMEOUT, 20000).

%%====================================================================
%% API
%%====================================================================

discover(Url, Context) ->
    UrlExtra = oembed_url_extra(Context),
    fixup_html(discover_per_provider(Url, UrlExtra, oembed_providers:list(), Context)).

providers(Url, _Context) ->
    find_providers(Url, oembed_providers:list(), []).

%%====================================================================
%% Internal functions
%%====================================================================

discover_per_provider(Url, UrlExtra, [Provider=#oembed_provider{}|Rest], Context) ->
    case re:run(Url, Provider#oembed_provider.url_re) of
        {match, _} ->
            case Provider#oembed_provider.callback of
                F when is_function(F) ->
                    F(Url);
                undefined ->
                    RequestUrl = Provider#oembed_provider.endpoint_url
                        ++ "?format=json&url=" ++ z_utils:url_encode(Url) ++ UrlExtra,
                    oembed_request(RequestUrl)
            end;
        nomatch ->
            discover_per_provider(Url, UrlExtra, Rest, Context)
    end;
discover_per_provider(Url, UrlExtra, [], Context) ->
    case m_config:get_value(mod_oembed, embedly_key, Context) of
        undefined ->
            {error, nomatch};
        Key ->
            lager:debug("Fallback embed.ly discovery for url: ~p~n", [Url]),
            EmbedlyEndpoint = iolist_to_binary([
                "http://api.embed.ly/1/oembed?format=json",
                "&key=", z_url:url_encode(Key),
                "&url=", z_url:url_encode(Url),
                UrlExtra
            ]),
            oembed_request(binary_to_list(EmbedlyEndpoint))
    end.


find_providers(_Url, [], Acc) ->
    lists:reverse(Acc);
find_providers(Url, [Provider=#oembed_provider{}|Rest], Acc) ->
    case re:run(Url, Provider#oembed_provider.url_re) of
        {match, _} -> find_providers(Url, Rest, [Provider|Acc]);
        nomatch -> find_providers(Url, Rest, Acc)
    end.

oembed_request(RequestUrl) ->
    case z_url_fetch:fetch(RequestUrl, []) of
        {ok, {_FinalUrl, _Hs, _Size, Body}} ->
            {ok, z_convert:convert_json(mochijson2:decode(Body))};
        {error, {404, _FinalUrl, _Hs, _Sz, _Body}} ->
            {error, {http, 404, <<>>}};
        {error, {NoAccess, _FinalUrl, Hs, _Sz, Body}} when NoAccess =:= 401; NoAccess =:= 403 ->
            lager:warning("OEmbed HTTP Request returned ~p for '~p' (~p ~p)", [NoAccess, RequestUrl, Hs, Body]),
            {error, {http, NoAccess, Body}};
        {error, {Code, _FinalUrl, Hs, _Sz, Body}} ->
            lager:info("OEmbed HTTP Request returned ~p for '~p' (~p ~p)", [Code, RequestUrl, Hs, Body]),
            {error, {http, Code, Body}};
        {error, _} = Error ->
            Error
    end.

%% @doc Construct extra URL arguments to the OEmbed client request from the oembed module config.
oembed_url_extra(Context) ->
    W = m_config:get_value(oembed, maxwidth, 640, Context),
    X1 =  "&maxwidth=" ++ z_utils:url_encode(z_convert:to_list(W)),
    X2 = case m_config:get_value(oembed, maxheight, Context) of
             undefined -> X1;
             H -> X1 ++ "&maxheight=" ++ z_utils:url_encode(z_convert:to_list(H))
         end,
    X2.

%% @doc Fix oembed returned HTML, remove http: protocol to ensure that the oembed can also be shown on https: sites.
fixup_html({ok, Props}) ->
    {ok, fixup_protocol(html, Props)};
fixup_html({error, _} = Error) ->
    Error.

fixup_protocol(Key, Props) ->
    case lists:keytake(Key, 1, Props) of
        false ->
            Props;
        {value, {Key, Value}, Props1} ->
            Value1 = binary:replace(Value, <<"http://">>, <<"//">>, [global]),
            [{Key,Value1}|Props1]
    end.
