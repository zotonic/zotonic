%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011-2024 Arjan Scherpenisse
%% @doc OEmbed client - discover embed information for URLs.
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

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("../include/oembed.hrl").

%% Endpoint for embed.ly oembed service
-define(EMBEDLY_ENDPOINT, "https://api.embed.ly/1/oembed?format=json&url=").
-define(HTTP_GET_TIMEOUT, 20000).
-define(DEFAULT_MAXWIDTH, 1200).
-define(MAX_CONTENT_LENGTH, 1024*1024).

%%====================================================================
%% API
%%====================================================================

discover(Url, Context) ->
    UrlExtra = oembed_url_extra(Context),
    fixup_html(discover_per_provider(Url, UrlExtra, oembed_providers:list(), Context)).

providers(Url, _Context) ->
    find_providers(Url, oembed_providers:list(), []).

%%====================================================================
%% support functions
%%====================================================================

find_providers(_Url, [], Acc) ->
    lists:reverse(Acc);
find_providers(Url, [Provider=#oembed_provider{}|Rest], Acc) ->
    case re:run(Url, Provider#oembed_provider.url_re) of
        {match, _} -> find_providers(Url, Rest, [Provider|Acc]);
        nomatch -> find_providers(Url, Rest, Acc)
    end.

discover_per_provider(Url, UrlExtra, [Provider=#oembed_provider{}|Rest], Context) ->
    case re:run(Url, Provider#oembed_provider.url_re) of
        {match, _} ->
            RequestUrl = iolist_to_binary([
                    Provider#oembed_provider.endpoint_url,
                    "?format=json&url=", z_url:url_encode(Url),
                    UrlExtra]),
            oembed_request(RequestUrl);
        nomatch ->
            discover_per_provider(Url, UrlExtra, Rest, Context)
    end;
discover_per_provider(Url, UrlExtra, [], Context) ->
    ?LOG_DEBUG(#{
        text => <<"Fallback embed.ly discovery">>,
        in => zotonic_mod_oembed,
        url => Url
    }),
    case m_config:get_value(mod_oembed, embedly_key, Context) of
        undefined ->
            {error, nomatch};
        <<>> ->
            {error, nomatch};
        Key ->
            EmbedlyUrl = iolist_to_binary([
                    ?EMBEDLY_ENDPOINT,
                    z_url:url_encode(Url),
                    UrlExtra
                ]),
            EmbedlyUrl1 = case z_utils:is_empty(Key) of
                true -> EmbedlyUrl;
                false -> <<EmbedlyUrl/binary, "&key=", Key/binary>>
            end,
            oembed_request(EmbedlyUrl1)
    end.


oembed_request(RequestUrl) ->
    FetchOptions = [
        {timeout, ?HTTP_GET_TIMEOUT},
        {max_length, ?MAX_CONTENT_LENGTH}
    ],
    case z_url_fetch:fetch(RequestUrl, FetchOptions) of
        {ok, {_Final, _Hs, _Size, Body}} ->
            try
                {ok, z_json:decode(Body)}
            catch
                _:_ -> {error, nojson}
            end;
        {error, Reason} = Error ->
            ?LOG_WARNING(#{
                text => <<"OEmbed HTTP Request returned error">>,
                in => zotonic_mod_oembed,
                result => error,
                reason => Reason,
                url => RequestUrl
            }),
            Error
    end.

%% @doc Construct extra URL arguments to the OEmbed client request from the oembed module config.
oembed_url_extra(Context) ->
    X1 = ["&maxwidth=", z_url:url_encode(get_config(maxwidth, ?DEFAULT_MAXWIDTH, Context))],
    X2 = case get_config(maxheight, undefined, Context) of
             undefined -> X1;
             <<>> -> X1;
             H -> [X1, "&maxheight=", z_url:url_encode(H)]
         end,
    X2.

get_config(Cfg, Default, Context) ->
    case m_config:get_value(mod_oembed, Cfg, Default, Context) of
        undefined -> m_config:get_value(oembed, Cfg, Default, Context);
        <<>> -> Default;
        Value -> Value
    end.

%% @doc Fix oembed returned HTML, remove http: protocol to ensure that the oembed can also be shown on https: sites.
fixup_html({ok, Map}) ->
    {ok, fixup_protocol(<<"html">>, Map)};
fixup_html({error, _} = Error) ->
    Error.

fixup_protocol(Key, Map) ->
    case maps:get(Key, Map, undefined) of
        undefined ->
            Map;
        Html ->
            Html1 = binary:replace(Html, <<"http://">>, <<"//">>, [global]),
            Map#{ <<"html">> => Html1 }
    end.
