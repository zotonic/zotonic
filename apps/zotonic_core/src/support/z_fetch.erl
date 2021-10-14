%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021 Marc Worrell
%% @doc Fetch data from URLs. Interfaces to z_url_fetch and z_url_metadata.

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

-module(z_fetch).

-export([
    fetch/3,
    fetch_partial/3,
    metadata/3,
    as_data_url/3,
    error_msg/2
]).

-include("../../include/zotonic.hrl").

%% @doc Fetch data from an URL. Let modules change the fetch options.
-spec fetch( string() | binary(), z_url_fetch:options(), z:context() ) -> z_url_fetch:fetch_result().
fetch(Url, Options, Context) ->
    Url1 = z_convert:to_binary(Url),
    Options1 = add_options(Url1, Options, Context),
    z_url_fetch:fetch(Url1, Options1).


%% @doc Fetch data from an URL. Let modules change the fetch options.
-spec fetch_partial( string() | binary(), z_url_fetch:options(), z:context() ) -> z_url_fetch:fetch_result().
fetch_partial(Url, Options, Context) ->
    Url1 = z_convert:to_binary(Url),
    Options1 = add_options(Url1, Options, Context),
    z_url_fetch:fetch_partial(Url1, Options1).


%% @doc Fetch the metadata from an URL. Let modules change the fetch options.
-spec metadata( string() | binary(), z_url_fetch:options(), z:context() ) -> {ok, z_url_metadat:metadata()} | {error, term()}.
metadata(Url, Options, Context) ->
    Options1 = add_options(Url, Options, Context),
    z_url_metadata:fetch(Url, Options1).


%% @doc Fetch data from an URL. Return the data as a data url.
-spec as_data_url( string() | binary() | undefined, z_url_fetch:options(), z:contex()) -> {ok, binary()} | {error, term()}.
as_data_url(Url, Options, Context) ->
    case fetch(Url, Options, Context) of
        {ok, {_Final, Hs, _Size, Data}} ->
            Mime = content_type(Hs),
            {ok, iolist_to_binary([
                <<"data:">>, Mime,
                <<";base64,">>,
                base64:encode(Data)
            ])};
        {error, _} = Error ->
            Error
    end.

-spec content_type( [ {string(), string()} ]) -> binary().
content_type(Hs) ->
    case proplists:get_value("content-type", Hs) of
        undefined ->
            <<"application/octet-stream">>;
        CT ->
            [ Mime | _ ] = binary:split(z_convert:to_binary(CT), <<";">>),
            z_string:trim(Mime)
    end.


%% @doc Map (http) errors to readable format.
-spec error_msg(integer() | {error, term()}, z:context()) -> binary().
error_msg({Status, _Url, _Hs, _Length, _Body}, Context) ->
    error_msg(Status, Context);
error_msg(401, Context) ->
    ?__("Unauthorized to access the remote resource URL.", Context);
error_msg(403, Context) ->
    ?__("Forbidden to access the remote resource URL.", Context);
error_msg(404, Context) ->
    ?__("The resource at the URL can not be found.", Context);
error_msg(410, Context) ->
    ?__("The resource at the URL is gone.", Context);
error_msg(429, Context) ->
    ?__("Too many requests for the remote server, try again later.", Context);
error_msg(S4xx, Context) when S4xx >= 400, S4xx < 500 ->
    ?__("The remote server can not handle this URL.", Context);
error_msg(503, Context) ->
    ?__("The remote server for the URL is having temporary problems.", Context);
error_msg(S5xx, Context) when S5xx >= 500 ->
    ?__("The remote server for the URL is having problems.", Context);
error_msg(_, Context) ->
    ?__("Could not fetch the remote resource URL.", Context).



%% @doc Add or modify fetch options. For development sites the 'insecure' options is added, as
%% development sites are using self-signed certificates. The #url_fetch_options notification is
%% used to add an authorization header or other option for a specific site. If no language is
%% set in the options then the current context language is used for the preferred language.
add_options(Url,Options, Context) ->
    Options1 = case proplists:is_defined(insecure, Options) of
        false ->
            case m_site:environment(Context) of
                development -> [ insecure | Options ];
                _ -> Options
            end;
        true ->
            Options
    end,
    Options2 = case proplists:is_defined(language, Options1) of
        false ->
            [ {language, z_context:language(Context)} | Options1 ];
        true ->
            Options1
    end,
    case uri_string:parse(Url) of
        #{ host := Host } = Parts ->
            HostPort = case maps:find(port, Parts) of
                {ok, Port} -> <<Host/binary, $:, (integer_to_binary(Port))/binary>>;
                error -> Host
            end,
            case z_notifier:first(#url_fetch_options{
                    url = Url,
                    host = HostPort,
                    options = Options2
                }, Context)
            of
                undefined -> Options1;
                Options3 when is_list(Options3) -> Options3
            end;
        _ ->
            Options2
    end.
