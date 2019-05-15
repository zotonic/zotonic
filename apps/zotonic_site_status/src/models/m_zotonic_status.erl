%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019 Marc Worrell
%% @doc Fetch the status of the sites.

%% Copyright 2019 Marc Worrell
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

-module(m_zotonic_status).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(zotonic_model).

%% interface functions
-export([
    m_get/3,

    get_sites/0,
    get_sites_status/0,
    get_sites_config/0,
    get_site_config/1
]).


-include_lib("zotonic_core/include/zotonic.hrl").


%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ sites_status | Rest ], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true ->
            {ok, {get_sites_status(), Rest}};
        false ->
            {error, eacces}
    end;
m_get([ site_url, Site | Rest ], _Msg, Context) ->
    case z_acl:is_admin(Context) of
        true ->
            {ok, {site_url(Site), Rest}};
        false ->
            {error, eacces}
    end;
m_get(Vs, _Msg, _Context) ->
    lager:info("Unknown ~p lookup: ~p", [?MODULE, Vs]),
    {error, unknown_path}.

-spec get_sites_status() -> [ {atom(), z_sites_manager:site_status()} ].
get_sites_status() ->
    Status1 = maps:filter(
        fun(Site, _Status) ->
            not lists:member(Site, z_sites_manager:get_builtin_sites())
        end,
        z_sites_manager:get_sites()),
    lists:sort(maps:to_list(Status1)).

get_sites_config() ->
    maps:fold(
        fun(Site, _Status, Acc) ->
            [ {Site, get_site_config(Site)} | Acc ]
        end,
        [],
        get_sites()).

get_site_config(Site) ->
    case z_sites_manager:get_site_config(Site) of
        {ok, Config} -> fix_hostname_port_config(Config);
        {error, _} = Error -> [{site,Site}, Error]
    end.

site_url(Site) when is_atom(Site) ->
    Config = get_site_config(Site),
    proplists:get_value(absurl, Config).

fix_hostname_port_config(Config) ->
    Hostname = proplists:get_value(hostname, Config),
    [ {absurl, fetch_absurl(Hostname)} ].

fetch_absurl(undefined) ->
    fetch_absurl("localhost");
fetch_absurl(Hostname) when is_binary(Hostname) ->
    fetch_absurl(binary_to_list(Hostname));
fetch_absurl(Hostname) ->
    [ Host | _ ] = string:tokens(Hostname, ":"),
    case get_protocol_port() of
        none -> "";
        {Protocol, ""} -> lists:flatten([Protocol, "://", Host, "/"]);
        {Protocol, Port} -> lists:flatten([Protocol, "://", Host, ":", Port, "/"])
    end.

%% @doc Return the preferred protocol and port.
get_protocol_port() ->
    case z_config:get(ssl_port) of
        none ->
            case z_config:get(port) of
                none -> none;
                80 -> {"http", ""};
                Port -> {"http", integer_to_list(Port)}
            end;
        443 -> {"https", ""};
        Port ->  {"https", integer_to_list(Port)}
    end.

get_sites() ->
    maps:filter(
        fun(Site,_Status) ->
            not lists:member(Site, z_sites_manager:get_builtin_sites())
         end,
         z_sites_manager:get_sites()).
