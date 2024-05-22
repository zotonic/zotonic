%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2024 Marc Worrell
%% @doc Reinstall the schema for the given site. The site must have valid configuration
%% to fetch the database configuration. The site will be stopped, the schema
%% dropped and then recreated using the usual installer by restarting the site.
%% @end

%% Copyright 2009-2024 Marc Worrell
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

-module(z_install_reinstall).

-export([
    reinstall/1
]).

-spec reinstall(Site) -> ok | {error, Reason} when
    Site :: atom(),
    Reason :: term().
reinstall(Site) when is_atom(Site) ->
    case z_sites_config:app_is_site(Site) of
        true ->
            case z_sites_config:site_config(Site) of
                {ok, Config} ->
                    case stop_drop(Site, Config) of
                        ok ->
                            z_sites_manager:start(Site);
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        false ->
            {error, nosite}
    end.

stop_drop(_Site, #{ dbdatabase := none }) ->
    {error, nodatabase};
stop_drop(_Site, #{ dbdatabase := <<"none">> }) ->
    {error, nodatabase};
stop_drop(_Site, #{ dbdatabase := "none" }) ->
    {error, nodatabase};
stop_drop(Site, #{ dbdatabase := Db } = Config) when is_list(Db); is_binary(Db) ->
    case stop_site(Site) of
        ok ->
            drop_schema(Site, Config);
        {error, _} = Error ->
            Error
    end;
stop_drop(_Site, #{}) ->
    {error, nodatabase}.

drop_schema(Site, Config) ->
    DbDatabase = z_convert:to_list(get_fallback(dbdatabase, Config, z_config:get(dbdatabase))),
    ConnectOptions = [
        {dbhost, z_convert:to_list(get_fallback(dbhost, Config, z_config:get(dbhost)))},
        {dbport, z_convert:to_integer(get_fallback(dbport, Config, z_config:get(dbport)))},
        {dbuser, z_convert:to_list(get_fallback(dbuser, Config, z_config:get(dbuser)))},
        {dbpassword, z_convert:to_list(get_fallback(dbpassword, Config, z_config:get(dbpassword)))},
        {dbdatabase, DbDatabase},
        {dbschema, z_convert:to_list(get_fallback(dbschema, Config, Site))}
    ],
    z_db:drop_schema(ConnectOptions).

get_fallback(K, Config, Default) ->
    case maps:get(K, Config, Default) of
        undefined -> Default;
        "" -> Default;
        <<>> -> Default;
        V -> V
    end.

stop_site(Site) ->
    case z_sites_manager:get_site_status(Site) of
        {ok, stopped} -> ok;
        {ok, failed} -> ok;
        {ok, stopping} ->
            timer:sleep(100),
            stop_site(Site);
        {ok, _} ->
            z_sites_manager:stop(Site),
            stop_site(Site);
        {error, _} = Error ->
            Error
    end.

