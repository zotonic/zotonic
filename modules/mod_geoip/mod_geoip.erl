%% @doc Map IP addresses to geo locations using the MaxMind database.
%% @author Marc Worrell <marc@worrell.nl>

%% Copyright 2019-2020 Marc Worrell
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

-module(mod_geoip).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("GeoIP").
-mod_description("Map IP addresses to geo locations.").

-define(MAXMIND_COUNTRY_DB, 'GeoLite2-Country').
-define(MAXMIND_CITY_DB, 'GeoLite2-City').

-export([
    init/1,
    lookup/1,

    license_key/0
]).

-include("zotonic.hrl").

-spec lookup( tuple() | string() | binary) -> {ok, map()} | {error, invalid_address|not_found}.
lookup(IP) ->
    case locus:lookup(city, IP) of
        {ok, Info} ->
            {ok, result(Info)};
        {error, _} = Error ->
            Error
    end.

init(Context) ->
    {ok, _} = application:ensure_all_started(locus),
    maybe_set_license_key(license_key(), Context),
    case locus:start_loader(city, ?MAXMIND_CITY_DB) of
        ok ->
            ok;
        {error, already_started} ->
            ok;
        {error, Reason} ->
            ?zError("Could not start mod_geoip dependency locus: ~p", [ Reason ], Context)
    end.

maybe_set_license_key(undefined, Context) -> maybe_log_error(Context);
maybe_set_license_key("", Context) -> maybe_log_error(Context);
maybe_set_license_key(<<>>, Context) -> maybe_log_error(Context);
maybe_set_license_key(LicenseKey, _Context) ->
    application:set_env(locus, license_key, z_convert:to_list(LicenseKey)).

maybe_log_error(Context) ->
    case application:get_env(locus, license_key) of
        {ok, "YOUR_LICENSE_KEY"} -> log_error(Context);
        {ok, ""} -> log_error(Context);
        {ok, _} -> ok;
        undefined -> log_error(Context)
    end.

log_error(Context) ->
    ?zError("Please provide a MaxMind license key in zotonic.config key 'maxmind_license_key'", Context).

license_key() ->
    case z_config:get(maxmind_license_key) of
        undefined -> license_key_app();
        "" -> license_key_app();
        LicKey -> LicKey
    end.

license_key_app() ->
    case application:get_env(locus, license_key) of
        {ok, "YOUR_LICENSE_KEY"} -> undefined;
        {ok, ""} -> undefined;
        {ok, LicenseKey} -> LicenseKey;
        undefined -> undefined
    end.

result(Map) ->
    #{
        city => extract_city(Map),
        continent => extract_continent(Map),
        country => extract_country(Map),
        location => extract_location(Map),
        subdivisions => extract_subdivisions(Map)
    }.

extract_city( #{ <<"city">> := City } ) ->
    map_trans( maps:get(<<"names">>, City, #{}) );
extract_city( _Map ) ->
    undefined.

extract_continent( #{ <<"continent">> := Continent } ) when is_map(Continent) ->
    #{
        code => maps:get(<<"code">>, Continent, undefined),
        name => map_trans( maps:get(<<"names">>, Continent, #{}) )
    };
extract_continent(_) ->
    #{}.

extract_country( #{ <<"country">> := Country } ) when is_map(Country) ->
    #{
        is_eu => maps:get(<<"is_in_european_union">>, Country, false),
        iso => z_string:to_lower( maps:get(<<"iso_code">>, Country, <<>>) ),
        name => map_trans( maps:get(<<"names">>, Country, #{}) )
    };
extract_country(_) ->
    #{}.

extract_location( #{ <<"location">> := Location } = Info ) when is_map(Location) ->
    Postal = maps:get(<<"postal">>, Info, #{}),
    #{
        accuracy_radius => maps:get(<<"accuracy_radius">>, Location, undefined),
        longitude => maps:get(<<"longitude">>, Location, undefined),
        latitude => maps:get(<<"latitude">>, Location, undefined),
        timezone => maps:get(<<"time_zone">>, Location, undefined),
        postcode => maps:get(<<"code">>, Postal, undefined)
    };
extract_location(_) ->
    #{}.

extract_subdivisions(#{ <<"subdivisions">> := Subdivisions }) when is_list(Subdivisions) ->
    lists:map(
        fun( #{ <<"names">> := Names, <<"iso_code">> := Code } ) ->
            #{
                code => Code,
                name => map_trans(Names)
            }
        end,
        Subdivisions);
extract_subdivisions(_) ->
    [].


map_trans( Names ) ->
    Tr = maps:fold(
        fun(K, V, Acc) ->
            [ {map_language(K), V} | Acc ]
        end,
        [],
        Names),
    {trans, Tr}.

map_language(<<A, B, $-, _/binary>>) ->
    list_to_atom([ A, B ]);
map_language(Lang) ->
    binary_to_atom(Lang, utf8).
