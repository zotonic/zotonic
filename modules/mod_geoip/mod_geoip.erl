%% @doc Map IP addresses to geo locations using the MaxMind database.
%% @author Marc Worrell <marc@worrell.nl>

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

-module(mod_geoip).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("GeoIP").
-mod_description("Map IP addresses to geo locations.").

-define(MAXMIND_DB_URL, "https://geolite.maxmind.com/download/geoip/database/GeoLite2-Country.tar.gz").

-export([
    init/1
]).

init(_Context) ->
    {ok, _} = application:ensure_all_started(locus),
    case locus:start_loader(country, ?MAXMIND_DB_URL) of
        ok ->
            ok;
        {error, already_started} ->
            ok;
        {error, Reason} ->
            lager:error("mod_geoip: could not start locus loader: ~p", [Reason])
    end.
