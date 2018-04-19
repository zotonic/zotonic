%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell
%% @doc Localization of Zotonic.  Country, and other lookups.

%% Copyright 2011 Marc Worrell
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

-module(m_l10n).

-author("Marc Worrell <marc@worrell.nl>").

-behaviour(zotonic_model).

-export([
    m_get/2,

    countries/1,
    country_name/2,
    country_name/3,

    timezones/0
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("erlang_localtime/include/tz_database.hrl").

%% @doc Fetch the value for the key from a model source
-spec m_get( list(), z:context() ) -> {term(), list()}.
m_get([ countries | Rest ], Context) ->
    {countries(Context), Rest};
m_get([ country_name, Code | Rest ], Context) ->
    {country_name(Code, Context), Rest};
m_get([ timezones | Rest ], _Context) ->
    {timezones(), Rest};
m_get([ default_timezone | Rest ], Context) ->
    {m_config:get_value(mod_l10n, timezone, Context), Rest};
m_get([ timezone_is_fixed | Rest ], Context) ->
    {m_config:get_boolean(mod_l10n, timezone_is_fixed, Context), Rest};
m_get(Vs, _Context) ->
    lager:error("Unknown ~p lookup: ~p", [?MODULE, Vs]),
    {undefined, []}.



%% @doc Return a sorted list of country names in the language of the context
countries(Context) ->
    Translated = [ {IsoCode, ?__(Country, Context)} || {IsoCode, Country} <- l10n_iso2country:iso2country() ],
    lists:sort(fun({_,A}, {_,B}) -> A =< B end, Translated).


%% @doc Return the name of the country belonging to the code, use the language of the context
country_name(Code, Context) ->
	country_name(Code, z_context:language(Context), Context).

country_name(Code, Language, Context) ->
    B = z_convert:to_binary(Code),
    case proplists:get_value(B, l10n_iso2country:iso2country()) of
        undefined ->
			B;
        CountryEN ->
			z_trans:lookup_fallback(z_trans:translations(CountryEN, Context), Language, Context)
    end.

%% Extracted from the timezone javascript.
timezones() ->
    [
        <<"UTC">>,
        <<"Africa/Johannesburg">>,
        <<"Africa/Lagos">>,
        <<"Africa/Windhoek">>,
        <<"America/Adak">>,
        <<"America/Anchorage">>,
        <<"America/Argentina/Buenos_Aires">>,
        <<"America/Bogota">>,
        <<"America/Caracas">>,
        <<"America/Chicago">>,
        <<"America/Denver">>,
        <<"America/Godthab">>,
        <<"America/Guatemala">>,
        <<"America/Halifax">>,
        <<"America/Los_Angeles">>,
        <<"America/Montevideo">>,
        <<"America/New_York">>,
        <<"America/Noronha">>,
        <<"America/Noronha">>,
        <<"America/Phoenix">>,
        <<"America/Santiago">>,
        <<"America/Santo_Domingo">>,
        <<"America/St_Johns">>,
        <<"Asia/Baghdad">>,
        <<"Asia/Baku">>,
        <<"Asia/Beirut">>,
        <<"Asia/Dhaka">>,
        <<"Asia/Dubai">>,
        <<"Asia/Irkutsk">>,
        <<"Asia/Jakarta">>,
        <<"Asia/Kabul">>,
        <<"Asia/Kamchatka">>,
        <<"Asia/Karachi">>,
        <<"Asia/Kathmandu">>,
        <<"Asia/Kolkata">>,
        <<"Asia/Krasnoyarsk">>,
        <<"Asia/Omsk">>,
        <<"Asia/Rangoon">>,
        <<"Asia/Shanghai">>,
        <<"Asia/Tehran">>,
        <<"Asia/Tokyo">>,
        <<"Asia/Vladivostok">>,
        <<"Asia/Yakutsk">>,
        <<"Asia/Yekaterinburg">>,
        <<"Atlantic/Azores">>,
        <<"Atlantic/Cape_Verde">>,
        <<"Australia/Adelaide">>,
        <<"Australia/Brisbane">>,
        <<"Australia/Darwin">>,
        <<"Australia/Eucla">>,
        <<"Australia/Lord_Howe">>,
        <<"Australia/Sydney">>,
        <<"Europe/Berlin">>,
        <<"Europe/London">>,
        <<"Europe/Moscow">>,
        <<"Pacific/Apia">>,
        <<"Pacific/Auckland">>,
        <<"Pacific/Chatham">>,
        <<"Pacific/Easter">>,
        <<"Pacific/Gambier">>,
        <<"Pacific/Honolulu">>,
        <<"Pacific/Kiritimati">>,
        <<"Pacific/Majuro">>,
        <<"Pacific/Marquesas">>,
        <<"Pacific/Norfolk">>,
        <<"Pacific/Noumea">>,
        <<"Pacific/Pago_Pago">>,
        <<"Pacific/Pitcairn">>,
        <<"Pacific/Tarawa">>,
        <<"Pacific/Tongatapu">>
    ].

