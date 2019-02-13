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
    m_get/3,

    countries/1,
    country_name/2,
    country_name/3,

    timezones/0,
    is_timezone/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("erlang_localtime/include/tz_database.hrl").

%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ countries | Rest ], _Msg, Context) ->
    {ok, {countries(Context), Rest}};
m_get([ country_name, Code | Rest ], _Msg, Context) ->
    {ok, {country_name(Code, Context), Rest}};
m_get([ timezones | Rest ], _Msg, _Context) ->
    {ok, {timezones(), Rest}};
m_get([ default_timezone | Rest ], _Msg, Context) ->
    {ok, {m_config:get_value(mod_l10n, timezone, Context), Rest}};
m_get([ timezone_is_fixed | Rest ], _Msg, Context) ->
    {ok, {m_config:get_boolean(mod_l10n, timezone_is_fixed, Context), Rest}};
m_get(Vs, _Msg, _Context) ->
    lager:info("Unknown ~p lookup: ~p", [?MODULE, Vs]),
    {error, unknown_path}.



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

-spec is_timezone( binary() | string() ) -> boolean().
is_timezone(Tz) ->
    Mapped = case mochiglobal:get(timezones_map) of
        undefined ->
            TzMap = maps:from_list([ {T, true} || T <- timezones() ]),
            mochiglobal:put(timezones_map, TzMap),
            TzMap;
        TzMap ->
            TzMap
    end,
    maps:is_key(Tz, Mapped).

-spec timezones() -> list( binary() ).
timezones() ->
    case mochiglobal:get(timezones) of
        undefined ->
            Tzs = [ z_convert:to_binary(element(1, Tz)) || Tz <- ?tz_database ],
            mochiglobal:put(timezones, Tzs),
            Tzs;
        Tzs ->
            Tzs
    end.

