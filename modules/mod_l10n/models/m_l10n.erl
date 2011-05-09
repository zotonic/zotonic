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

-behaviour(gen_model).

-export([
    m_find_value/3,
    m_value/2,
    m_to_list/2,
    
    countries/1,
    country_name/2,
    country_name/3
]).

-include("zotonic.hrl").

% m.l10n.countries

% @doc Return a list of countries in the current language
m_find_value(countries, #m{value=undefined}, Context) ->
    countries(Context);
m_find_value(country_name, #m{value=undefined} = M, _Context) ->
    M#m{value=country_name};
m_find_value(Code, #m{value=country_name}, Context) ->
    country_name(Code, Context).

m_value(#m{}, _Context) ->
    undefined.

m_to_list(#m{}, _Context) ->
    [].


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
	

