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

-module(mod_l10n).

-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Localization").
-mod_description("Localization support and translations for country names etc.").

-export([
    observe_pivot_rsc_data/3
]).

-include("zotonic.hrl").

%% @doc Expand the two letter iso code country depending on the languages in the resource.
observe_pivot_rsc_data(pivot_rsc_data, Rsc, Context) ->
    Languages = lists:usort([ en, default_language(Context) | resource_languages(Rsc) ]),
    Rsc2 = expand_country(address_country, Rsc, Languages, Context),
    expand_country(mail_country, Rsc2, Languages, Context).

expand_country(Prop, Rsc, Languages, Context) ->
	Rsc1 = map_country(Prop, Rsc),
    case proplists:get_value(Prop, Rsc1) of
        <<>> -> Rsc1;
        undefined -> Rsc1;
        <<_,_>> = Iso ->
            Countries = lists:map(
                            fun(Lang) -> 
                                m_l10n:country_name(Iso, Lang, Context)
                            end,
                            Languages),
            add_alias(Iso, Rsc1 ++ [ {extra_pivot_data, C} || C <- lists:usort(Countries)]);
        _Other -> Rsc1
    end.

    add_alias(<<"us">>, R) ->
        [{extra_pivot_data, <<"USA">>} | R];
    add_alias(<<"uk">>, R) ->
        [{extra_pivot_data, <<"England">>} | R];
    add_alias(<<"nl">>, R) ->
        [{extra_pivot_data, <<"Holland">>} | R];
    add_alias(<<"be">>, R) ->
        [{extra_pivot_data, <<"België">>}, {extra_pivot_data, "Belgique"} | R];
    add_alias(_, R) ->
        R.

%% @doc Map a country name to its iso code.  This very crude and should be more comprehensive.
map_country(Prop, Rsc) ->
    case proplists:get_value(Prop, Rsc) of
        <<>> -> Rsc;
        undefined -> Rsc;
        <<A,B>> when A =< $Z orelse B =< $Z ->
            [{Prop, z_convert:to_binary(z_string:to_lower([A,B]))} | Rsc];
        Country ->
            case z_string:to_lower(Country) of
                "usa" -> [{Prop, <<"us">>} | Rsc];
                "holland" -> [{Prop, <<"nl">>} | Rsc];
                "nederland" -> [{Prop, <<"nl">>} | Rsc];
                "netherlands" -> [{Prop, <<"nl">>} | Rsc];
                "the netherlands" -> [{Prop, <<"nl">>} | Rsc];
                "belgië" -> [{Prop, <<"be">>} | Rsc];
                "belgie" -> [{Prop, <<"be">>} | Rsc];
                "belgique" -> [{Prop, <<"be">>} | Rsc];
                % typos
                "nethlerlands" -> [{Prop, <<"nl">>} | Rsc];
                % just keep as-is
                _ -> Rsc
            end
    end.


default_language(Context) ->
    case m_config:get_value(i18n, language, Context) of
        undefined -> en;
        <<>> -> en;
        Lang -> z_convert:to_atom(Lang)
    end.

resource_languages(Rsc) ->
    case proplists:get_value(language, Rsc) of
        undefined -> [];
        Langs -> [ z_convert:to_atom(Lang) || Lang <- Langs ]
    end.
