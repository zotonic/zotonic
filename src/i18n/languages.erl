%% coding: utf-8
-module(languages).

%% @doc Quoting http://www.i18nguy.com/unicode/language-identifiers.html:
%%      Language identifiers as specified by RFC 3066 [1], can have the form language,
%%      language-country, language-country-variant and some other specialized forms. The
%%      guidelines for choosing between language and language-country are ambiguous.
%%      Language codes used as primary subtags are from ISO 639 [2].
%%      Country codes used as secondary subtags (or tertiary if script tags are secondary)
%%      are from ISO 3166 [3].
%%      Note also that several unique language tags are also defined in the IANA Language
%%      Tag Registry [4]. Script codes, generally 4 letters in length, such as "Hans" for
%%      Simplified Chinese, "Hant" for Traditonal Chinese, "Cyrl" for Cyrillic, etc. are
%%      defined by ISO 15924 Codes For The Representation Of Names Of Scripts [5].
%%      [1] http://www.ietf.org/rfc/rfc3066.txt
%%      [2] http://www.loc.gov/standards/iso639-2/langcodes.html
%%      [3] https://www.iso.org/obp/ui/#search and
%%          http://data.okfn.org/data/core/country-list
%%      [4] http://www.iana.org/assignments/language-tags
%%      [5] http://www.unicode.org/iso15924/codelists.html and
%%          https://en.wikipedia.org/wiki/ISO_15924

-export([
    lc2lang/1,
    languages/0,
    languages_data/0
]).


%% Record for language stats
-record(language_data, {
    sub_languages = dict:new()
}).


%% @doc Returns the language code, if the language is defined; otherwise undefined.
-spec lc2lang(binary()) -> binary() | undefined.
lc2lang(LangCode) ->
    case proplists:get_value(LangCode, languages()) of
        undefined -> undefined;
        LangProps -> proplists:get_value(name, LangProps)
    end.


%% @doc Returns language data. Used to present language options in the admin interface.
%%      Adds a couple of properties.
%%      Added for each language:
%%      -   is_main (boolean)
%%      -   sub_languages (list of language codes)
-spec languages_data() -> list().
languages_data() ->
    Languages = languages(),
    % First create a dictionary of main_language:[sub_language1, ...]
    Data = lists:foldl(fun({Code, Props}, Acc=#language_data{}) ->
        Language = proplists:get_value(language, Props),
        Dict = Acc#language_data.sub_languages,
        Dict1 = case dict:is_key(Language, Dict) of
            true -> Dict;
            false -> dict:store(Language, [], Dict)
        end,
        case (Language == Code) of
            true -> Acc#language_data{sub_languages=Dict1};
            false -> Acc#language_data{sub_languages=dict:append(Language, Code, Dict1)}
        end
    end, #language_data{}, Languages),
    SubLanguages = Data#language_data.sub_languages,
    % Use the dictionary to add is_main and sub_languages to the original list
    % of languages.
    WithStats = lists:map(fun({Code, Props}) ->
        Language = proplists:get_value(language, Props),
        Props1 = [{is_main, Language == Code} | Props],
        case dict:is_key(Code, SubLanguages) of
            false -> {Code, Props1};
            true -> {Code, [{sub_languages, dict:fetch(Code, SubLanguages)} | Props1]}
        end
    end, Languages),
    lists:sort(fun({_, PropsA}, {_, PropsB}) ->
        proplists:get_value(name_en, PropsA) =< proplists:get_value(name_en, PropsB)
    end, WithStats).


%% @doc List of language data.
%%      For each language:
%%      -   key (binary): language code - this is the ISO 639-1 language code or otherwise
%%          the ISO 639-3, combined with territory or script extension (lowercase)
%%      -   language (binary; only for region/script variations): fallback language if
%%          translation is not available for variation
%%      -   territory (binary; only for region variations): Alpha-2 code of country/region
%%          (ISO 3166-2)
%%      -   script (binary; only for script variations): 4-letter script code (ISO 15924)
%%      -   direction (binary): LTR or RTL
%%      -   name (binary): native language name
%%      -   name_en (binary): English language name
-spec languages() -> list().
languages() -> [
    {<<"af">>, [
        {language, <<"af">>},
        {direction, <<"LTR">>},
        {name, <<"Afrikaans"/utf8>>},
        {name_en, <<"Afrikaans"/utf8>>}
    ]},
    {<<"ar">>, [
        {language, <<"ar">>},
        {direction, <<"RTL">>},
        {script, <<"Arab">>},
        {name, <<"العربية"/utf8>>},
        {name_en, <<"Arabic"/utf8>>}
    ]},
        {<<"ar-ae">>, [
            {language, <<"ar">>},
            {direction, <<"RTL">>},
            {territory, <<"AE">>},
            {script, <<"Arab">>},
            {name, <<"العربية - الإمارات العربية المتحدة."/utf8>>},
            {name_en, <<"Arabic - U.A.E."/utf8>>}
        ]},
        {<<"ar-bh">>, [
            {language, <<"ar">>},
            {direction, <<"RTL">>},
            {territory, <<"BH">>},
            {script, <<"Arab">>},
            {name, <<"العربية - البحرين"/utf8>>},
            {name_en, <<"Arabic - Bahrain"/utf8>>}
        ]},
        {<<"ar-dz">>, [
            {language, <<"ar">>},
            {direction, <<"RTL">>},
            {territory, <<"DZ">>},
            {script, <<"Arab">>},
            {name, <<"العربية - الجزائر"/utf8>>},
            {name_en, <<"Arabic - Algeria"/utf8>>}
        ]},
        {<<"ar-eg">>, [
            {language, <<"ar">>},
            {direction, <<"RTL">>},
            {territory, <<"EG">>},
            {script, <<"Arab">>},
            {name, <<"مصر"/utf8>>},
            {name_en, <<"Arabic - Egypt"/utf8>>}
        ]},
        {<<"ar-iq">>, [
            {language, <<"ar">>},
            {direction, <<"RTL">>},
            {territory, <<"IQ">>},
            {script, <<"Arab">>},
            {name, <<"العربية - مصر"/utf8>>},
            {name_en, <<"Arabic - Iraq"/utf8>>}
        ]},
        {<<"ar-jo">>, [
            {language, <<"ar">>},
            {direction, <<"RTL">>},
            {territory, <<"JO">>},
            {script, <<"Arab">>},
            {name, <<"العربية - الأردن"/utf8>>},
            {name_en, <<"Arabic - Jordan"/utf8>>}
        ]},
        {<<"ar-kw">>, [
            {language, <<"ar">>},
            {direction, <<"RTL">>},
            {territory, <<"KW">>},
            {script, <<"Arab">>},
            {name, <<"العربية - الكويت"/utf8>>},
            {name_en, <<"Arabic - Kuwait"/utf8>>}
        ]},
        {<<"ar-lb">>, [
            {language, <<"ar">>},
            {direction, <<"RTL">>},
            {territory, <<"LB">>},
            {script, <<"Arab">>},
            {name, <<"العربية - لبنان"/utf8>>},
            {name_en, <<"Arabic - Lebanon"/utf8>>}
        ]},
        {<<"ar-ly">>, [
            {language, <<"ar">>},
            {direction, <<"RTL">>},
            {territory, <<"LY">>},
            {script, <<"Arab">>},
            {name, <<"العربية - ليبيا"/utf8>>},
            {name_en, <<"Arabic - Libya"/utf8>>}
        ]},
        {<<"ar-ma">>, [
            {language, <<"ar">>},
            {direction, <<"RTL">>},
            {territory, <<"MA">>},
            {script, <<"Arab">>},
            {name, <<"العربية - المغرب"/utf8>>},
            {name_en, <<"Arabic - Morocco"/utf8>>}
        ]},
        {<<"ar-om">>, [
            {language, <<"ar">>},
            {direction, <<"RTL">>},
            {territory, <<"OM">>},
            {script, <<"Arab">>},
            {name, <<"العربية - عمان"/utf8>>},
            {name_en, <<"Arabic - Oman"/utf8>>}
        ]},
        {<<"ar-qa">>, [
            {language, <<"ar">>},
            {direction, <<"RTL">>},
            {territory, <<"QA">>},
            {script, <<"Arab">>},
            {name, <<"العربية - قطر"/utf8>>},
            {name_en, <<"Arabic - Qatar"/utf8>>}
        ]},
        {<<"ar-sa">>, [
            {language, <<"ar">>},
            {direction, <<"RTL">>},
            {territory, <<"SA">>},
            {script, <<"Arab">>},
            {name, <<"العربية - المملكة العربية السعودية"/utf8>>},
            {name_en, <<"Arabic - Saudi Arabia"/utf8>>}
        ]},
        {<<"ar-sy">>, [
            {language, <<"ar">>},
            {direction, <<"RTL">>},
            {territory, <<"SY">>},
            {script, <<"Arab">>},
            {name, <<"العربية - سوريا"/utf8>>},
            {name_en, <<"Arabic - Syria"/utf8>>}
        ]},
        {<<"ar-tn">>, [
            {language, <<"ar">>},
            {direction, <<"RTL">>},
            {territory, <<"TN">>},
            {script, <<"Arab">>},
            {name, <<"العربية - تونس"/utf8>>},
            {name_en, <<"Arabic - Tunisia"/utf8>>}
        ]},
        {<<"ar-ye">>, [
            {language, <<"ar">>},
            {direction, <<"RTL">>},
            {territory, <<"YE">>},
            {script, <<"Arab">>},
            {name, <<"العربية - اليمن"/utf8>>},
            {name_en, <<"Arabic - Yemen"/utf8>>}
        ]},
    {<<"bg">>, [
        {language, <<"bg">>},
        {direction, <<"LTR">>},
        {name, <<"български"/utf8>>},
        {name_en, <<"Bulgarian"/utf8>>}
    ]},
    {<<"bs">>, [
        {language, <<"bs">>},
        {direction, <<"LTR">>},
        {name, <<"bosanski"/utf8>>},
        {name_en, <<"Bosnian"/utf8>>}
    ]},
    {<<"cs">>, [
        {language, <<"cs">>},
        {direction, <<"LTR">>},
        {name, <<"čeština"/utf8>>},
        {name_en, <<"Czech"/utf8>>}
    ]},
    {<<"cy">>, [
        {language, <<"cy">>},
        {direction, <<"LTR">>},
        {name, <<"Cymraeg"/utf8>>},
        {name_en, <<"Welsh"/utf8>>}
    ]},
    {<<"de">>, [
        {language, <<"de">>},
        {direction, <<"LTR">>},
        {name, <<"Deutsch"/utf8>>},
        {name_en, <<"German"/utf8>>}
    ]},
        {<<"de-at">>, [
            {language, <<"de">>},
            {direction, <<"LTR">>},
            {territory, <<"AT">>},
            {name, <<"Deutsch - Österreich"/utf8>>},
            {name_en, <<"German - Austria"/utf8>>}
        ]},
        {<<"de-ch">>, [
            {language, <<"de">>},
            {direction, <<"LTR">>},
            {territory, <<"CH">>},
            {name, <<"Deutsch - Schweiz"/utf8>>},
            {name_en, <<"German - Switzerland"/utf8>>}
        ]},
        {<<"de-de">>, [
            {language, <<"de">>},
            {direction, <<"LTR">>},
            {territory, <<"DE">>},
            {name, <<"Deutsch - Deutschland"/utf8>>},
            {name_en, <<"German - Germany"/utf8>>}
        ]},
        {<<"de-li">>, [
            {language, <<"de">>},
            {direction, <<"LTR">>},
            {territory, <<"LI">>},
            {name, <<"Deutsch - Liechtenstein"/utf8>>},
            {name_en, <<"German - Liechtenstein"/utf8>>}
        ]},
        {<<"de-lu">>, [
            {language, <<"de">>},
            {direction, <<"LTR">>},
            {territory, <<"LU">>},
            {name, <<"Deutsch - Luxemburg"/utf8>>},
            {name_en, <<"German - Luxembourg"/utf8>>}
        ]},
    {<<"dk">>, [
        {language, <<"dk">>},
        {direction, <<"LTR">>},
        {name, <<"dansk"/utf8>>},
        {name_en, <<"Danish"/utf8>>}
    ]},
    {<<"el">>, [
        {language, <<"el">>},
        {direction, <<"LTR">>},
        {name, <<"Ελληνικά"/utf8>>},
        {name_en, <<"Greek"/utf8>>}
    ]},
    {<<"en">>, [
        {language, <<"en">>},
        {direction, <<"LTR">>},
        {name, <<"English"/utf8>>},
        {name_en, <<"English"/utf8>>}
    ]},
        {<<"en-au">>, [
            {language, <<"en">>},
            {direction, <<"LTR">>},
            {territory, <<"AU">>},
            {name, <<"English - Australia"/utf8>>},
            {name_en, <<"English - Australia"/utf8>>}
        ]},
        {<<"en-bz">>, [
            {language, <<"en">>},
            {direction, <<"LTR">>},
            {territory, <<"BZ">>},
            {name, <<"English - Belize"/utf8>>},
            {name_en, <<"English - Belize"/utf8>>}
        ]},
        {<<"en-ca">>, [
            {language, <<"en">>},
            {direction, <<"LTR">>},
            {territory, <<"CA">>},
            {name, <<"English - Canada"/utf8>>},
            {name_en, <<"English - Canada"/utf8>>}
        ]},
        {<<"en-cb">>, [
            {language, <<"en">>},
            {direction, <<"LTR">>},
            {territory, <<"CB">>},
            {name, <<"English - Caribbean"/utf8>>},
            {name_en, <<"English - Caribbean"/utf8>>}
        ]},
        {<<"en-gb">>, [
            {language, <<"en">>},
            {direction, <<"LTR">>},
            {territory, <<"GB">>},
            {name, <<"English - United Kingdom"/utf8>>},
            {name_en, <<"English - United Kingdom"/utf8>>}
        ]},
        {<<"en-ie">>, [
            {language, <<"en">>},
            {direction, <<"LTR">>},
            {territory, <<"IE">>},
            {name, <<"English - Ireland"/utf8>>},
            {name_en, <<"English - Ireland"/utf8>>}
        ]},
        {<<"en-jm">>, [
            {language, <<"en">>},
            {direction, <<"LTR">>},
            {territory, <<"JM">>},
            {name, <<"English - Jamaica"/utf8>>},
            {name_en, <<"English - Jamaica"/utf8>>}
        ]},
        {<<"en-nz">>, [
            {language, <<"en">>},
            {direction, <<"LTR">>},
            {territory, <<"NZ">>},
            {name, <<"English - New Zealand"/utf8>>},
            {name_en, <<"English - New Zealand"/utf8>>}
        ]},
        {<<"en-ph">>, [
            {language, <<"en">>},
            {direction, <<"LTR">>},
            {territory, <<"PH">>},
            {name, <<"English - Republic of the Philippines"/utf8>>},
            {name_en, <<"English - Republic of the Philippines"/utf8>>}
        ]},
        {<<"en-tt">>, [
            {language, <<"en">>},
            {direction, <<"LTR">>},
            {territory, <<"TT">>},
            {name, <<"English - Trinidad and Tobago"/utf8>>},
            {name_en, <<"English - Trinidad and Tobago"/utf8>>}
        ]},
        {<<"en-us">>, [
            {language, <<"en">>},
            {direction, <<"LTR">>},
            {territory, <<"US">>},
            {name, <<"English - United States"/utf8>>},
            {name_en, <<"English - United States"/utf8>>}
        ]},
        {<<"en-za">>, [
            {language, <<"en">>},
            {direction, <<"LTR">>},
            {territory, <<"ZA">>},
            {name, <<"English - South Africa"/utf8>>},
            {name_en, <<"English - South Africa"/utf8>>}
        ]},
        {<<"en-zw">>, [
            {language, <<"en">>},
            {direction, <<"LTR">>},
            {territory, <<"ZW">>},
            {name, <<"English - Zimbabwe"/utf8>>},
            {name_en, <<"English - Zimbabwe"/utf8>>}
        ]},
    {<<"es">>, [
        {language, <<"es">>},
        {direction, <<"LTR">>},
        {name, <<"español"/utf8>>},
        {name_en, <<"Spanish"/utf8>>}
    ]},
        {<<"es-ar">>, [
            {language, <<"es">>},
            {direction, <<"LTR">>},
            {territory, <<"AR">>},
            {name, <<"Español - Argentina"/utf8>>},
            {name_en, <<"Spanish - Argentina"/utf8>>}
        ]},
        {<<"es-bo">>, [
            {language, <<"es">>},
            {direction, <<"LTR">>},
            {territory, <<"BO">>},
            {name, <<"Español - Bolivia"/utf8>>},
            {name_en, <<"Spanish - Bolivia"/utf8>>}
        ]},
        {<<"es-cl">>, [
            {language, <<"es">>},
            {direction, <<"LTR">>},
            {territory, <<"CL">>},
            {name, <<"Español - Chile"/utf8>>},
            {name_en, <<"Spanish - Chile"/utf8>>}
        ]},
        {<<"es-co">>, [
            {language, <<"es">>},
            {direction, <<"LTR">>},
            {territory, <<"CO">>},
            {name, <<"Español - Colombia"/utf8>>},
            {name_en, <<"Spanish - Colombia"/utf8>>}
        ]},
        {<<"es-cr">>, [
            {language, <<"es">>},
            {direction, <<"LTR">>},
            {territory, <<"CR">>},
            {name, <<"Español - Costa Rica"/utf8>>},
            {name_en, <<"Spanish - Costa Rica"/utf8>>}
        ]},
        {<<"es-do">>, [
            {language, <<"es">>},
            {direction, <<"LTR">>},
            {territory, <<"DO">>},
            {name, <<"Español - República Dominicana"/utf8>>},
            {name_en, <<"Spanish - Dominican Republic"/utf8>>}
        ]},
        {<<"es-ec">>, [
            {language, <<"es">>},
            {direction, <<"LTR">>},
            {territory, <<"EC">>},
            {name, <<"Español - Ecuador"/utf8>>},
            {name_en, <<"Spanish - Ecuador"/utf8>>}
        ]},
        {<<"es-es">>, [
            {language, <<"es">>},
            {direction, <<"LTR">>},
            {territory, <<"ES">>},
            {name, <<"Español - España"/utf8>>},
            {name_en, <<"Spanish - Spain"/utf8>>}
        ]},
        {<<"es-gt">>, [
            {language, <<"es">>},
            {direction, <<"LTR">>},
            {territory, <<"GT">>},
            {name, <<"Español - Guatemala"/utf8>>},
            {name_en, <<"Spanish - Guatemala"/utf8>>}
        ]},
        {<<"es-hn">>, [
            {language, <<"es">>},
            {direction, <<"LTR">>},
            {territory, <<"HN">>},
            {name, <<"Español - Honduras"/utf8>>},
            {name_en, <<"Spanish - Honduras"/utf8>>}
        ]},
        {<<"es-mx">>, [
            {language, <<"es">>},
            {direction, <<"LTR">>},
            {territory, <<"MX">>},
            {name, <<"Español - México"/utf8>>},
            {name_en, <<"Spanish - Mexico"/utf8>>}
        ]},
        {<<"es-ni">>, [
            {language, <<"es">>},
            {direction, <<"LTR">>},
            {territory, <<"NI">>},
            {name, <<"Español - Nicaragua"/utf8>>},
            {name_en, <<"Spanish - Nicaragua"/utf8>>}
        ]},
        {<<"es-pa">>, [
            {language, <<"es">>},
            {direction, <<"LTR">>},
            {territory, <<"PA">>},
            {name, <<"Español - Panamá"/utf8>>},
            {name_en, <<"Spanish - Panama"/utf8>>}
        ]},
        {<<"es-pe">>, [
            {language, <<"es">>},
            {direction, <<"LTR">>},
            {territory, <<"PE">>},
            {name, <<"Español - Perú"/utf8>>},
            {name_en, <<"Spanish - Peru"/utf8>>}
        ]},
        {<<"es-pr">>, [
            {language, <<"es">>},
            {direction, <<"LTR">>},
            {territory, <<"PR">>},
            {name, <<"Español - Puerto Rico"/utf8>>},
            {name_en, <<"Spanish - Puerto Rico"/utf8>>}
        ]},
        {<<"es-py">>, [
            {language, <<"es">>},
            {direction, <<"LTR">>},
            {territory, <<"PY">>},
            {name, <<"Español - Paraguay"/utf8>>},
            {name_en, <<"Spanish - Paraguay"/utf8>>}
        ]},
        {<<"es-sv">>, [
            {language, <<"es">>},
            {direction, <<"LTR">>},
            {territory, <<"SV">>},
            {name, <<"Español - El Salvador"/utf8>>},
            {name_en, <<"Spanish - El Salvador"/utf8>>}
        ]},
        {<<"es-uy">>, [
            {language, <<"es">>},
            {direction, <<"LTR">>},
            {territory, <<"UY">>},
            {name, <<"Español - Uruguay"/utf8>>},
            {name_en, <<"Spanish - Uruguay"/utf8>>}
        ]},
        {<<"es-ve">>, [
            {language, <<"es">>},
            {direction, <<"LTR">>},
            {territory, <<"VE">>},
            {name, <<"Español - Venezuela"/utf8>>},
            {name_en, <<"Spanish - Venezuela"/utf8>>}
        ]},
    {<<"et">>, [
        {language, <<"et">>},
        {direction, <<"LTR">>},
        {name, <<"eesti"/utf8>>},
        {name_en, <<"Estonian"/utf8>>}
    ]},
    {<<"fi">>, [
        {language, <<"fi">>},
        {direction, <<"LTR">>},
        {name, <<"suomi"/utf8>>},
        {name_en, <<"Finnish"/utf8>>}
    ]},
    {<<"fr">>, [
        {language, <<"fr">>},
        {direction, <<"LTR">>},
        {name, <<"français"/utf8>>},
        {name_en, <<"French"/utf8>>}
    ]},
        {<<"fr-be">>, [
            {language, <<"fr">>},
            {direction, <<"LTR">>},
            {territory, <<"BE">>},
            {name, <<"Français - Belgique"/utf8>>},
            {name_en, <<"French - Belgium"/utf8>>}
        ]},
        {<<"fr-ca">>, [
            {language, <<"fr">>},
            {direction, <<"LTR">>},
            {territory, <<"CA">>},
            {name, <<"Français - Canada"/utf8>>},
            {name_en, <<"French - Canada"/utf8>>}
        ]},
        {<<"fr-ch">>, [
            {language, <<"fr">>},
            {direction, <<"LTR">>},
            {territory, <<"CH">>},
            {name, <<"Français - Suisse"/utf8>>},
            {name_en, <<"French - Switzerland"/utf8>>}
        ]},
        {<<"fr-fr">>, [
            {language, <<"fr">>},
            {direction, <<"LTR">>},
            {territory, <<"FR">>},
            {name, <<"Français - France"/utf8>>},
            {name_en, <<"French - France"/utf8>>}
        ]},
        {<<"fr-lu">>, [
            {language, <<"fr">>},
            {direction, <<"LTR">>},
            {territory, <<"LU">>},
            {name, <<"Français - Luxembourg"/utf8>>},
            {name_en, <<"French - Luxembourg"/utf8>>}
        ]},
        {<<"fr-mc">>, [
            {language, <<"fr">>},
            {direction, <<"LTR">>},
            {territory, <<"MC">>},
            {name, <<"Français - Monaco"/utf8>>},
            {name_en, <<"French - Monaco"/utf8>>}
        ]},
    {<<"fy">>, [
        {language, <<"fy">>},
        {direction, <<"LTR">>},
        {name, <<"West-Frysk"/utf8>>},
        {name_en, <<"Frisian"/utf8>>}
    ]},
    {<<"ga">>, [
        {language, <<"ga">>},
        {direction, <<"LTR">>},
        {name, <<"Gaeilge"/utf8>>},
        {name_en, <<"Gaelic"/utf8>>}
    ]},
    {<<"he">>, [
        {language, <<"he">>},
        {direction, <<"RTL">>},
        {script, <<"Hebr">>},
        {name, <<"עברית"/utf8>>},
        {name_en, <<"Hebrew"/utf8>>}
    ]},
    {<<"hi">>, [
        {language, <<"hi">>},
        {direction, <<"LTR">>},
        {name, <<"हिन्दी"/utf8>>},
        {name_en, <<"Hindi"/utf8>>}
    ]},
    {<<"hr">>, [
        {language, <<"hr">>},
        {direction, <<"LTR">>},
        {name, <<"hrvatski"/utf8>>},
        {name_en, <<"Croatian"/utf8>>}
    ]},
        {<<"hr-ba">>, [
            {language, <<"hr">>},
            {direction, <<"LTR">>},
            {territory, <<"BA">>},
            {name, <<"hrvatski - Bosna i Hercegovina"/utf8>>},
            {name_en, <<"Croatian - Bosnia and Herzegovina"/utf8>>}
        ]},
        {<<"hr-hr">>, [
            {language, <<"hr">>},
            {direction, <<"LTR">>},
            {territory, <<"HR">>},
            {name, <<"hrvatski - Hrvatska"/utf8>>},
            {name_en, <<"Croatian - Croatia"/utf8>>}
        ]},
    {<<"hu">>, [
        {language, <<"hu">>},
        {direction, <<"LTR">>},
        {name, <<"magyar"/utf8>>},
        {name_en, <<"Hungarian"/utf8>>}
    ]},
    {<<"is">>, [
        {language, <<"is">>},
        {direction, <<"LTR">>},
        {name, <<"íslenska"/utf8>>},
        {name_en, <<"Islandic"/utf8>>}
    ]},
    {<<"it">>, [
        {language, <<"it">>},
        {direction, <<"LTR">>},
        {name, <<"italiano"/utf8>>},
        {name_en, <<"Italian"/utf8>>}
    ]},
        {<<"it-ch">>, [
            {language, <<"it">>},
            {territory, <<"CH">>},
            {name, <<"italiano - Svizzera"/utf8>>},
            {name_en, <<"Italian - Switzerland"/utf8>>}
        ]},
        {<<"it-it">>, [
            {language, <<"it">>},
            {territory, <<"IT">>},
            {name, <<"italiano - Italia"/utf8>>},
            {name_en, <<"Italian - Italy"/utf8>>}
        ]},
    {<<"ja">>, [
        {language, <<"ja">>},
        {direction, <<"LTR">>},
        {script, <<"Jpan">>}, % alias for Han + Hiragana + Katakana
        {name, <<"日本語"/utf8>>},
        {name_en, <<"Japanese"/utf8>>}
    ]},
    {<<"lt">>, [
        {language, <<"lt">>},
        {direction, <<"LTR">>},
        {name, <<"lietuvių"/utf8>>},
        {name_en, <<"Lithuanian"/utf8>>}
    ]},
    {<<"lv">>, [
        {language, <<"lv">>},
        {direction, <<"LTR">>},
        {name, <<"latviešu"/utf8>>},
        {name_en, <<"Latvian"/utf8>>}
    ]},
    {<<"mg">>, [
        {language, <<"mg">>},
        {direction, <<"LTR">>},
        {name, <<"Malagasy"/utf8>>},
        {name_en, <<"Malagasy"/utf8>>}
    ]},
    {<<"nl">>, [
        {language, <<"nl">>},
        {direction, <<"LTR">>},
        {name, <<"Nederlands"/utf8>>},
        {name_en, <<"Dutch"/utf8>>}
    ]},
        {<<"nl-be">>, [
            {language, <<"nl">>},
            {direction, <<"LTR">>},
            {territory, <<"BE">>},
            {name, <<"Vlaams - België"/utf8>>},
            {name_en, <<"Flemish - Belgium"/utf8>>}
        ]},
        {<<"nl-nl">>, [
            {language, <<"nl">>},
            {direction, <<"LTR">>},
            {territory, <<"NL">>},
            {name, <<"Nederlands - Nederland"/utf8>>},
            {name_en, <<"Dutch - Netherlands"/utf8>>}
        ]},
    {<<"no">>, [
        {language, <<"no">>},
        {direction, <<"LTR">>},
        {name, <<"norsk"/utf8>>},
        {name_en, <<"Norwegian"/utf8>>}
    ]},
    {<<"nn">>, [
        {language, <<"nn">>},
        {direction, <<"LTR">>},
        {name, <<"nynorsk"/utf8>>},
        {name_en, <<"Norwegian Nynorsk"/utf8>>}
    ]},
    {<<"pl">>, [
        {language, <<"pl">>},
        {direction, <<"LTR">>},
        {name, <<"polszczyzna"/utf8>>},
        {name_en, <<"Polish"/utf8>>}
    ]},
    {<<"ps">>, [
        {language, <<"ps">>},
        {direction, <<"LTR">>},
        {name, <<"تو"/utf8>>},
        {name_en, <<"Pashto"/utf8>>}
    ]},
    {<<"pt">>, [
        {language, <<"pt">>},
        {direction, <<"LTR">>},
        {name, <<"português"/utf8>>},
        {name_en, <<"Portuguese"/utf8>>}
    ]},
        {<<"pt-br">>, [
            {language, <<"pt">>},
            {direction, <<"LTR">>},
            {territory, <<"BR">>},
            {name, <<"português - Brasil"/utf8>>},
            {name_en, <<"Portuguese - Brazil"/utf8>>}
        ]},
        {<<"pt-pt">>, [
            {language, <<"pt">>},
            {direction, <<"LTR">>},
            {territory, <<"PT">>},
            {name, <<"português - Portugal"/utf8>>},
            {name_en, <<"Portuguese - Portugal"/utf8>>}
        ]},
    {<<"ro">>, [
        {language, <<"ro">>},
        {direction, <<"LTR">>},
        {name, <<"română"/utf8>>},
        {name_en, <<"Romanian"/utf8>>}
    ]},
    {<<"ru">>, [
        {language, <<"ru">>},
        {direction, <<"LTR">>},
        {name, <<"русский язык"/utf8>>},
        {name_en, <<"Russian"/utf8>>}
    ]},
    {<<"sk">>, [
        {language, <<"sk">>},
        {direction, <<"LTR">>},
        {name, <<"slovenčina"/utf8>>},
        {name_en, <<"Slovak"/utf8>>}
    ]},
    {<<"sl">>, [
        {language, <<"sl">>},
        {direction, <<"LTR">>},
        {name, <<"slovenščina"/utf8>>},
        {name_en, <<"Slovenian"/utf8>>}
    ]},
    {<<"sr-latn">>, [
        {language, <<"sr-latn">>},
        {direction, <<"LTR">>},
        {script, <<"Latn">>},
        {name, <<"srpski"/utf8>>},
        {name_en, <<"Serbian (Latin)"/utf8>>}
    ]},
        {<<"sr-latn-ba">>, [
            {language, <<"sr-latn">>},
            {direction, <<"LTR">>},
            {script, <<"Latn">>},
            {name, <<"srpski (latinica) - Bosna i Hercegovina"/utf8>>},
            {name_en, <<"Serbian (Latin) - Bosnia and Herzegovina"/utf8>>}
        ]},
    {<<"sr-cyrl">>, [
        {language, <<"sr-cyrl">>},
        {direction, <<"LTR">>},
        {script, <<"Cyrl">>},
        {name, <<"Српски језик (Ћирилица)"/utf8>>},
        {name_en, <<"Serbian (Cyrillic)"/utf8>>}
    ]},
        {<<"sr-cyrl-ba">>, [
            {language, <<"sr-cyrl">>},
            {direction, <<"LTR">>},
            {script, <<"Cyrl">>},
            {name, <<"Српски језик (Ћирилица) - Босна и Херцеговина"/utf8>>},
            {name_en, <<"Serbian (Cyrillic) - Bosnia and Herzegovina"/utf8>>}
        ]},
    {<<"sv">>, [
        {language, <<"sv">>},
        {direction, <<"LTR">>},
        {name, <<"svenska"/utf8>>},
        {name_en, <<"Swedish"/utf8>>}
    ]},
    {<<"tr">>, [
        {language, <<"tr">>},
        {direction, <<"LTR">>},
        {name, <<"Türkçe"/utf8>>},
        {name_en, <<"Turkish"/utf8>>}
    ]},
    {<<"uk">>, [
        {language, <<"uk">>},
        {direction, <<"LTR">>},
        {name, <<"українська"/utf8>>},
        {name_en, <<"Ukrainian"/utf8>>}
    ]},
    {<<"zh">>, [
        {language, <<"zh">>},
        {script, <<"Hans">>},
        {direction, <<"LTR">>},
        {name, <<"中文"/utf8>>},
        {name_en, <<"Chinese (Simplified)"/utf8>>}
    ]},
        % Omitting 'zh-hans' as 'zh' is implicitly using Hans
        {<<"zh-hans-cn">>, [
            {language, <<"zh">>},
            {territory, <<"CN">>},
            {script, <<"Hans">>},
            {direction, <<"LTR">>},
            {name, <<"中国大陆简体脚本"/utf8>>},
            {name_en, <<"Mainland Chinese (Simplified)"/utf8>>}
        ]},
        {<<"zh-hans-sg">>, [
            {language, <<"zh">>},
            {territory, <<"SG">>},
            {script, <<"Hans">>},
            {direction, <<"LTR">>},
            {name, <<"新加坡中国简体脚本"/utf8>>},
            {name_en, <<"Singapore Chinese (Simplified)"/utf8>>}
        ]},
    {<<"zh-hant">>, [
        {language, <<"zh-hant">>},
        {script, <<"Hant">>},
        {direction, <<"LTR">>},
        {name, <<"中國傳統的腳本"/utf8>>},
        {name_en, <<"Chinese (Traditional)"/utf8>>}
    ]},
        {<<"zh-hant-hk">>, [
            {language, <<"zh-hant">>},
            {territory, <<"HK">>},
            {script, <<"Hant">>},
            {direction, <<"LTR">>},
            {name, <<"香港中國傳統腳本"/utf8>>},
            {name_en, <<"Hong Kong Chinese (Traditional)"/utf8>>}
        ]},
        {<<"zh-hant-mo">>, [
            {language, <<"zh-hant">>},
            {territory, <<"MO">>},
            {script, <<"Hant">>},
            {direction, <<"LTR">>},
            {name, <<"澳門中國人在傳統的腳本"/utf8>>},
            {name_en, <<"Macau Chinese (Traditional)"/utf8>>}
        ]},
        {<<"zh-hant-tw">>, [
            {language, <<"zh-hant">>},
            {territory, <<"TW">>},
            {script, <<"Hant">>},
            {direction, <<"LTR">>},
            {name, <<"台灣中國傳統腳本"/utf8>>},
            {name_en, <<"Taiwan Chinese (Traditional)"/utf8>>}
        ]}
].

%% TODO:

% "aa" <<"Afar"/utf8>>;
% "ab" <<"Abkhazian"/utf8>>;
% "ae" <<"Avestan"/utf8>>;
% "am" <<"Amharic"/utf8>>;
% "as" <<"Assamese"/utf8>>;
% "ay" <<"Aymara"/utf8>>;
% "az" <<"Azerbaijani"/utf8>>;
% "ba" <<"Bashkir"/utf8>>;
% "be" <<"Byelorussian; Belarusian"/utf8>>;
% "bh" <<"Bihari"/utf8>>;
% "bi" <<"Bislama"/utf8>>;
% "bn" <<"Bengali; Bangla"/utf8>>;
% "bo" <<"Tibetan"/utf8>>;
% "br" <<"Breton"/utf8>>;
% "ca" <<"Catalan"/utf8>>;
% "ce" <<"Chechen"/utf8>>;
% "ch" <<"Chamorro"/utf8>>;
% "co" <<"Corsican"/utf8>>;
% "cu" <<"Church Slavic"/utf8>>;
% "cv" <<"Chuvash"/utf8>>;
% "dz" <<"Dzongkha; Bhutani"/utf8>>;
% "eo" <<"Esperanto"/utf8>>;
% "et" <<"Estonian"/utf8>>;
% "eu" <<"Basque"/utf8>>;
% "fa" <<"Persian"/utf8>>;
% "fj" <<"Fijian; Fiji"/utf8>>;
% "fo" <<"Faroese"/utf8>>;
% "gd" <<"Scots; Gaelic"/utf8>>;
% "gl" <<"Gallegan; Galician"/utf8>>;
% "gn" <<"Guarani"/utf8>>;
% "gu" <<"Gujarati"/utf8>>;
% "gv" <<"Manx"/utf8>>;
% "ha" <<"Hausa"/utf8>>;
% "ho" <<"Hiri Motu"/utf8>>;
% "hy" <<"Armenian"/utf8>>;
% "hz" <<"Herero"/utf8>>;
% "ia" <<"Interlingua"/utf8>>;
% "id" <<"Indonesian"/utf8>>;
% "ie" <<"Interlingue"/utf8>>;
% "ik" <<"Inupiak"/utf8>>;
% "io" <<"Ido"/utf8>>;
% "iu" <<"Inuktitut"/utf8>>;
% "jv" <<"Javanese"/utf8>>;
% "ka" <<"Georgian"/utf8>>;
% "ki" <<"Kikuyu"/utf8>>;
% "kj" <<"Kuanyama"/utf8>>;
% "kk" <<"Kazakh"/utf8>>;
% "kl" <<"Kalaallisut; Greenlandic"/utf8>>;
% "km" <<"Khmer; Cambodian"/utf8>>;
% "kn" <<"Kannada"/utf8>>;
% "ko" <<"Korean"/utf8>>;
% "ks" <<"Kashmiri"/utf8>>;
% "ku" <<"Kurdish"/utf8>>;
% "kv" <<"Komi"/utf8>>;
% "kw" <<"Cornish"/utf8>>;
% "ky" <<"Kirghiz"/utf8>>;
% "la" <<"Latin"/utf8>>;
% "lb" <<"Letzeburgesch"/utf8>>;
% "ln" <<"Lingala"/utf8>>;
% "lo" <<"Lao; Laotian"/utf8>>;
% "mh" <<"Marshall"/utf8>>;
% "mi" <<"Maori"/utf8>>;
% "mk" <<"Macedonian"/utf8>>;
% "ml" <<"Malayalam"/utf8>>;
% "mn" <<"Mongolian"/utf8>>;
% "mo" <<"Moldavian"/utf8>>;
% "mr" <<"Marathi"/utf8>>;
% "ms" <<"Malay"/utf8>>;
% "mt" <<"Maltese"/utf8>>;
% "my" <<"Burmese"/utf8>>;
% "na" <<"Nauru"/utf8>>;
% "nb" <<"Norwegian Bokmål"/utf8>>;
% "nd" <<"Ndebele, North"/utf8>>;
% "ne" <<"Nepali"/utf8>>;
% "ng" <<"Ndonga"/utf8>>;
% "nr" <<"Ndebele, South"/utf8>>;
% "nv" <<"Navajo"/utf8>>;
% "ny" <<"Chichewa; Nyanja"/utf8>>;
% "oc" <<"Occitan; Provençal"/utf8>>;
% "om" <<"(Afan) Oromo"/utf8>>;
% "or" <<"Oriya"/utf8>>;
% "os" <<"Ossetian; Ossetic"/utf8>>;
% "pa" <<"Panjabi; Punjabi"/utf8>>;
% "pi" <<"Pali"/utf8>>;
% "qu" <<"Quechua"/utf8>>;
% "rm" <<"Rhaeto-Romance"/utf8>>;
% "rn" <<"Rundi; Kirundi"/utf8>>;
% "rw" <<"Kinyarwanda"/utf8>>;
% "sa" <<"Sanskrit"/utf8>>;
% "sc" <<"Sardinian"/utf8>>;
% "sd" <<"Sindhi"/utf8>>;
% "se" <<"Northern Sami"/utf8>>;
% "sg" <<"Sango; Sangro"/utf8>>;
% "si" <<"Sinhalese"/utf8>>;
% "sm" <<"Samoan"/utf8>>;
% "sn" <<"Shona"/utf8>>;
% "so" <<"Somali"/utf8>>;
% "sq" <<"Albanian"/utf8>>;
% "sr" <<"Serbian"/utf8>>;
% "ss" <<"Swati; Siswati"/utf8>>;
% "st" <<"Sesotho; Sotho, Southern"/utf8>>;
% "su" <<"Sundanese"/utf8>>;
% "sw" <<"Swahili"/utf8>>;
% "ta" <<"Tamil"/utf8>>;
% "te" <<"Telugu"/utf8>>;
% "tg" <<"Tajik"/utf8>>;
% "th" <<"Thai"/utf8>>;
% "ti" <<"Tigrinya"/utf8>>;
% "tk" <<"Turkmen"/utf8>>;
% "tl" <<"Tagalog"/utf8>>;
% "tn" <<"Tswana; Setswana"/utf8>>;
% "to" <<"Tonga"/utf8>>;
% "ts" <<"Tsonga"/utf8>>;
% "tt" <<"Tatar"/utf8>>;
% "tw" <<"Twi"/utf8>>;
% "ty" <<"Tahitian"/utf8>>;
% "ug" <<"Uighur"/utf8>>;
% "ur" <<"Urdu"/utf8>>;
% "uz" <<"Uzbek"/utf8>>;
% "vi" <<"Vietnamese"/utf8>>;
% "vo" <<"Volapuk"/utf8>>;
% "wa" <<"Walloon"/utf8>>;
% "wo" <<"Wolof"/utf8>>;
% "xh" <<"Xhosa"/utf8>>;
% "yi" <<"Yiddish"/utf8>>;
% "yo" <<"Yoruba"/utf8>>;
% "za" <<"Zhuang"/utf8>>;
% "zu" <<"Zulu"/utf8>>

