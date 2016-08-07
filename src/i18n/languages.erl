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
%%          http://www.unicode.org/iso15924/iso15924-codes.html

-export([
    lc2lang/1,
    languages/0,
    languages_stats/0
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


%% @doc Returns language data. Adds optional properties sub_languages and is_main:
%%      {sub_languages, list()}
%%      {is_main, boolean()}
-spec languages_stats() -> list().
languages_stats() ->
    Languages = languages(),
    Stats = lists:foldl(fun({Code, Props}, Acc=#language_data{}) ->
        Language = proplists:get_value(language, Props),
        Territory = proplists:get_value(territory, Props),
        Script = proplists:get_value(script, Props),
        case ((Territory /= undefined) or (Script /= undefined)) of
            false -> Acc;
            true ->
                D0 = Acc#language_data.sub_languages,
                D1 = case dict:is_key(Language, D0) of
                    true -> D0;
                    false -> dict:store(Language, [], D0)
                end,
                Acc#language_data{sub_languages=dict:append(Language, Code, D1)}
        end
    end, #language_data{}, Languages),
    SubLanguages = Stats#language_data.sub_languages,
    lists:map(fun(L) ->
        {Code, Props} = L,
        Language = proplists:get_value(language, Props),
        Props1 = [{is_main, Language == Code} | Props],
        case dict:is_key(Code, SubLanguages) of
            false ->
                {Code, Props1};
            true -> {Code, [{sub_languages, dict:fetch(Code, SubLanguages)} | Props1]}
        end
    end, Languages).


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
    {<<"ar">>, [
        {language, <<"ar">>},
        {direction, <<"RTL">>},
        {name, <<"العربية"/utf8>>},
        {name_en, <<"Arabic"/utf8>>}
    ]},
    {<<"de">>, [
        {language, <<"de">>},
        {direction, <<"LTR">>},
        {name, <<"Deutsch"/utf8>>},
        {name_en, <<"German"/utf8>>}
    ]},
    {<<"en">>, [
        {language, <<"en">>},
        {direction, <<"LTR">>},
        {name, <<"English"/utf8>>},
        {name_en, <<"English"/utf8>>}
    ]},
    {<<"es">>, [
        {language, <<"es">>},
        {direction, <<"LTR">>},
        {name, <<"español"/utf8>>},
        {name_en, <<"Spanish"/utf8>>}
    ]},
    {<<"et">>, [
        {language, <<"et">>},
        {direction, <<"LTR">>},
        {name, <<"eesti"/utf8>>},
        {name_en, <<"Estonian"/utf8>>}
    ]},
    {<<"fr">>, [
        {language, <<"fr">>},
        {direction, <<"LTR">>},
        {name, <<"français"/utf8>>},
        {name_en, <<"French"/utf8>>}
    ]},
    {<<"nl">>, [
        {language, <<"nl">>},
        {direction, <<"LTR">>},
        {name, <<"Nederlands"/utf8>>},
        {name_en, <<"Dutch"/utf8>>}
    ]},
    {<<"nl-be">>, [
        {language, <<"nl">>},
        {territory, <<"BE">>},
        {name, <<"Nederlands - België"/utf8>>},
        {name_en, <<"Dutch - Belgium"/utf8>>}
    ]},
    {<<"nl-nl">>, [
        {language, <<"nl">>},
        {territory, <<"NL">>},
        {name, <<"Nederlands - Nederland"/utf8>>},
        {name_en, <<"Dutch - The Netherlands"/utf8>>}
    ]},
    {<<"pl">>, [
        {language, <<"pl">>},
        {direction, <<"LTR">>},
        {name, <<"polszczyzna"/utf8>>},
        {name_en, <<"Polish"/utf8>>}
    ]},
    {<<"ru">>, [
        {language, <<"ru">>},
        {direction, <<"LTR">>},
        {name, <<"русский язык"/utf8>>},
        {name_en, <<"Russian"/utf8>>}
    ]},
    {<<"tr">>, [
        {language, <<"tr">>},
        {direction, <<"LTR">>},
        {name, <<"Türkçe"/utf8>>},
        {name_en, <<"Turkish"/utf8>>}
    ]},
    {<<"zh">>, [
        {language, <<"zh">>},
        {direction, <<"LTR">>},
        {name, <<"中文"/utf8>>},
        {name_en, <<"Chinese"/utf8>>}
    ]},
    {<<"zh-hans">>, [
        {language, <<"zh">>},
        {script, <<"Hans">>},
        {name, <<"Chinese (Simplified)"/utf8>>},
        {name_en, <<"Chinese (Simplified)"/utf8>>}
    ]},
    {<<"zh-hans-cn">>, [
        {language, <<"zh">>},
        {territory, <<"CN">>},
        {script, <<"Hans">>},
        {name, <<"Chinese (Simplified) - China"/utf8>>},
        {name_en, <<"Chinese (Simplified) - China"/utf8>>}
    ]},
    {<<"zh-hant">>, [
        {language, <<"zh">>},
        {script, <<"Hant">>},
        {name, <<"Chinese (Traditional)"/utf8>>},
        {name_en, <<"Chinese (Traditional)"/utf8>>}
    ]}
].
