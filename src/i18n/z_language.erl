%% @doc Mandatory background read on language tags: [1].
%%
%%      Some quotes from [1]:
%%
%%          The golden rule when creating language tags is to keep the tag as short as
%%          possible. Avoid region, script or other subtags except where they add useful
%%          distinguishing information. For instance, use 'ja' for Japanese and not
%%          'ja-JP', unless there is a particular reason that you need to say that this is
%%          Japanese as spoken in Japan, rather than elsewhere.
%%
%%          The entries in the registry follow certain conventions with regard to upper
%%          and lower letter-casing. For example, language tags are lower case, alphabetic
%%          region subtags are upper case, and script tags begin with an initial capital.
%%          This is only a convention!
%%
%%      Note that we use lower case subtags in subtag identifiers and URLs.
%%
%%          Language+extlang combinations are provided to accommodate legacy language tag
%%          forms, however, there is a single language subtag available for every
%%          language+extlang combination. That language subtag should be used rather than
%%          the language+extlang combination, where possible. For example, use 'yue'
%%          rather than 'zh-yue' for Cantonese, and 'afb' rather than 'ar-afb' for Gulf
%%          Arabic, if you can.

%%      Language identifiers can have the following forms:
%%      - language;
%%      - language-extlang;
%%      - language-region;
%%      - language-script;
%%      It is discouraged to use language-script-region, but it is possible if
%%      required.
%%      For a list of language, region and script codes, see [2].
%%      [1] http://www.w3.org/International/articles/language-tags/
%%      [2] http://www.iana.org/assignments/language-subtag-registry/language-subtag-registry
-module(z_language).

-export([
    default_language/1,
    is_valid/1,
    to_language_atom/1,
    fallback_language/2,
    english_name/1,
    is_rtl/1,
    properties/1,
    sort_properties/2,
    all_languages/0,
    main_languages/0
]).

-include("zotonic.hrl").

-define(DEFAULT_LANGUAGE, en).

%% @doc Returns the configured default language for this server; if not set, 'en'
%%      (English).
-spec default_language(#context{}) -> atom().
default_language(undefined) -> ?DEFAULT_LANGUAGE;
default_language(Context) ->
    z_convert:to_atom(m_config:get_value(i18n, language, ?DEFAULT_LANGUAGE, Context)).


%% @doc Check if the language code code is a valid language.
-spec is_valid(Code::binary() | any()) -> boolean().
is_valid(Code) when is_binary(Code) ->
    Languages = all_languages(),
    proplists:get_value(Code, Languages) /= undefined;
is_valid(Code) ->
    is_valid(z_convert:to_binary(Code)).


%% @doc Translate a language-code to an atom.
-spec to_language_atom(Code:: list() | binary()) -> {ok, atom()} | {error, not_a_language}.
to_language_atom(Code) when is_binary(Code) ->
    case is_valid(Code) of
        false -> {error, not_a_language};
        true -> {ok, z_convert:to_atom(Code)}
    end;
to_language_atom(Code) ->
    to_language_atom(z_convert:to_binary(Code)).


%% @doc Return the fallback language (the base language);  if no fallback language is
%%      found, returns the default language.
-spec fallback_language(Code::binary() | any() | undefined, #context{}) -> atom().
fallback_language(Code, Context) when Code =:= undefined ->
    default_language(Context);
fallback_language(Code, Context) when is_binary(Code) ->
    case proplists:get_value(Code, all_languages()) of
        undefined -> default_language(Context);
        LanguageData ->
            LanguageCode = proplists:get_value(language, LanguageData),
            case LanguageCode =:= Code of
                true -> default_language(Context);
                false -> binary_to_atom(LanguageCode, latin1)
            end
    end;
fallback_language(Code, Context) ->
    fallback_language(z_convert:to_binary(Code), Context).


%% @doc Returns the English language name.
-spec english_name(Code::atom()) -> binary() | undefined.
english_name(Code) ->
    get_property(Code, name_en).


%% @doc Check if the given language is a rtl language.
-spec is_rtl(Code::binary() | any()) -> boolean().
is_rtl(Code) ->
    get_property(Code, direction) == <<"RTL">>.


%% @doc Returns a list of properties from a language item retrieved from *all* languages.
%%      Proplists key: language code - this is the ISO 639-1 language code or otherwise
%%      the ISO 639-3, combined with region or script extension (lowercase).
%%      Properties:
%%      -   name: native language name.
%%      -   name_en: English language name.
%%      -   language: base language; functions as fallback language if translation
%%          is not available for the sub-language
%%      -   region (only for region variations): Alpha-2 code of country/region
%%          (ISO 3166-2).
%%      -   script (only for script variations): 4-letter script code (ISO 15924); if omitted: Latn.
%%      -   direction: (if omitted: LTR) or RTL.
-spec properties(Code::binary() | any()) -> list().
properties(Code) when is_binary(Code) ->
    Data = proplists:get_value(Code, all_languages()),
    properties(Code, Data);
properties(Code) ->
    properties(z_convert:to_binary(Code)).


%% @private
-spec properties(Code::binary() | any(), list()) -> list().
properties(Code, Data) when is_binary(Code) ->
    [
        {language, proplists:get_value(language, Data)},
        {direction, proplists:get_value(direction, Data)},
        {name, proplists:get_value(name, Data)},
        {name_en, proplists:get_value(name_en, Data)},
        {region, proplists:get_value(region, Data)},
        {script, proplists:get_value(script, Data)},
        {sublanguages, sort_properties(lists:map(fun({SubCode, SubData}) ->
            {SubCode, properties(SubCode, SubData)}
        end, proplists:get_value(sublanguages, Data, [])), name_en)}
    ];
properties(Code, Data) ->
    properties(z_convert:to_binary(Code), Data).


%% @doc Sorts a properties list.
-spec sort_properties(List::list(), SortKey::string()) -> list().
sort_properties(List, SortKey) ->
    lists:sort(fun({_, PropsA}, {_, PropsB}) ->
        z_string:to_lower(proplists:get_value(SortKey, PropsA)) =< z_string:to_lower(proplists:get_value(SortKey, PropsB))
    end, List).


%% @doc List of language data.
%%      Returns a flattened list of property lists; sub-languages are added to the list of
%%      main languages.
%%      For each language a property list is returned - see properties/1.
-spec all_languages() -> list().
all_languages() ->
    all_languages1(languages()).
all_languages1(List) ->
    lists:foldl(fun({Code, Data}, Acc) ->
        Language1 = {Code, properties(Code, Data)},
        case proplists:get_value(sublanguages, Data) of
            undefined ->
                [Language1|Acc];
            SubLanguages ->
                [Language1|Acc ++ all_languages1(SubLanguages)]
        end
    end, [], List).


%% @doc Flat list of language data of main languages.
-spec main_languages() -> list().
main_languages() ->
    lists:foldl(fun({Code, Data}, Acc) ->
        [{Code, properties(Code, Data)}|Acc]
    end, [], languages()).


%% @private
%% Gets a property from an item retrieved from *all* languages.
-spec get_property(Code::binary() | any(), Key:: atom()) -> binary() | undefined.
get_property(Code, Key) ->
    get_property_from_list(Code, Key, all_languages()).


%% @private
%% Gets a property from an item retrieved from specified list
-spec get_property_from_list(Code::binary() | any(), Key:: atom(), List:: list()) -> binary() | list() | undefined.
get_property_from_list(Code, Key, List) when is_binary(Code) ->
    case proplists:get_value(Code, List) of
        undefined -> undefined;
        Data -> proplists:get_value(Key, Data)
    end;
get_property_from_list(Code, Key, List) ->
    get_property_from_list(z_convert:to_binary(Code), Key, List).


%% @private
-spec languages() -> list().
languages() -> [
    {<<"aa">>, [
        {language, <<"aa">>},
        {name, <<"Qafaraf"/utf8>>},
        {name_en, <<"Afar"/utf8>>}
    ]},
    {<<"ab">>, [
        {language, <<"ab">>},
        {script, <<"Cyrl">>},
        {region, <<"GE">>},
        {name, <<"Аҧсуа бызшәа"/utf8>>},
        {name_en, <<"Abkhazian"/utf8>>}
    ]},
    {<<"af">>, [
        {language, <<"af">>},
        {name, <<"Afrikaans"/utf8>>},
        {name_en, <<"Afrikaans"/utf8>>}
    ]},
    {<<"am">>, [
        {language, <<"am">>},
        {script, <<"Ethi">>},
        {region, <<"ET">>},
        {name, <<"አማርኛ"/utf8>>},
        {name_en, <<"Amharic"/utf8>>}
    ]},
    {<<"ar">>, [
        {type, <<"macro_language">>},
        {language, <<"ar">>},
        {direction, <<"RTL">>},
        {script, <<"Arab">>},
        {name, <<"العربية"/utf8>>},
        {name_en, <<"Arabic"/utf8>>},
        {sublanguages, [
            {<<"arb">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {script, <<"Arab">>},
                {name, <<"اللغة العربية الفصحى"/utf8>>},
                {name_en, <<"Standard Arabic"/utf8>>}
            ]},
            {<<"afb">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {script, <<"Arab">>},
                {name, <<"العربية - الخليج"/utf8>>},
                {name_en, <<"Arabic - Gulf"/utf8>>}
            ]},
            {<<"ajp">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {script, <<"Arab">>},
                {name, <<"العربية - جنوب بلاد الشام"/utf8>>},
                {name_en, <<"Arabic - South Levant"/utf8>>}
            ]},
            {<<"apc">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {script, <<"Arab">>},
                {name, <<"العربية - شمال بلاد الشام"/utf8>>},
                {name_en, <<"Arabic - North Levant"/utf8>>}
            ]},
            {<<"apd">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {script, <<"Arab">>},
                {name, <<"العربية - السودان"/utf8>>},
                {name_en, <<"Arabic - Sudan"/utf8>>}
            ]},
            {<<"ar-ae">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {region, <<"AE">>},
                {script, <<"Arab">>},
                {name, <<"العربية - الإمارات العربية المتحدة."/utf8>>},
                {name_en, <<"Arabic - U.A.E."/utf8>>}
            ]},
            {<<"ar-bh">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {region, <<"BH">>},
                {script, <<"Arab">>},
                {name, <<"العربية - البحرين"/utf8>>},
                {name_en, <<"Arabic - Bahrain"/utf8>>}
            ]},
            {<<"aao">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {region, <<"DZ">>},
                {script, <<"Arab">>},
                {name, <<"العربية الصحراء الجزائرية"/utf8>>},
                {name_en, <<"Arabic - Algerian Sahara"/utf8>>}
            ]},
            {<<"ary">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {region, <<"DZ">>},
                {script, <<"Arab">>},
                {name, <<"العربية - المغرب"/utf8>>},
                {name_en, <<"Arabic - Marocco"/utf8>>}
            ]},
            {<<"arz">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {region, <<"EG">>},
                {script, <<"Arab">>},
                {name, <<"مصر"/utf8>>},
                {name_en, <<"Arabic - Egypt"/utf8>>}
            ]},
            {<<"ar-iq">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {region, <<"IQ">>},
                {script, <<"Arab">>},
                {name, <<"العربية - مصر"/utf8>>},
                {name_en, <<"Arabic - Iraq"/utf8>>}
            ]},
            {<<"ar-jo">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {region, <<"JO">>},
                {script, <<"Arab">>},
                {name, <<"العربية - الأردن"/utf8>>},
                {name_en, <<"Arabic - Jordan"/utf8>>}
            ]},
            {<<"ar-kw">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {region, <<"KW">>},
                {script, <<"Arab">>},
                {name, <<"العربية - الكويت"/utf8>>},
                {name_en, <<"Arabic - Kuwait"/utf8>>}
            ]},
            {<<"ar-lb">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {region, <<"LB">>},
                {script, <<"Arab">>},
                {name, <<"العربية - لبنان"/utf8>>},
                {name_en, <<"Arabic - Lebanon"/utf8>>}
            ]},
            {<<"ayl">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {region, <<"LY">>},
                {script, <<"Arab">>},
                {name, <<"العربية - ليبيا"/utf8>>},
                {name_en, <<"Arabic - Libya"/utf8>>}
            ]},
            {<<"ar-ma">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {region, <<"MA">>},
                {script, <<"Arab">>},
                {name, <<"العربية - المغرب"/utf8>>},
                {name_en, <<"Arabic - Morocco"/utf8>>}
            ]},
            {<<"acx">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {region, <<"OM">>},
                {script, <<"Arab">>},
                {name, <<"العربية - عمان"/utf8>>},
                {name_en, <<"Arabic - Oman"/utf8>>}
            ]},
            {<<"ar-qa">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {region, <<"QA">>},
                {script, <<"Arab">>},
                {name, <<"العربية - قطر"/utf8>>},
                {name_en, <<"Arabic - Qatar"/utf8>>}
            ]},
            {<<"ar-sa">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {region, <<"SA">>},
                {script, <<"Arab">>},
                {name, <<"العربية - المملكة العربية السعودية"/utf8>>},
                {name_en, <<"Arabic - Saudi Arabia"/utf8>>}
            ]},
            {<<"ar-sy">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {region, <<"SY">>},
                {script, <<"Arab">>},
                {name, <<"العربية - سوريا"/utf8>>},
                {name_en, <<"Arabic - Syria"/utf8>>}
            ]},
            {<<"aeb">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {region, <<"TN">>},
                {script, <<"Arab">>},
                {name, <<"العربية - تونس"/utf8>>},
                {name_en, <<"Arabic - Tunisia"/utf8>>}
            ]},
            {<<"ar-ye">>, [
                {language, <<"ar">>},
                {direction, <<"RTL">>},
                {region, <<"YE">>},
                {script, <<"Arab">>},
                {name, <<"العربية - اليمن"/utf8>>},
                {name_en, <<"Arabic - Yemen"/utf8>>}
            ]}
        ]}
    ]},
    {<<"as">>, [
        {language, <<"as">>},
        {script, <<"Beng">>},
        {name, <<"অসমীয়া"/utf8>>},
        {name_en, <<"Assamese"/utf8>>}
    ]},
    {<<"ay">>, [
        {language, <<"ay">>},
        {name, <<"Aymar aru"/utf8>>},
        {name_en, <<"Aymara"/utf8>>}
    ]},
    {<<"az">>, [
        {type, <<"macrolanguage">>},
        {language, <<"az">>},
        {name, <<"azərbaycan dili"/utf8>>},
        {name_en, <<"Azerbaijani"/utf8>>}
    ]},
    {<<"ba">>, [
        {language, <<"ba">>},
        {script, <<"Cyrl">>},
        {region, <<"RU">>},
        {name, <<"башҡорт теле"/utf8>>},
        {name_en, <<"Bashkir"/utf8>>}
    ]},
    {<<"be">>, [
        {language, <<"be">>},
        {script, <<"Cyrl">>},
        {region, <<"BY">>},
        {name, <<"беларуская"/utf8>>},
        {name_en, <<"Byelorussian"/utf8>>}
    ]},
    {<<"bg">>, [
        {language, <<"bg">>},
        {script, <<"Cyrl">>},
        {region, <<"BG">>},
        {name, <<"български"/utf8>>},
        {name_en, <<"Bulgarian"/utf8>>}
    ]},
    %% Omitting "bh", which is "Bihari languages", a collection
    {<<"bi">>, [
        {language, <<"bi">>},
        {region, <<"VU">>},
        {name, <<"Bislama"/utf8>>},
        {name_en, <<"Bislama"/utf8>>}
    ]},
    {<<"bn">>, [
        {language, <<"bn">>},
        {script, <<"Beng">>},
        {name, <<"বাংলা"/utf8>>},
        {name_en, <<"Bengali"/utf8>>}
    ]},
    {<<"bo">>, [
        {language, <<"bo">>},
        {script, <<"Tibt">>},
        {name, <<"བོད་སྐད"/utf8>>},
        {name_en, <<"Tibetan"/utf8>>}
    ]},
    {<<"br">>, [
        {language, <<"br">>},
        {name, <<"brezhoneg"/utf8>>},
        {name_en, <<"Breton"/utf8>>}
    ]},
    {<<"bs">>, [
        {language, <<"bs">>},
        {region, <<"BA">>},
        {name, <<"bosanski"/utf8>>},
        {name_en, <<"Bosnian"/utf8>>}
    ]},
    {<<"ca">>, [
        {language, <<"ca">>},
        {region, <<"AD">>},
        {name, <<"català"/utf8>>},
        {name_en, <<"Catalan"/utf8>>}
    ]},
    {<<"ce">>, [
        {language, <<"ce">>},
        {script, <<"Cyrl">>},
        {region, <<"RU">>},
        {name, <<"нохчийн"/utf8>>},
        {name_en, <<"Chechen"/utf8>>}
    ]},
    {<<"ch">>, [
        {language, <<"ch">>},
        {region, <<"GU">>},
        {name, <<"Finu' Chamoru"/utf8>>},
        {name_en, <<"Chamorro"/utf8>>}
    ]},
    {<<"co">>, [
        {language, <<"co">>},
        {name, <<"Corsu"/utf8>>},
        {name_en, <<"Corsican"/utf8>>}
    ]},
    {<<"cs">>, [
        {language, <<"cs">>},
        {region, <<"CZ">>},
        {name, <<"čeština"/utf8>>},
        {name_en, <<"Czech"/utf8>>}
    ]},
    %% Omitting Church Slavic
    {<<"cv">>, [
        {language, <<"cv">>},
        {region, <<"RU">>},
        {script, <<"Cyrl">>},
        {name, <<"Чӑвашла"/utf8>>},
        {name_en, <<"Chuvash"/utf8>>}
    ]},
    {<<"cy">>, [
        {language, <<"cy">>},
        {region, <<"GB">>},
        {name, <<"Cymraeg"/utf8>>},
        {name_en, <<"Welsh"/utf8>>}
    ]},
    {<<"de">>, [
        {language, <<"de">>},
        {name, <<"Deutsch"/utf8>>},
        {name_en, <<"German"/utf8>>},
        {sublanguages, [
            {<<"de-at">>, [
                {language, <<"de">>},
                {region, <<"AT">>},
                {name, <<"Deutsch - Österreich"/utf8>>},
                {name_en, <<"German - Austria"/utf8>>}
            ]},
            {<<"de-ch">>, [
                {language, <<"de">>},
                {region, <<"CH">>},
                {name, <<"Deutsch - Schweiz"/utf8>>},
                {name_en, <<"German - Switzerland"/utf8>>}
            ]},
            {<<"de-de">>, [
                {language, <<"de">>},
                {region, <<"DE">>},
                {name, <<"Deutsch - Deutschland"/utf8>>},
                {name_en, <<"German - Germany"/utf8>>}
            ]},
            {<<"de-li">>, [
                {language, <<"de">>},
                {region, <<"LI">>},
                {name, <<"Deutsch - Liechtenstein"/utf8>>},
                {name_en, <<"German - Liechtenstein"/utf8>>}
            ]},
            {<<"de-lu">>, [
                {language, <<"de">>},
                {region, <<"LU">>},
                {name, <<"Deutsch - Luxemburg"/utf8>>},
                {name_en, <<"German - Luxembourg"/utf8>>}
            ]}
        ]}
    ]},
    {<<"da">>, [
        {language, <<"da">>},
        {name, <<"dansk"/utf8>>},
        {name_en, <<"Danish"/utf8>>}
    ]},
    {<<"dz">>, [
        {language, <<"dz">>},
        {script, <<"Tibt">>},
        {region, <<"BT">>},
        {name, <<"རྫོང་ཁ་"/utf8>>},
        {name_en, <<"Dzongkha"/utf8>>}
    ]},
    {<<"el">>, [
        {language, <<"el">>},
        {script, <<"Grek">>},
        {name, <<"Ελληνικά"/utf8>>},
        {name_en, <<"Greek"/utf8>>}
    ]},
    {<<"en">>, [
        {language, <<"en">>},
        {name, <<"English"/utf8>>},
        {name_en, <<"English"/utf8>>},
        {sublanguages, [
            {<<"en-au">>, [
                {language, <<"en">>},
                {region, <<"AU">>},
                {name, <<"English - Australia"/utf8>>},
                {name_en, <<"English - Australia"/utf8>>}
            ]},
            {<<"en-bz">>, [
                {language, <<"en">>},
                {region, <<"BZ">>},
                {name, <<"English - Belize"/utf8>>},
                {name_en, <<"English - Belize"/utf8>>}
            ]},
            {<<"en-ca">>, [
                {language, <<"en">>},
                {region, <<"CA">>},
                {name, <<"English - Canada"/utf8>>},
                {name_en, <<"English - Canada"/utf8>>}
            ]},
            {<<"en-cb">>, [
                {language, <<"en">>},
                {region, <<"CB">>},
                {name, <<"English - Caribbean"/utf8>>},
                {name_en, <<"English - Caribbean"/utf8>>}
            ]},
            {<<"en-gb">>, [
                {language, <<"en">>},
                {region, <<"GB">>},
                {name, <<"English - United Kingdom"/utf8>>},
                {name_en, <<"English - United Kingdom"/utf8>>}
            ]},
            {<<"en-ie">>, [
                {language, <<"en">>},
                {region, <<"IE">>},
                {name, <<"English - Ireland"/utf8>>},
                {name_en, <<"English - Ireland"/utf8>>}
            ]},
            {<<"en-jm">>, [
                {language, <<"en">>},
                {region, <<"JM">>},
                {name, <<"English - Jamaica"/utf8>>},
                {name_en, <<"English - Jamaica"/utf8>>}
            ]},
            {<<"en-nz">>, [
                {language, <<"en">>},
                {region, <<"NZ">>},
                {name, <<"English - New Zealand"/utf8>>},
                {name_en, <<"English - New Zealand"/utf8>>}
            ]},
            {<<"en-ph">>, [
                {language, <<"en">>},
                {region, <<"PH">>},
                {name, <<"English - Republic of the Philippines"/utf8>>},
                {name_en, <<"English - Republic of the Philippines"/utf8>>}
            ]},
            {<<"en-tt">>, [
                {language, <<"en">>},
                {region, <<"TT">>},
                {name, <<"English - Trinidad and Tobago"/utf8>>},
                {name_en, <<"English - Trinidad and Tobago"/utf8>>}
            ]},
            {<<"en-us">>, [
                {language, <<"en">>},
                {region, <<"US">>},
                {name, <<"English - United States"/utf8>>},
                {name_en, <<"English - United States"/utf8>>}
            ]},
            {<<"en-za">>, [
                {language, <<"en">>},
                {region, <<"ZA">>},
                {name, <<"English - South Africa"/utf8>>},
                {name_en, <<"English - South Africa"/utf8>>}
            ]},
            {<<"en-zw">>, [
                {language, <<"en">>},
                {region, <<"ZW">>},
                {name, <<"English - Zimbabwe"/utf8>>},
                {name_en, <<"English - Zimbabwe"/utf8>>}
            ]}
        ]}
    ]},
    {<<"eo">>, [
        {language, <<"eo">>},
        {name, <<"Esperanto"/utf8>>},
        {name_en, <<"Esperanto"/utf8>>}
    ]},
    {<<"es">>, [
        {language, <<"es">>},
        {name, <<"español"/utf8>>},
        {name_en, <<"Spanish"/utf8>>},
        {sublanguages, [
            {<<"es-419">>, [
                {language, <<"es">>},
                {region, <<"419">>},
                {name, <<"español latinoamericano"/utf8>>},
                {name_en, <<"Spanish - Latin America and the Caribbean"/utf8>>}
            ]},
            {<<"es-ar">>, [
                {language, <<"es">>},
                {region, <<"AR">>},
                {name, <<"español - Argentina"/utf8>>},
                {name_en, <<"Spanish - Argentina"/utf8>>}
            ]},
            {<<"es-bo">>, [
                {language, <<"es">>},
                {region, <<"BO">>},
                {name, <<"español - Bolivia"/utf8>>},
                {name_en, <<"Spanish - Bolivia"/utf8>>}
            ]},
            {<<"es-cl">>, [
                {language, <<"es">>},
                {region, <<"CL">>},
                {name, <<"español - Chile"/utf8>>},
                {name_en, <<"Spanish - Chile"/utf8>>}
            ]},
            {<<"es-co">>, [
                {language, <<"es">>},
                {region, <<"CO">>},
                {name, <<"español - Colombia"/utf8>>},
                {name_en, <<"Spanish - Colombia"/utf8>>}
            ]},
            {<<"es-cr">>, [
                {language, <<"es">>},
                {region, <<"CR">>},
                {name, <<"español - Costa Rica"/utf8>>},
                {name_en, <<"Spanish - Costa Rica"/utf8>>}
            ]},
            {<<"es-do">>, [
                {language, <<"es">>},
                {region, <<"DO">>},
                {name, <<"español - República Dominicana"/utf8>>},
                {name_en, <<"Spanish - Dominican Republic"/utf8>>}
            ]},
            {<<"es-ec">>, [
                {language, <<"es">>},
                {region, <<"EC">>},
                {name, <<"español - Ecuador"/utf8>>},
                {name_en, <<"Spanish - Ecuador"/utf8>>}
            ]},
            {<<"es-es">>, [
                {language, <<"es">>},
                {region, <<"ES">>},
                {name, <<"español - España"/utf8>>},
                {name_en, <<"Spanish - Spain"/utf8>>}
            ]},
            {<<"es-gt">>, [
                {language, <<"es">>},
                {region, <<"GT">>},
                {name, <<"español - Guatemala"/utf8>>},
                {name_en, <<"Spanish - Guatemala"/utf8>>}
            ]},
            {<<"es-hn">>, [
                {language, <<"es">>},
                {region, <<"HN">>},
                {name, <<"español - Honduras"/utf8>>},
                {name_en, <<"Spanish - Honduras"/utf8>>}
            ]},
            {<<"es-mx">>, [
                {language, <<"es">>},
                {region, <<"MX">>},
                {name, <<"español - México"/utf8>>},
                {name_en, <<"Spanish - Mexico"/utf8>>}
            ]},
            {<<"es-ni">>, [
                {language, <<"es">>},
                {region, <<"NI">>},
                {name, <<"español - Nicaragua"/utf8>>},
                {name_en, <<"Spanish - Nicaragua"/utf8>>}
            ]},
            {<<"es-pa">>, [
                {language, <<"es">>},
                {region, <<"PA">>},
                {name, <<"español - Panamá"/utf8>>},
                {name_en, <<"Spanish - Panama"/utf8>>}
            ]},
            {<<"es-pe">>, [
                {language, <<"es">>},
                {region, <<"PE">>},
                {name, <<"español - Perú"/utf8>>},
                {name_en, <<"Spanish - Peru"/utf8>>}
            ]},
            {<<"es-pr">>, [
                {language, <<"es">>},
                {region, <<"PR">>},
                {name, <<"español - Puerto Rico"/utf8>>},
                {name_en, <<"Spanish - Puerto Rico"/utf8>>}
            ]},
            {<<"es-py">>, [
                {language, <<"es">>},
                {region, <<"PY">>},
                {name, <<"español - Paraguay"/utf8>>},
                {name_en, <<"Spanish - Paraguay"/utf8>>}
            ]},
            {<<"es-sv">>, [
                {language, <<"es">>},
                {region, <<"SV">>},
                {name, <<"español - El Salvador"/utf8>>},
                {name_en, <<"Spanish - El Salvador"/utf8>>}
            ]},
            {<<"es-uy">>, [
                {language, <<"es">>},
                {region, <<"UY">>},
                {name, <<"español - Uruguay"/utf8>>},
                {name_en, <<"Spanish - Uruguay"/utf8>>}
            ]},
            {<<"es-ve">>, [
                {language, <<"es">>},
                {region, <<"VE">>},
                {name, <<"español - Venezuela"/utf8>>},
                {name_en, <<"Spanish - Venezuela"/utf8>>}
            ]}
        ]}
    ]},
    {<<"et">>, [
        {language, <<"et">>},
        {region, <<"EE">>},
        {name, <<"eesti"/utf8>>},
        {name_en, <<"Estonian"/utf8>>}
    ]},
    {<<"eu">>, [
        {language, <<"eu">>},
        {region, <<"ES">>},
        {name, <<"Euskara"/utf8>>},
        {name_en, <<"Basque"/utf8>>}
    ]},
    {<<"fa">>, [
        {language, <<"fa">>},
        {script, <<"Arab">>},
        {name, <<"فارسی"/utf8>>},
        {name_en, <<"Persian"/utf8>>}
    ]},
    {<<"fi">>, [
        {language, <<"fi">>},
        {name, <<"suomi"/utf8>>},
        {name_en, <<"Finnish"/utf8>>}
    ]},
    {<<"fj">>, [
        {language, <<"fj">>},
        {region, <<"FJ">>},
        {name, <<"Na Vosa Vakaviti"/utf8>>},
        {name_en, <<"Fijian"/utf8>>}
    ]},
    {<<"fr">>, [
        {language, <<"fr">>},
        {name, <<"français"/utf8>>},
        {name_en, <<"French"/utf8>>},
        {sublanguages, [
            {<<"fr-be">>, [
                {language, <<"fr">>},
                {region, <<"BE">>},
                {name, <<"Français - Belgique"/utf8>>},
                {name_en, <<"French - Belgium"/utf8>>}
            ]},
            {<<"fr-ca">>, [
                {language, <<"fr">>},
                {region, <<"CA">>},
                {name, <<"Français - Canada"/utf8>>},
                {name_en, <<"French - Canada"/utf8>>}
            ]},
            {<<"fr-ch">>, [
                {language, <<"fr">>},
                {region, <<"CH">>},
                {name, <<"Français - Suisse"/utf8>>},
                {name_en, <<"French - Switzerland"/utf8>>}
            ]},
            {<<"fr-fr">>, [
                {language, <<"fr">>},
                {region, <<"FR">>},
                {name, <<"Français - France"/utf8>>},
                {name_en, <<"French - France"/utf8>>}
            ]},
            {<<"fr-lu">>, [
                {language, <<"fr">>},
                {region, <<"LU">>},
                {name, <<"Français - Luxembourg"/utf8>>},
                {name_en, <<"French - Luxembourg"/utf8>>}
            ]},
            {<<"fr-mc">>, [
                {language, <<"fr">>},
                {region, <<"MC">>},
                {name, <<"Français - Monaco"/utf8>>},
                {name_en, <<"French - Monaco"/utf8>>}
            ]}
        ]}
    ]},
    {<<"fo">>, [
        {language, <<"fo">>},
        {region, <<"FO">>},
        {name, <<"føroyskt"/utf8>>},
        {name_en, <<"Faroese"/utf8>>}
    ]},
    {<<"fy">>, [
        {language, <<"fy">>},
        {region, <<"NL">>},
        {name, <<"West-Frysk"/utf8>>},
        {name_en, <<"Frisian"/utf8>>}
    ]},
    {<<"ga">>, [
        {language, <<"ga">>},
        {name, <<"Gaeilge"/utf8>>},
        {name_en, <<"Gaelic"/utf8>>}
    ]},
    {<<"gd">>, [
        {language, <<"gd">>},
        {region, <<"GB">>},
        {name, <<"Gàidhlig"/utf8>>},
        {name_en, <<"Scottish Gaelic"/utf8>>}
    ]},
    {<<"gl">>, [
        {language, <<"gl">>},
        {region, <<"ES">>},
        {name, <<"galego"/utf8>>},
        {name_en, <<"Galician"/utf8>>}
    ]},
    {<<"gn">>, [
        {language, <<"gn">>},
        {region, <<"PY">>},
        {name, <<"avañe'ẽ"/utf8>>},
        {name_en, <<"Guarani"/utf8>>}
    ]},
    {<<"gu">>, [
        {language, <<"gu">>},
        {script, <<"Gujr">>},
        {name, <<"ગુજરાતી"/utf8>>},
        {name_en, <<"Gujarati"/utf8>>}
    ]},
    {<<"he">>, [
        {language, <<"he">>},
        {direction, <<"RTL">>},
        {script, <<"Hebr">>},
        {region, <<"IL">>},
        {name, <<"עברית"/utf8>>},
        {name_en, <<"Hebrew"/utf8>>}
    ]},
    {<<"hi">>, [
        {language, <<"hi">>},
        {script, <<"Deva">>},
        {name, <<"हिन्दी"/utf8>>},
        {name_en, <<"Hindi"/utf8>>}
    ]},
    {<<"hr">>, [
        {language, <<"hr">>},
        {name, <<"hrvatski"/utf8>>},
        {name_en, <<"Croatian"/utf8>>},
        {sublanguages, [
            {<<"hr-ba">>, [
                {language, <<"hr">>},
                {region, <<"BA">>},
                {name, <<"hrvatski - Bosna i Hercegovina"/utf8>>},
                {name_en, <<"Croatian - Bosnia and Herzegovina"/utf8>>}
            ]},
            {<<"hr-hr">>, [
                {language, <<"hr">>},
                {region, <<"HR">>},
                {name, <<"hrvatski - Hrvatska"/utf8>>},
                {name_en, <<"Croatian - Croatia"/utf8>>}
            ]}
        ]}
    ]},
    {<<"hu">>, [
        {language, <<"hu">>},
        {name, <<"magyar"/utf8>>},
        {name_en, <<"Hungarian"/utf8>>}
    ]},
    {<<"id">>, [
        {language, <<"id">>},
        {region, <<"ID">>},
        {name, <<"Indonesia"/utf8>>},
        {name_en, <<"Indonesian"/utf8>>}
    ]},
    {<<"ia">>, [
        {language, <<"ia">>},
        {name, <<"Interlingua"/utf8>>},
        {name_en, <<"Interlingua"/utf8>>}
    ]},
    {<<"is">>, [
        {language, <<"is">>},
        {region, <<"IS">>},
        {name, <<"íslenska"/utf8>>},
        {name_en, <<"Islandic"/utf8>>}
    ]},
    {<<"it">>, [
        {language, <<"it">>},
        {name, <<"italiano"/utf8>>},
        {name_en, <<"Italian"/utf8>>},
        {sublanguages, [
            {<<"it-ch">>, [
                {language, <<"it">>},
                {region, <<"CH">>},
                {name, <<"italiano - Svizzera"/utf8>>},
                {name_en, <<"Italian - Switzerland"/utf8>>}
            ]},
            {<<"it-it">>, [
                {language, <<"it">>},
                {region, <<"IT">>},
                {name, <<"italiano - Italia"/utf8>>},
                {name_en, <<"Italian - Italy"/utf8>>}
            ]}
        ]}
    ]},
    {<<"ja">>, [
        {language, <<"ja">>},
        {script, <<"Jpan">>}, % alias for Han + Hiragana + Katakana
        {name, <<"日本語"/utf8>>},
        {name_en, <<"Japanese"/utf8>>}
    ]},
    {<<"jv">>, [
        {language, <<"jv">>},
        {region, <<"ID">>},
        {name, <<"basa jawa"/utf8>>},
        {name_en, <<"Javanese"/utf8>>}
    ]},
    {<<"ka">>, [
        {language, <<"ka">>},
        {script, <<"Geor">>},
        {region, <<"GE">>},
        {name, <<"ქართული"/utf8>>},
        {name_en, <<"Georgian"/utf8>>}
    ]},
    {<<"ko">>, [
        {language, <<"ko">>},
        {script, <<"Kore">>},
        {name, <<"한국어"/utf8>>},
        {name_en, <<"Korean"/utf8>>}
    ]},
    {<<"lt">>, [
        {language, <<"lt">>},
        {name, <<"lietuvių"/utf8>>},
        {name_en, <<"Lithuanian"/utf8>>}
    ]},
    {<<"lv">>, [
        {language, <<"lv">>},
        {region, <<"LV">>},
        {name, <<"latviešu"/utf8>>},
        {name_en, <<"Latvian"/utf8>>}
    ]},
    {<<"mg">>, [
        {language, <<"mg">>},
        {region, <<"MG">>},
        {name, <<"Malagasy"/utf8>>},
        {name_en, <<"Malagasy"/utf8>>}
    ]},
    {<<"mk">>, [
        {language, <<"mk">>},
        {region, <<"MK">>},
        {script, <<"Cyrl">>},
        {name, <<"македонски"/utf8>>},
        {name_en, <<"Macedonian"/utf8>>}
    ]},
    {<<"mn">>, [
        {language, <<"mn">>},
        {name, <<"монгол"/utf8>>},
        {name_en, <<"Mongolian"/utf8>>}
    ]},
    {<<"mt">>, [
        {language, <<"mt">>},
        {region, <<"MT">>},
        {name, <<"Malti"/utf8>>},
        {name_en, <<"Maltese"/utf8>>}
    ]},
    {<<"nl">>, [
        {language, <<"nl">>},
        {name, <<"Nederlands"/utf8>>},
        {name_en, <<"Dutch"/utf8>>},
        {sublanguages, [
            {<<"nl-be">>, [
                {language, <<"nl">>},
                {region, <<"BE">>},
                {name, <<"Vlaams - België"/utf8>>},
                {name_en, <<"Flemish - Belgium"/utf8>>}
            ]},
            {<<"nl-nl">>, [
                {language, <<"nl">>},
                {region, <<"NL">>},
                {name, <<"Nederlands - Nederland"/utf8>>},
                {name_en, <<"Dutch - Netherlands"/utf8>>}
            ]}
        ]}
    ]},
    {<<"no">>, [
        {language, <<"no">>},
        {name, <<"norsk"/utf8>>},
        {name_en, <<"Norwegian"/utf8>>}
    ]},
    {<<"nn">>, [
        {language, <<"nn">>},
        {region, <<"NO">>},
        {name, <<"nynorsk"/utf8>>},
        {name_en, <<"Norwegian Nynorsk"/utf8>>}
    ]},
    {<<"pa">>, [
        {language, <<"pa">>},
        {script, <<"Arab">>},
        {name, <<"ਪੰਜਾਬੀ"/utf8>>},
        {name_en, <<"Punjabi"/utf8>>},
        {sublanguages, [
            {<<"pa-arab">>, [
                {language, <<"pa">>},
                {script, <<"Arab">>},
                {name, <<"ابی"/utf8>>},
                {name_en, <<"Punjabi - Arab"/utf8>>}
            ]},
            {<<"pa-guru">>, [
                {language, <<"pa">>},
                {script, <<"Guru">>},
                {name, <<"ਪੰਜਾਬੀ ਦੇ - ਗੁਰਮੁਖੀ"/utf8>>},
                {name_en, <<"Punjabi - Arab"/utf8>>}
            ]}
        ]}
    ]},
    {<<"pl">>, [
        {language, <<"pl">>},
        {region, <<"PL">>},
        {name, <<"polszczyzna"/utf8>>},
        {name_en, <<"Polish"/utf8>>}
    ]},
    {<<"ps">>, [
        {language, <<"ps">>},
        {script, <<"Arab">>},
        {name, <<"تو"/utf8>>},
        {name_en, <<"Pashto"/utf8>>}
    ]},
    {<<"pt">>, [
        {language, <<"pt">>},
        {name, <<"português"/utf8>>},
        {name_en, <<"Portuguese"/utf8>>},
        {sublanguages, [
            {<<"pt-br">>, [
                {language, <<"pt">>},
                {region, <<"BR">>},
                {name, <<"português - Brasil"/utf8>>},
                {name_en, <<"Portuguese - Brazil"/utf8>>}
            ]},
            {<<"pt-pt">>, [
                {language, <<"pt">>},
                {region, <<"PT">>},
                {name, <<"português - Portugal"/utf8>>},
                {name_en, <<"Portuguese - Portugal"/utf8>>}
            ]}
        ]}
    ]},
    {<<"ro">>, [
        {language, <<"ro">>},
        {name, <<"română"/utf8>>},
        {name_en, <<"Romanian"/utf8>>}
    ]},
    {<<"ru">>, [
        {language, <<"ru">>},
        {script, <<"Cyrl">>},
        {name, <<"русский язык"/utf8>>},
        {name_en, <<"Russian"/utf8>>}
    ]},
    {<<"sk">>, [
        {language, <<"sk">>},
        {name, <<"slovenčina"/utf8>>},
        {name_en, <<"Slovak"/utf8>>}
    ]},
    {<<"sl">>, [
        {language, <<"sl">>},
        {name, <<"slovenščina"/utf8>>},
        {name_en, <<"Slovenian"/utf8>>}
    ]},
    {<<"sr">>, [
        {language, <<"sr">>},
        {script, <<"Cyrl">>},
        {name, <<"српски"/utf8>>},
        {name_en, <<"Serbian"/utf8>>}
    ]},
    {<<"sv">>, [
        {language, <<"sv">>},
        {name, <<"svenska"/utf8>>},
        {name_en, <<"Swedish"/utf8>>}
    ]},
    {<<"sq">>, [
        {language, <<"sq">>},
        {name, <<"shqip"/utf8>>},
        {name_en, <<"Albanian"/utf8>>}
    ]},
    {<<"th">>, [
        {language, <<"th">>},
        {script, <<"Thai">>},
        {name, <<"ไทย"/utf8>>},
        {name_en, <<"Thai"/utf8>>}
    ]},
    {<<"tr">>, [
        {language, <<"tr">>},
        {name, <<"Türkçe"/utf8>>},
        {name_en, <<"Turkish"/utf8>>}
    ]},
    {<<"uk">>, [
        {language, <<"uk">>},
        {script, <<"Cyrl">>},
        {name, <<"українська"/utf8>>},
        {name_en, <<"Ukrainian"/utf8>>}
    ]},
    {<<"vi">>, [
        {language, <<"vi">>},
        {region, <<"VN">>},
        {name, <<"Tiếng Việt"/utf8>>},
        {name_en, <<"Vietnamese"/utf8>>}
    ]},
    {<<"zh">>, [
        {type, <<"macro_language">>},
        {language, <<"zh">>},
        {script, <<"Hans">>},
        {name, <<"中文"/utf8>>},
        {name_en, <<"Chinese (Simplified)"/utf8>>},
        {sublanguages, [
            {<<"zh-hans">>, [
                {language, <<"zh">>},
                {script, <<"Hans">>},
                {name, <<"简体中文"/utf8>>},
                {name_en, <<"Chinese (Simplified)"/utf8>>}
            ]},
            {<<"zh-hans-cn">>, [
                {language, <<"zh-hans">>},
                {region, <<"CN">>},
                {script, <<"Hans">>},
                {name, <<"中国大陆简体脚本"/utf8>>},
                {name_en, <<"Chinese - Mainland (Simplified)"/utf8>>}
            ]},
            {<<"zh-hans-sg">>, [
                {language, <<"zh-hans">>},
                {region, <<"SG">>},
                {script, <<"Hans">>},
                {name, <<"新加坡中国简体脚本"/utf8>>},
                {name_en, <<"Chinese - Singapore (Simplified)"/utf8>>}
            ]}
        ]}
    ]},
    {<<"zh-hant">>, [
        {language, <<"zh-hant">>},
        {script, <<"Hant">>},
        {name, <<"中國傳統的腳本"/utf8>>},
        {name_en, <<"Chinese (Traditional)"/utf8>>},
        {sublanguages, [
            {<<"zh-hant-hk">>, [
                {language, <<"zh-hant">>},
                {region, <<"HK">>},
                {script, <<"Hant">>},
                {name, <<"香港中國傳統腳本"/utf8>>},
                {name_en, <<"Chinese - Hong Kong (Traditional)"/utf8>>}
            ]},
            {<<"zh-hant-mo">>, [
                {language, <<"zh-hant">>},
                {region, <<"MO">>},
                {script, <<"Hant">>},
                {name, <<"澳門中國人在傳統的腳本"/utf8>>},
                {name_en, <<"Chinese - Macau (Traditional)"/utf8>>}
            ]},
            {<<"zh-hant-tw">>, [
                {language, <<"zh-hant">>},
                {region, <<"TW">>},
                {script, <<"Hant">>},
                {name, <<"台灣中國傳統腳本"/utf8>>},
                {name_en, <<"Chinese - Taiwan (Traditional)"/utf8>>}
            ]}
        ]}
    ]}
].

%% Other, less used languages:

% gv: Manx
% ha: Hausa
% ho: Hiri Motu
% hy: Armenian
% hz: Herero
% ik: Inupiak
% io: Ido
% iu: Inuktitut
% ki: Kikuyu
% kj: Kuanyama
% kk: Kazakh
% kl: Kalaallisut Greenlandic
% km: Khmer Cambodian
% kn: Kannada
% ks: Kashmiri
% ku: Kurdish
% kv: Komi
% kw: Cornish
% ky: Kirghiz
% lb: Letzeburgesch
% ln: Lingala
% lo: Lao Laotian
% mh: Marshall
% mi: Maori
% ml: Malayalam
% mo: Moldavian
% mr: Marathi
% ms: Malay
% my: Burmese
% na: Nauru
% nb: Norwegian Bokmål
% nd: Ndebele, North
% ne: Nepali
% ng: Ndonga
% nr: Ndebele, South
% nv: Navajo
% ny: Chichewa Nyanja
% oc: Occitan Provençal
% om: (Afan) Oromo
% or: Oriya
% os: Ossetian Ossetic
% pi: Pali
% qu: Quechua
% rm: Rhaeto-Romance
% rn: Rundi Kirundi
% rw: Kinyarwanda
% sa: Sanskrit
% sc: Sardinian
% sd: Sindhi
% se: Northern Sami
% sg: Sango Sangro
% si: Sinhalese
% sm: Samoan
% sn: Shona
% so: Somali
% ss: Swati Siswati
% st: Sesotho Sotho, Southern
% su: Sundanese
% sw: Swahili
% ta: Tamil
% te: Telugu
% tg: Tajik
% ti: Tigrinya
% tk: Turkmen
% tl: Tagalog
% tn: Tswana Setswana
% to: Tonga
% ts: Tsonga
% tt: Tatar
% tw: Twi
% ty: Tahitian
% ug: Uighur
% ur: Urdu
% uz: Uzbek
% vo: Volapuk
% wa: Walloon
% wo: Wolof
% xh: Xhosa
% yo: Yoruba
% za: Zhuang
% zu: Zulu

