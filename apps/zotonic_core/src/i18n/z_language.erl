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
    fallback_language/1,
    fallback_language/2,
    english_name/1,
    is_rtl/1,
    properties/1,
    all_languages/0,
    main_languages/0,
    language_list/1,
    is_language_enabled/2,
    enabled_language_codes/1
]).

-include("zotonic.hrl").

-define(DEFAULT_LANGUAGE, en).

-type language_code() :: atom().
-type language() :: language_code() | binary() | string().

-export_type([ language/0, language_code/0 ]).

%% @doc Returns the configured default language for this server; if not set, 'en'
%%      (English).
-spec default_language( z:context() | undefined ) -> language_code().
default_language(undefined) ->
    ?DEFAULT_LANGUAGE;
default_language(Context) ->
    z_convert:to_atom(m_config:get_value(i18n, language, ?DEFAULT_LANGUAGE, Context)).

%% @doc Check if the language code code is a valid language.
-spec is_valid( language() ) -> boolean().
is_valid(Code) ->
    z_language_data:is_language(Code).

%% @doc Translate a language-code to an atom.
-spec to_language_atom( language() ) -> {ok, language()} | {error, not_a_language}.
to_language_atom(Code) when is_binary(Code) ->
    case is_valid(Code) of
        false -> {error, not_a_language};
        true -> {ok, z_convert:to_atom(Code)}
    end;
to_language_atom(Code) ->
    to_language_atom(z_convert:to_binary(Code)).


%% @doc Return the list of fallback languages (atoms) for the lanaguage.
-spec fallback_language( language() ) -> [ language_code() ].
fallback_language(Code) ->
    z_language_data:fallback(Code).

%% @doc Return the fallback language (the base language);  if no fallback language is
%%      found, returns the default language.
-spec fallback_language( language() | undefined, z:context() ) -> language_code().
fallback_language(undefined, Context) ->
    default_language(Context);
fallback_language(Code, Context) when is_binary(Code); is_atom(Code) ->
    case is_valid(Code) of
        false ->
            default_language(Context);
        true ->
            case z_language_data:fallback(Code) of
                [ Fallback | _ ] -> Fallback;
                [] -> default_language(Context)
            end
    end;
fallback_language(Code, Context) ->
    fallback_language(z_convert:to_binary(Code), Context).


%% @doc Returns the English language name.
-spec english_name( language() ) -> binary() | undefined.
english_name(Code) ->
    get_property(Code, name_en).


%% @doc Check if the given language is a rtl language.
-spec is_rtl( language() ) -> boolean().
is_rtl(Code) ->
    get_property(Code, direction) =:= <<"RTL">>.

-spec is_language_enabled( language(), z:context() ) -> boolean().
is_language_enabled(Code, Context) when is_atom(Code) ->
    lists:member(Code, enabled_language_codes(Context));
is_language_enabled(Code, Context) ->
    case to_language_atom(Code) of
        {ok, Lang} ->
            lists:member(Lang, enabled_language_codes(Context));
        {error, not_a_language} ->
            false
    end.

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
-spec properties( language() ) -> map() | undefined.
properties(Code) when is_binary(Code); is_atom(Code) ->
    maps:get(Code, z_language_data:languages_map_flat(), undefined);
properties(Code) when is_list(Code) ->
    properties(z_convert:to_binary(Code)).

%% @doc List of language data.
%%      Returns a flattened list of property lists; sub-languages are added to the list of
%%      main languages.
%%      For each language a property list is returned - see properties/1.
-spec all_languages() -> map().
all_languages() ->
    z_language_data:languages_map_flat().

%% @doc Map of language data of main languages.
-spec main_languages() -> map().
main_languages() ->
    z_language_data:languages_map_main().

%% @doc Return the currently configured list of languages
-spec language_list(z:context()) -> list( {language_code(), list()} ).
language_list(Context) ->
    case m_config:get(i18n, languages, Context) of
        undefined ->
            [ {default_language(Context), []} ];
        Cfg ->
            case proplists:get_value(list, Cfg, []) of
                [] -> [ {default_language(Context), []} ];
                L when is_list(L) -> L
            end
    end.

-spec enabled_language_codes(z:context()) -> list( language_code() ).
enabled_language_codes(Context) ->
    case m_config:get(i18n, languages, Context) of
        undefined ->
            [ default_language(Context) ];
        Cfg when is_list(Cfg) ->
            [ Code || {Code, true} <- proplists:get_value(list, Cfg, []) ]
    end.


%% @private
%% Gets a property from an item retrieved from *all* languages.
-spec get_property( language(), Key:: atom() ) -> binary() | undefined.
get_property(Code, Key) ->
    get_property_from_list(Code, Key, all_languages()).


%% @private
%% Gets a property from an item retrieved from specified list
-spec get_property_from_list( language(), Key:: atom(), List:: list() ) -> binary() | list() | undefined.
get_property_from_list(Code, Key, List) when is_binary(Code) ->
    case proplists:get_value(Code, List) of
        undefined -> undefined;
        Data -> proplists:get_value(Key, Data)
    end;
get_property_from_list(Code, Key, List) ->
    get_property_from_list(z_convert:to_binary(Code), Key, List).


