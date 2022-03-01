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
    initialize_config/1,
    available_translations/1,
    default_language/1,
    enabled_languages/1,
    editable_languages/1,
    acceptable_languages/1,
    language_config/1,
    set_language_config/2,
    is_valid/1,
    to_language_atom/1,
    fallback_language/1,
    fallback_language/2,
    english_name/1,
    local_name/1,
    is_rtl/1,
    properties/1,
    all_languages/0,
    main_languages/0,
    language_list/1,
    is_language_enabled/2,
    is_language_editable/2,
    enabled_language_codes/1,
    editable_language_codes/1
]).

-include("zotonic.hrl").

-define(DEFAULT_LANGUAGE, en).

-type language_code() :: atom().
-type language() :: language_code() | binary() | string().

-export_type([ language/0, language_code/0 ]).



%% @doc Initialize the i18n language configurations
-spec initialize_config( z:context() ) -> ok.
initialize_config(Context) ->
    case z_db:has_connection(Context) of
        true ->
            % Set default language
            case m_config:get_value(i18n, language, Context) of
                undefined -> m_config:set_value(i18n, language, default_language(Context), Context);
                _ -> ok
            end,
            % Set list of enabled languages
            case m_config:get(i18n, languages, Context) of
                undefined ->
                    init_config_languages(Context);
                _Existing ->
                    ok
            end;
        false ->
            ok
    end.

init_config_languages(Context) ->
    case m_config:get(i18n, language_list, Context) of
        undefined ->
            m_config:set_prop(i18n, languages, list, default_languages(Context), Context);
        I18NLanguageList ->
            maybe_update_config_list(I18NLanguageList, Context),
            ok
    end.

%% @doc Return the list of available languages, enable the default language.
default_languages(Context) ->
    Codes = available_translations(Context),
    List = [ {C, editable} || C <- Codes ],
    EnabledLang = case m_config:get_value(i18n, language, Context) of
        undefined -> en;
        <<>> -> en;
        Code -> z_convert:to_atom(Code)
    end,
    List1 = proplists:delete(EnabledLang, List),
    lists:sort( [ {EnabledLang, true} | List1 ]).


%% @doc Fetch the available translations by checking for all .po files in zotonic_core
-spec available_translations(z:context()) -> list( atom() ).
available_translations(Context) ->
    ModPoFiles = z_module_indexer:translations(Context),
    PoFiles = proplists:get_value(zotonic_core, ModPoFiles, []),
    lists:filter(
        fun z_language:is_valid/1, lists:usort([ C || {C, _File} <- PoFiles ])
    ).

% Fetch the list of acceptable languages and their fallback languages.
% Store this in the depcache (and memo) for quick(er) lookups.
-spec acceptable_languages( z:context() ) -> list( {binary(), [ binary() ]} ).
acceptable_languages(Context) ->
    z_depcache:memo(
        fun() ->
            Enabled = enabled_languages(Context),
            lists:foldl(
                fun(Code, Acc) ->
                    LangProps = z_language:properties(Code),
                    Lang = atom_to_binary(Code, utf8),
                    Fs = z_language:fallback_language(Code),
                    FsAsBin = [ z_convert:to_binary(F) || F <- Fs ],
                    Acc1 = [ {Lang, FsAsBin} | Acc ],
                    lists:foldl(
                        fun(Alias, AliasAcc) ->
                            AliasBin = z_convert:to_binary(Alias),
                            [ {AliasBin, FsAsBin} | AliasAcc ]
                        end,
                        Acc1,
                        maps:get(alias, LangProps, []))
                end,
                [],
                Enabled)
        end,
        acceptable_languages,
        3600,
        [config],
        Context).

%% @doc Get the list of configured languages that are enabled.
-spec enabled_languages(z:context()) -> list( atom() ).
enabled_languages(Context) ->
    case z_memo:get('z_language$enabled_languages') of
        V when is_list(V) ->
            V;
        _ ->
            ConfigLanguages = lists:filtermap(
                fun
                    ({Code, true}) -> {true, Code};
                    ({_, _}) -> false
                end,
                language_config(Context)),
            z_memo:set('z_language$enabled_languages', ConfigLanguages)
    end.

%% @doc Get the list of configured languages that are editable.
-spec editable_languages(z:context()) -> list( atom() ).
editable_languages(Context) ->
    case z_memo:get('z_language$editable_languages') of
        V when is_list(V) ->
            V;
        _ ->
            ConfigLanguages = lists:filtermap(
                fun
                    ({Code, true}) -> {true, Code};
                    ({Code, editable}) -> {true, Code};
                    ({_, _}) -> false
                end,
                language_config(Context)),
            z_memo:set('z_language$editable_languages', ConfigLanguages)
    end.

%% @doc Get the list of configured languages.
-spec language_config(z:context()) -> list( {atom(), boolean()} ).
language_config(Context) ->
    case m_config:get(i18n, languages, Context) of
        undefined -> [];
        LanguageConfig -> proplists:get_value(list, LanguageConfig, [])
    end.

%% @doc Save a new language config list
-spec set_language_config( list(), z:context() ) -> ok.
set_language_config(NewConfig, Context) ->
    case language_config(Context) of
        NewConfig -> ok;
        _ ->
            SortedConfig = lists:sort(NewConfig),
            m_config:set_prop(i18n, languages, list, SortedConfig, Context)
    end,
    z_memo:delete('z_language$enabled_languages'),
    z_memo:delete('z_language$editable_languages'),
    ok.

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

%% @doc Translate a language-code to an atom; only return known codes.
%% Also map aliased language codes to their preferred format. Eg. 'zh-tw' to 'zh-hant'
-spec to_language_atom( language() ) -> {ok, language()} | {error, not_a_language}.
to_language_atom(Code) when is_binary(Code); is_atom(Code) ->
    z_language_data:to_language_atom(Code);
to_language_atom(Code) ->
    to_language_atom(z_convert:to_binary(Code)).


%% @doc Return the list of fallback languages (atoms) for the language.
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

%% @doc Returns the local language name.
-spec local_name( language() ) -> binary() | undefined.
local_name(Code) ->
    get_property(Code, name).

%% @doc Check if the given language is a rtl language.
-spec is_rtl( language() ) -> boolean().
is_rtl(Code) ->
    get_property(Code, direction) =:= <<"RTL">>.


%% @doc Check if a language code is allowed to be used as an user
%%      selectable language for the interface.
is_language_enabled(Code, Context) when is_atom(Code) ->
    lists:member(Code, enabled_language_codes(Context));
is_language_enabled(Code, Context) ->
    case to_language_atom(Code) of
        {ok, Lang} ->
            lists:member(Lang, enabled_language_codes(Context));
        {error, not_a_language} ->
            false
    end.

%% @doc Check if a language code is allowed to be edited.
%%      This is a superset of the enabled languages.
-spec is_language_editable( language(), z:context() ) -> boolean().
is_language_editable(Code, Context) when is_atom(Code) ->
    lists:member(Code, editable_language_codes(Context));
is_language_editable(Code, Context) ->
    case to_language_atom(Code) of
        {ok, Lang} ->
            lists:member(Lang, editable_language_codes(Context));
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

%% @doc Return list of languges enabled in the user interface.
-spec enabled_language_codes(z:context()) -> list( language_code() ).
enabled_language_codes(Context) ->
    case m_config:get(i18n, languages, Context) of
        undefined ->
            [ default_language(Context) ];
        Cfg when is_list(Cfg) ->
            lists:filtermap(
                fun
                    ({Code, true}) -> {true, Code};
                    (_) -> false
                end,
                proplists:get_value(list, Cfg, []))
    end.

%% @doc Return list of languages enabled in the user interface.
-spec editable_language_codes(z:context()) -> list( language_code() ).
editable_language_codes(Context) ->
    case m_config:get(i18n, languages, Context) of
        undefined ->
            [ default_language(Context) ];
        Cfg when is_list(Cfg) ->
            lists:filtermap(
                fun
                    ({Code, true}) -> {true, Code};
                    ({Code, editable}) -> {true, Code};
                    (_) -> false
                end,
                proplists:get_value(list, Cfg, []))
    end.

%% @private
%% Gets a property from an item retrieved from *all* languages.
-spec get_property( language(), Key:: atom() ) -> binary() | undefined.
get_property(Code, Key) ->
    Map = maps:get(Code, z_language_data:languages_map_flat(), #{}),
    maps:get(Key, Map, undefined).



%% @doc Convert the 0.x config i18n.language_list to the 1.x i18n.languages
-spec maybe_update_config_list(I18NLanguageList::list(), Context::z:context()) -> ok.
maybe_update_config_list(I18NLanguageList, Context) ->
    case proplists:get_value(list, I18NLanguageList) of
        undefined ->
            m_config:delete(i18n, language_list, Context),
            ok;
        List when is_list(List) ->
            ?LOG_INFO("mod_translation: Converting 'i18n.language_list.list' config list from 0.x to 1.0."),
            NewList = lists:foldr(
                fun
                    ({Code, ItemProps}, Acc) when is_list(ItemProps), is_atom(Code) ->
                        case z_language:is_valid(Code) of
                            true ->
                                IsEnabled = z_convert:to_bool( proplists:get_value(is_enabled, ItemProps, false) ),
                                IsEditable = z_convert:to_bool( proplists:get_value(is_editable, ItemProps, false) ),
                                case {IsEnabled, IsEditable} of
                                    {true, _} -> [ {Code, true} | Acc ];
                                    {_, true} -> [ {Code, edit} | Acc ];
                                    {_, _} -> [ {Code, false} | Acc ]
                                end;
                            false ->
                                ?LOG_WARNING("mod_translation: conversion error, language ~p does not exist in z_language, skipping.", [Code]),
                                Acc
                        end;
                    (Unknown, Acc) ->
                        ?LOG_WARNING("mod_translation: conversion error, contains unknown record: ", [Unknown]),
                        Acc
                end,
                [],
                List),
            m_config:set_prop(i18n, languages, list, NewList, Context),
            m_config:delete(i18n, language_list, Context);
        _ ->
            ?LOG_WARNING("mod_translation: conversion error, 'i18n.language_list.list' is not a list. Resetting languages."),
            m_config:delete(i18n, language_list, Context)
    end.

