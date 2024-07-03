%% @doc Language code handling functions.
%%
%% Mandatory background read on language tags: [1].
%%
%% Some quotes from [1]:
%%
%%      The golden rule when creating language tags is to keep the tag as short as
%%      possible. Avoid region, script or other subtags except where they add useful
%%      distinguishing information. For instance, use 'ja' for Japanese and not
%%      'ja-JP', unless there is a particular reason that you need to say that this is
%%      Japanese as spoken in Japan, rather than elsewhere.
%%
%%      The entries in the registry follow certain conventions with regard to upper
%%      and lower letter-casing. For example, language tags are lower case, alphabetic
%%      region subtags are upper case, and script tags begin with an initial capital.
%%      This is only a convention!
%%
%% Note that we use lower case subtags in subtag identifiers and URLs.
%%
%%  Language identifiers can have the following forms:
%%  - language;
%%  - language-extlang;
%%  - language-region;
%%  - language-script;
%%  It is discouraged to use language-script-region, but it is possible if
%%  required.
%%  For a list of language, region and script codes, see [2].
%%  [1] http://www.w3.org/International/articles/language-tags/
%%  [2] http://www.iana.org/assignments/language-subtag-registry/language-subtag-registry
%% @end

%% Copyright 2016-2023 Arthur Clemens
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

-module(z_language).

-export([
    initialize_config/1,
    available_translations/1,
    default_language/1,
    enabled_languages/1,
    editable_languages/1,
    acceptable_languages_map/1,
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
-type language_status() :: true | editable | false.

-export_type([ language/0, language_code/0, language_status/0 ]).



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
            case m_config:get_prop(i18n, languages, list, Context) of
                undefined ->
                    init_config_languages(Context);
                [] ->
                    Default = [ {default_language(Context), true} ],
                    m_config:set_prop(i18n, languages, list, Default, Context);
                [{FirstCode, _} | _ ] = Config ->
                    % Ensure that the default language is the first enabled language
                    Default = default_language(Context),
                    if
                        FirstCode =:= Default ->
                            ok;
                        true ->
                            {Enabled, Disabled} = lists:partition(
                                fun({_,Status}) -> Status =:= true end,
                                Config),
                            {Editable, Off} = lists:partition(
                                fun({_,Status}) -> Status =:= editable end,
                                Disabled),
                            Config1 = Enabled ++ Editable ++ Off,
                            Default1 = [ {Default, true} | proplists:delete(Default, Config1) ],
                            m_config:set_prop(i18n, languages, list, Default1, Context)
                    end,
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
    DefaultLang = default_language(Context),
    [ {DefaultLang, true} | proplists:delete(DefaultLang, List) ].


%% @doc Fetch the available translations by checking for all .po files in zotonic_core
-spec available_translations(z:context()) -> list( atom() ).
available_translations(Context) ->
    ModPoFiles = z_module_indexer:translations(Context),
    PoFiles = proplists:get_value(zotonic_core, ModPoFiles, []),
    lists:filter(
        fun z_language:is_valid/1, lists:usort([ C || {C, _File} <- PoFiles ])
    ).


% Fetch the map of acceptable languages and their fallback languages.
% Store this in the depcache for quick(er) lookups.
-spec acceptable_languages_map( z:context() ) -> #{ binary() => binary() }.
acceptable_languages_map(Context) ->
    z_depcache:memo(
        fun() -> acceptable_languages_map_1(Context) end,
        acceptable_languages_map,
        3600,
        [config],
        Context).

acceptable_languages_map_1(Context) ->
    lists:foldl(
        fun(Code, Acc) ->
            Props = #{
                code_bin := CodeBin,
                fallback := FallbackList
            } = z_language:properties(Code),
            AliasList = maps:get(alias, Props, []),
            Acc1 = Acc#{ CodeBin => CodeBin },
            Acc2 = lists:foldl(
                fun(Alias, FAcc) ->
                    maybe_set(Alias, CodeBin, FAcc)
                end,
                Acc1,
                AliasList),
            lists:foldl(
                fun(Fallback, FAcc) ->
                    maybe_set(atom_to_binary(Fallback), CodeBin, FAcc)
                end,
                Acc2,
                FallbackList)
        end,
        #{},
        enabled_languages(Context)).

maybe_set(K, V, Map) ->
    case maps:find(K, Map) of
        error -> Map#{ K => V };
        _ -> Map
    end.


%% @doc Get the list of configured languages that are enabled. The list is
%% in the order of configured priority.
-spec enabled_languages(Context) -> LanguageCodes when
    Context :: z:context(),
    LanguageCodes :: [ language_code() ].
enabled_languages(Context) ->
    MemoKey = {'z_language$enabled_languages', z_context:site(Context)},
    case z_memo:get(MemoKey) of
        V when is_list(V) ->
            V;
        _ ->
            ConfigLanguages = lists:filtermap(
                fun
                    ({Code, true}) -> {true, Code};
                    ({_, _}) -> false
                end,
                language_config(Context)),
            z_memo:set(MemoKey, ConfigLanguages)
    end.

%% @doc Get the list of configured languages that are editable. The list is
%% in the order of configured priority.
-spec editable_languages(Context) -> LanguageCodes when
    Context :: z:context(),
    LanguageCodes :: [ language_code() ].
editable_languages(Context) ->
    MemoKey = {'z_language$editable_languages', z_context:site(Context)},
    case z_memo:get(MemoKey) of
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
            z_memo:set(MemoKey, ConfigLanguages)
    end.

%% @doc Get the list of configured languages.
-spec language_config(Context) -> LanguageConfigList when
    Context :: z:context(),
    LanguageConfigList :: [ {language_code(), language_status()} ].
language_config(Context) ->
    case m_config:get(i18n, languages, Context) of
        undefined -> [];
        LanguageConfig -> proplists:get_value(list, LanguageConfig, [])
    end.

%% @doc Save a new language config list. If the empty list is saved then the
%% default language (en) is enabled. There must be a language enabled. The list
%% is in the order of preference. The default system language is the first
%% enabled (true) language in the list.
-spec set_language_config( LanguageStatusList, Context ) -> ok when
    Context :: z:context(),
    LanguageStatusList :: [ {language_code(), language_status()} ].
set_language_config([], Context) ->
    set_language_config([ {?DEFAULT_LANGUAGE, true} ], Context);
set_language_config(LanguageStatusList, Context) ->
    case language_config(Context) of
        LanguageStatusList -> ok;
        _ -> m_config:set_prop(i18n, languages, list, LanguageStatusList, Context)
    end,
    % Store the first enabled language as the default language
    Default = first_enabled(LanguageStatusList),
    DefaultB = z_convert:to_binary(Default),
    case m_config:get_value(i18n, language, Context) of
        DefaultB -> ok;
        _ -> m_config:set_value(i18n, language, Default, Context)
    end,
    z_memo:delete({'z_language$enabled_languages', z_context:site(Context)}),
    z_memo:delete({'z_language$editable_languages', z_context:site(Context)}),
    ok.

first_enabled([]) ->
    ?DEFAULT_LANGUAGE;
first_enabled([{Code, true}|_]) ->
    Code;
first_enabled([_|Cs]) ->
    first_enabled(Cs).


%% @doc Returns the configured default language for this server; if not set, 'en'
%%      (English).
-spec default_language(OptContext) -> language_code() when
    OptContext :: z:context() | undefined.
default_language(undefined) ->
    ?DEFAULT_LANGUAGE;
default_language(Context) ->
    case m_config:get_value(i18n, language, Context) of
        undefined -> ?DEFAULT_LANGUAGE;
        <<>> -> ?DEFAULT_LANGUAGE;
        "" -> ?DEFAULT_LANGUAGE;
        Lang -> z_convert:to_atom(Lang)
    end.

%% @doc Check if the language code code is a valid language.
-spec is_valid( language() ) -> boolean().
is_valid(Code) ->
    z_language_data:is_language(Code).

%% @doc Translate a language-code to an atom; only return known codes.
%% Also map aliased language codes to their preferred format. Eg. 'zh-tw' to 'zh-hant'
-spec to_language_atom( language() ) -> {ok, language_code()} | {error, not_a_language}.
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


%% @doc Check if a language code is allowed to be used as a user
%% selectable language for the interface. Returns false for
%% unknown languages.
-spec is_language_enabled(Language, Context) -> boolean() when
    Language :: language(),
    Context :: z:context().
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
%% This is a superset of the enabled languages. Returns false for
%% unknown languages.
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
%% Returns a maps of language maps; sub-languages are added to the map of main languages.
%% For each language a map with properties is returned - see properties/1.
%% Each language is present with its iso code as an atom and binary key. This for
%% easier lookups.
-spec all_languages() -> map().
all_languages() ->
    z_language_data:languages_map_flat().

%% @doc Map of language data of main languages.
-spec main_languages() -> map().
main_languages() ->
    z_language_data:languages_map_main().

%% @doc Return the currently configured list of languages
-spec language_list(z:context()) -> list( {language_code(), language_status()} ).
language_list(Context) ->
    case m_config:get(i18n, languages, Context) of
        undefined ->
            [ {default_language(Context), true} ];
        Cfg ->
            case proplists:get_value(list, Cfg, []) of
                [] -> [ {default_language(Context), true} ];
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
            ?LOG_INFO(#{
                text => <<"mod_translation: Converting 'i18n.language_list.list' config list from 0.x to 1.0.">>,
                in => zotonic_core
            }),
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
                                ?LOG_WARNING(#{
                                    text => <<"Conversion error, language does not exist in z_language, skipping.">>,
                                    in => zotonic_core,
                                    language => Code
                                }),
                                Acc
                        end;
                    (Unknown, Acc) ->
                        ?LOG_WARNING(#{
                            text => <<"Conversion error, contains unknown record.">>,
                            in => zotonic_core,
                            record => Unknown
                        }),
                        Acc
                end,
                [],
                List),
            % Ensure the default language is the first enabled language
            Default = default_language(Context),
            NewList1 = [ {Default, true} | proplists:delete(Default, NewList) ],
            m_config:set_prop(i18n, languages, list, NewList1, Context),
            m_config:delete(i18n, language_list, Context);
        _ ->
            ?LOG_WARNING(#{
                text => <<"Conversion error, 'i18n.language_list.list' is not a list. Resetting languages.">>,
                in => zotonic_core
            }),
            m_config:delete(i18n, language_list, Context)
    end.

