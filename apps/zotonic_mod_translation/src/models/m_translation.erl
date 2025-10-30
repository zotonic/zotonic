%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013-2025 Marc Worrell
%% @doc Model for access to request language, language lists and language configuration.
%% @end

%% Copyright 2013-2025 Marc Worrell
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

-module(m_translation).
-moduledoc("
The m\\_translation model gives easy access to language and translation related information.

The following `m.translation` model properties are available in templates:

| Property                  | Description                           |
| ------------------------- | ------------------------------------- |
| language                  | The current language.                 |
| language\\\\_list           | The list of all configured languages. |
| language\\\\_list\\\\_enabled | The list of all enabled languages.    |

This is an example of the languages returned by `m.translation.language_list`:


```erlang
[{en, [{is_enabled,true}, {language,<<\"English\">>}]},
 {fr, [{is_enabled,false}, {language,<<\"Français\">>}]},
 {nl, [{is_enabled,true}, {language,<<\"Nederlands\">>}]},
 {tr, [{is_enabled,true}, {language,<<\"Türkçe\">>}]}].
```

For example to list all enabled languages in a select box:


```django
<select>
{% for code,props in m.translation.language_list_enabled %}
  <option value=\"{{ code }}\" {% if m.translation.language == code %}selected{% endif %}>{{ props.language }}</option>
{% endfor %}
</select>
```
").
-author("Marc Worrell <marc@worrell.nl").

-behaviour(zotonic_model).

%% interface functions
-export([
    m_get/3,

    translate_to_lookup/4,
    translate/4,
    has_translation_service/1,

    has_language/3,

    add_translation/5,
    add_translation_map/5,

    remove_translation/3,
    remove_translation_map/2,

    keep_translation/3,
    keep_translation_map/2,

    language_list_configured/1,
    language_list_enabled/1,
    language_list_editable/1,
    main_languages/0,
    all_languages/0,

    query_language/1,

    add_properties/1,
    sort_codes/1,
    sort/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ <<"rewrite_url">> | Rest ], _Msg, Context) ->
    {ok, {m_config:get_boolean(mod_translation, rewrite_url, true, Context), Rest}};
m_get([ <<"force_default">> | Rest ], _Msg, Context) ->
    {ok, {m_config:get_boolean(mod_translation, force_default, false, Context), Rest}};
m_get([ <<"language">> | Rest ], _Msg, Context) ->
    {ok, {z_context:language(Context), Rest}};
m_get([ <<"language_list_configured">> | Rest ], _Msg, Context) ->
    {ok, {language_list_configured(Context), Rest}};
m_get([ <<"language_list_enabled">> | Rest ], _Msg, Context) ->
    {ok, {language_list_enabled(Context), Rest}};
m_get([ <<"language_list_editable">> | Rest ], _Msg, Context) ->
    {ok, {language_list_editable(Context), Rest}};
m_get([ <<"default_language">> | Rest ], _Msg, Context) ->
    {ok, {z_language:default_language(Context), Rest}};
m_get([ <<"query_language">> | Rest ], _Msg, Context) ->
    {ok, {query_language(Context), Rest}};
m_get([ <<"x_default_language">> | Rest ], _Msg, Context) ->
    Lang = case m_config:get_boolean(mod_translation, force_default, false, Context) of
        true ->
            z_language:default_language(Context);
        false ->
            'x-default'
    end,
    {ok, {Lang, Rest}};
m_get([ <<"main_languages">> | Rest ], _Msg, _Context) ->
    {ok, {main_languages(), Rest}};
m_get([ <<"all_languages">> | Rest ], _Msg, _Context) ->
    {ok, {all_languages(), Rest}};
m_get([ <<"enabled_language_codes">> | Rest ], _Msg, Context) ->
    {ok, {z_language:enabled_language_codes(Context), Rest}};
m_get([ <<"editable_language_codes">> | Rest ], _Msg, Context) ->
    {ok, {z_language:editable_language_codes(Context), Rest}};
m_get([ <<"language_list">> | Rest ], _Msg, Context) ->
    {ok, {z_language:language_list(Context), Rest}};
m_get([ <<"language_list_sorted">> | Rest ], _Msg, Context) ->
    {ok, {z_language:language_list_sorted(Context), Rest}};
m_get([ <<"language_stemmer">> | Rest ], _Msg, Context) ->
    Stemmer = case m_config:get_value(i18n, language_stemmer, Context) of
        undefined -> z_language:default_language(Context);
        <<>> -> z_language:default_language(Context);
        St -> St
    end,
    {ok, {Stemmer, Rest}};
m_get([ <<"name">>, Code | Rest ], _Msg, _Context) ->
    {ok, {z_language:local_name(Code), Rest}};
m_get([ <<"english_name">>, Code | Rest ], _Msg, _Context) ->
    {ok, {z_language:english_name(Code), Rest}};
m_get([ <<"localized_name">>, Code | Rest ], _Msg, Context) ->
    {ok, {z_language:localized_name(Code, Context), Rest}};
m_get([ <<"properties">>, Code | Rest ], _Msg, _Context) ->
    {ok, {z_language:properties(Code), Rest}};
m_get([ <<"translate">> | Rest ], #{ payload := Payload }, Context) ->
    case Payload of
        #{
            <<"from">> := FromCode,
            <<"texts">> := Texts
        } ->
            ToCode = maps:get(<<"to">>, Payload, z_context:language(Context)),
            case translate_to_lookup(FromCode, ToCode, Texts, Context) of
                {ok, Result} ->
                    {ok, {Result, Rest}};
                {error, _} = Error ->
                    Error
            end;
        _ ->
            {error, payload}
    end;
m_get([ <<"has_translation_service">> | Rest ], _Msg, Context) ->
    {ok, {has_translation_service(Context), Rest}};
m_get([ <<"detect">> | Rest ], Msg, Context) ->
    case translation_detect:detect(get_text_arg(Msg, Context)) of
        {ok, Iso} ->
            {ok, {Iso, Rest}};
        {error, _} = Error ->
            Error
    end;
m_get([ <<"detect_enabled">> | Rest ], Msg, Context) ->
    case translation_detect:detect(get_text_arg(Msg, Context), Context) of
        {ok, Iso} ->
            {ok, {Iso, Rest}};
        {error, _} = Error ->
            Error
    end;
m_get([ <<"has_language">>, RscId, Language | Rest ], _Msg, Context) ->
    case z_language:to_language_atom(Language) of
        {ok, Code} ->
            {ok, {has_language(RscId, Code, Context), Rest}};
        {error, _} = Error ->
            Error
    end;
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.


get_text_arg(#{ payload := #{ <<"text">> := Text } }, _Context) when is_binary(Text) ->
    Text;
get_text_arg(_Payload, Context) ->
    z_convert:to_binary(z_context:get_q(<<"text">>, Context)).

%% @doc Check if a resource has a language or one of the languages.
-spec has_language(Rsc, Language, Context) -> boolean() when
    Rsc :: m_rsc:resource() | map(),
    Language :: z_language:language_code() | [ z_language:language_code() ],
    Context :: z:context().
has_language(Rsc, Language, Context) ->
    translation_translate_rsc:has_language(Rsc, Language, Context).

%% @doc Add a translation to the resource. The source and destination language must be editable
%% languages for the site. If the source language is not in the resource then an error is returned.
-spec add_translation(Id, FromLanguage, ToLanguage, IsOverwrite, Context) -> ok | {error, Reason} when
    Id :: m_rsc:resource(),
    FromLanguage :: z_language:language_code(),
    ToLanguage :: z_language:language_code(),
    IsOverwrite :: boolean(),
    Context :: z:context(),
    Reason :: term().
add_translation(Id, FromLanguage, ToLanguage, IsOverwrite, Context) ->
    translation_translate_rsc:add_translation(Id, FromLanguage, ToLanguage, IsOverwrite, Context).

%% @doc Add a translation to a map. The source and destination language must be editable
%% languages for the site.
-spec add_translation_map(Map, FromLanguage, ToLanguage, IsOverwrite, Context) -> {ok, NewMap} | {error, Reason} when
    Map :: map(),
    NewMap :: map(),
    FromLanguage :: z_language:language_code(),
    ToLanguage :: z_language:language_code(),
    IsOverwrite :: boolean(),
    Context :: z:context(),
    Reason :: term().
add_translation_map(Map, FromLanguage, ToLanguage, IsOverwrite, Context) ->
    translation_translate_rsc:add_translation_map(Map, FromLanguage, ToLanguage, IsOverwrite, Context).

-spec remove_translation(Id, Language, Context) -> ok | {error, Reason} when
    Id :: m_rsc:resource(),
    Language :: z_language:language_code() | [ z_language:language_code() ],
    Context :: z:context(),
    Reason :: term().
remove_translation(Id, Language, Context) ->
    translation_translate_rsc:remove_translation(Id, Language, Context).

%% @doc Remove languages from a map.
-spec remove_translation_map(Map, Language) -> {ok, NewMap} when
    Map :: map(),
    NewMap :: map(),
    Language :: z_language:language_code() | [ z_language:language_code() ].
remove_translation_map(Map, Language) ->
    translation_translate_rsc:remove_translation_map(Map, Language).

%% @doc Remove all translations except the given ones from a resource.
-spec keep_translation(Id, Language, Context) -> ok | {error, Reason} when
    Id :: m_rsc:resource(),
    Language :: z_language:language_code() | [ z_language:language_code() ],
    Context :: z:context(),
    Reason :: term().
keep_translation(Id, Language, Context) when is_atom(Language) ->
    translation_translate_rsc:keep_translation(Id, Language, Context).

%% @doc Remove all translations except the given ones from a map.
-spec keep_translation_map(Map, Language) -> {ok, NewMap} when
    Map :: map(),
    NewMap :: map(),
    Language :: z_language:language_code() | [ z_language:language_code() ].
keep_translation_map(Map, Language) ->
    translation_translate_rsc:keep_translation_map(Map, Language).

%% @doc Check if there are modules that offer the translation service.
-spec has_translation_service(Context) -> boolean() when
    Context :: z:context().
has_translation_service(Context) ->
    case z_notifier:get_observers(#translate{}, Context) of
        [] -> false;
        [_|_] -> true
    end.

%% @doc Translate one or more strings from one language to another. The strings
%% can be trans records, a single string or a list of strings. If the source language
%% is 'en' then the .po files are consulted for a preferred translation.
%% Both the from and to language must be configured as editable languages.
%% The result is a list of maps with keys 'text' and 'translation'. The translation can be
%% 'undefined' if no translation could be provided.
-spec translate_to_lookup(FromLanguage, ToLanguage, Texts, Context) -> {ok, Translations} | {error, Reason} when
    FromLanguage :: z_language:language(),
    ToLanguage :: z_language:language(),
    Texts :: [ Text ] | Text,
    Text :: binary() | #trans{},
    Context :: z:context(),
    Translations :: list( map() ),
    Reason :: term().
translate_to_lookup(FromLanguage, ToLanguage, Texts, Context) ->
    TextList = to_list(Texts),
    case translate(FromLanguage, ToLanguage, Texts, Context) of
        {ok, Result} ->
            R = lists:map(
                fun({From, To}) ->
                    #{
                        <<"text">> => From,
                        <<"translation">> => To
                    }
                end,
                lists:zip(TextList, Result)),
            {ok, R};
        {error, _} = Error ->
            Error
    end.

%% @doc Translate one or more strings from one language to another. The strings
%% can be trans records, a single string or a list of strings. If the source language
%% is 'en' then the .po files are consulted for a preferred translation.
%% Both the from and to language must be configured as editable languages.
%% The result is a list of translations, in the same order as the input Texts. Translations
%% that could not be done result in 'undefined'.
-spec translate(FromLanguage, ToLanguage, Texts, Context) -> {ok, Translations} | {error, Reason} when
    FromLanguage :: z_language:language(),
    ToLanguage :: z_language:language(),
    Texts :: [ Text ] | Text,
    Text :: binary() | #trans{},
    Context :: z:context(),
    Translations :: [ binary() | undefined ],
    Reason :: term().
translate(FromLanguage, ToLanguage, Texts, Context) ->
    translation_translate:translate(FromLanguage, ToLanguage, Texts, Context).

to_list(#trans{} = Tr) ->
    [Tr];
to_list(Text) when is_binary(Text) ->
    [Text];
to_list(L) when is_list(L) ->
    L.

language_list_configured(Context) ->
    Default = z_language:default_language(Context),
    Config = z_language:language_config(Context),
    List = lists:map(
        fun
            ({Code, _}) when Code =:= Default ->
                Props = z_language:properties(Code),
                {Code, Props#{ is_enabled => true, is_editable => true, is_default => true } };
            ({Code, true}) ->
                Props = z_language:properties(Code),
                {Code, Props#{ is_enabled => true, is_editable => true, is_default => false } };
            ({Code, editable}) ->
                Props = z_language:properties(Code),
                {Code, Props#{ is_enabled => false, is_editable => true, is_default => false } };
            ({Code, false}) ->
                Props = z_language:properties(Code),
                {Code, Props#{ is_enabled => false, is_editable => false, is_default => false } }
        end,
        Config),
    % Ensure the default language is enabled
    List1 = case Default =:= undefined orelse proplists:is_defined(Default, Config) of
        true ->
            List;
        false ->
            Props = z_language:properties(Default),
            [ {Default, Props#{ is_enabled => true, is_editable => true, is_default => true } } | List ]
    end,
    List1.

-spec language_list_enabled(Context) -> Languages when
    Context :: z:context(),
    Languages :: [ {z_language:language_code(), map()} ].
language_list_enabled(Context) ->
	add_properties(z_language:enabled_languages(Context)).

-spec language_list_editable(Context) -> Languages when
    Context :: z:context(),
    Languages :: [ {z_language:language_code(), map()} ].
language_list_editable(Context) ->
    add_properties(z_language:editable_languages(Context)).

-spec main_languages() -> Languages when
    Languages :: [ {z_language:language_code(), map()} ].
main_languages() ->
    sort(z_language:main_languages()).

-spec all_languages() -> Languages when
    Languages :: [ {z_language:language_code(), map()} ].
all_languages() ->
    Langs = z_language:all_languages(),
    Langs1 = maps:filter( fun(K,_V) -> is_atom(K) end, Langs ),
    sort(Langs1).

%% @doc Return the specific language as requested in the current HTTP query (URL).
%% Return 'x-default' if there isn't a HTTP request or no language was
%% specified in the current request.
-spec query_language(Context) -> Language when
    Context :: z:context(),
    Language :: atom().
query_language(Context) ->
    case mod_translation:get_q_language(Context) of
        undefined ->
            'x-default';
        Language ->
            Language
    end.

add_properties(Codes) ->
    lists:map(
        fun(Code) ->
            {Code, z_language:properties(Code)}
        end,
        Codes).

sort_codes(Codes) when is_list(Codes) ->
    sort(add_properties(Codes)).

sort(Map) when is_map(Map) ->
    List = maps:fold(
        fun
            (Lang, V, Acc) ->
                case z_language:to_language_atom(Lang) of
                    {ok, Code} when is_atom(Code) ->
                        [ {Code, V} | Acc ];
                    {error, not_a_language} ->
                        %% Ignore unknown languages
                        Acc
                end
        end,
        [],
        Map),
    sort(List);
sort(List) ->
    lists:sort(fun sortfun/2, List).

sortfun({_, As}, {_, Bs}) ->
    maps:get(sort_key, As) =< maps:get(sort_key, Bs).

