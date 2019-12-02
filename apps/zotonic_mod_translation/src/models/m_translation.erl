%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013 Marc Worrell
%%
%% @doc Model for access to language lists etc

%% Copyright 2013 Marc Worrell
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
-author("Marc Worrell <marc@worrell.nl").

-behaviour(zotonic_model).

%% interface functions
-export([
    m_get/3,

    language_list_configured/1,
    language_list_enabled/1,
    main_languages/0,
    all_languages/0
]).

-include_lib("zotonic_core/include/zotonic.hrl").


%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ rewrite_url | Rest ], _Msg, Context) ->
    {ok, {m_config:get_boolean(mod_translation, rewrite_url, true, Context), Rest}};
m_get([ force_default | Rest ], _Msg, Context) ->
    {ok, {m_config:get_boolean(mod_translation, rewrite_url, false, Context), Rest}};
m_get([ language | Rest ], _Msg, Context) ->
    {ok, {z_context:language(Context), Rest}};
m_get([ language_list_configured | Rest ], _Msg, Context) ->
    {ok, {language_list_configured(Context), Rest}};
m_get([ language_list_enabled | Rest ], _Msg, Context) ->
    {ok, {language_list_enabled(Context), Rest}};
m_get([ default_language | Rest ], _Msg, Context) ->
    {ok, {default_language(Context), Rest}};
m_get([ main_languages | Rest ], _Msg, _Context) ->
    {ok, {main_languages(), Rest}};
m_get([ all_languages | Rest ], _Msg, _Context) ->
    {ok, {all_languages(), Rest}};
m_get([ enabled_language_codes | Rest ], _Msg, Context) ->
    {ok, {z_language:enabled_language_codes(Context), Rest}};
m_get([ language_list | Rest ], _Msg, Context) ->
    {ok, {z_language:language_list(Context), Rest}};
m_get([ language_stemmer | Rest ], _Msg, Context) ->
    Stemmer = case m_config:get_value(i18n, language_stemmer, Context) of
        undefined -> default_language(Context);
        <<>> -> default_language(Context);
        St -> St
    end,
    {ok, {Stemmer, Rest}};
m_get(Vs, _Msg, _Context) ->
    lager:info("Unknown ~p lookup: ~p", [?MODULE, Vs]),
    {error, unknown_path}.

default_language(Context) ->
    z_language:default_language(Context).

language_list_configured(Context) ->
    sort_codes(mod_translation:language_config(Context)).

language_list_enabled(Context) ->
	sort_codes(mod_translation:enabled_languages(Context)).

main_languages() ->
    sort(z_language:main_languages()).

all_languages() ->
    sort(z_language:all_languages()).

sort_codes(Codes) when is_list(Codes) ->
    List = lists:map(
        fun(Code) ->
            {Code, z_translation:properties(Code)}
        end,
        Codes),
    sort(List).

sort(Map) when is_map(Map) ->
    List = maps:fold(
        fun
            (K, V, Acc) when is_atom(K) ->
                [ {K, V} | Acc ];
            (_, _, Acc) ->
                Acc
        end,
        [],
        Map),
    sort(List);
sort(List) ->
    lists:sort(fun sortfun/2, List).

sortfun({_, As}, {_, Bs}) ->
    maps:get(sort_key, As) =< maps:get(sort_key, Bs).

