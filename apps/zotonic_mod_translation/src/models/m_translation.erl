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
    m_get/2,

    language_list_enabled/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").


%% @doc Fetch the value for the key from a model source
-spec m_get( list(), z:context() ) -> {term(), list()}.
m_get([ rewrite_url | Rest ], Context) ->
    {m_config:get_boolean(mod_translation, rewrite_url, true, Context), Rest};
m_get([ force_default | Rest ], Context) ->
    {m_config:get_boolean(mod_translation, rewrite_url, false, Context), Rest};
m_get([ language | Rest ], Context) ->
    {z_context:language(Context), Rest};
m_get([ language_list_configured | Rest ], Context) ->
    {language_list_configured(Context), Rest};
m_get([ language_list_enabled | Rest ], Context) ->
    {language_list_enabled(Context), Rest};
m_get([ default_language | Rest ], Context) ->
    {default_language(Context), Rest};
m_get([ main_languages | Rest ], _Context) ->
    {main_languages(), Rest};
m_get([ all_languages | Rest ], _Context) ->
    {all_languages(), Rest};
m_get([ enabled_language_codes | Rest ], Context) ->
    {z_language:enabled_language_codes(Context), Rest};
m_get([ language_list | Rest ], Context) ->
    {z_language:language_list(Context), Rest};
m_get([ language_stemmer | Rest ], Context) ->
    Stemmer = case m_config:get_value(i18n, language_stemmer, Context) of
        undefined -> default_language(Context);
        <<>> -> default_language(Context);
        St -> St
    end,
    {Stemmer, Rest};
m_get(Vs, _Context) ->
    lager:error("Unknown ~p lookup: ~p", [?MODULE, Vs]),
    {undefined, []}.

default_language(Context) ->
    z_language:default_language(Context).

language_list_configured(Context) ->
    z_language:sort_properties(mod_translation:language_config(Context), name_en).

language_list_enabled(Context) ->
	z_language:sort_properties(mod_translation:enabled_languages(Context), name).

main_languages() ->
    z_language:sort_properties(z_language:main_languages(), name_en).

all_languages() ->
    z_language:sort_properties(z_language:all_languages(), name_en).

