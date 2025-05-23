%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017-2023 Marc Worrell
%% @doc Model for mod_seo configuration access and JSON-LD generation.
%% @end

%% Copyright 2017-2023 Marc Worrell
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

-module(m_seo).

-behaviour(zotonic_model).

-export([ m_get/3 ]).

-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ <<"noindex">> | Rest ], _Msg, Context) ->
    {ok, {m_config:get_boolean(seo, noindex, Context), Rest}};
m_get([ <<"keywords">> | Rest ], _Msg, Context) ->
    {ok, {m_config:get_value(seo, keywords, Context), Rest}};
m_get([ <<"description">> | Rest ], _Msg, Context) ->
    {ok, {m_config:get_value(seo, description, Context), Rest}};
m_get([ <<"bing">>, <<"webmaster_verify">> | Rest ], _Msg, Context) ->
    {ok, {m_config:get_value(seo_bing, webmaster_verify, Context), Rest}};
m_get([ <<"google">>, <<"webmaster_verify">> | Rest ], _Msg, Context) ->
    {ok, {m_config:get_value(seo_google, webmaster_verify, Context), Rest}};
m_get([ <<"google">>, <<"analytics">> | Rest ], _Msg, Context) ->
    {ok, {m_config:get_value(seo_google, analytics, Context), Rest}};
m_get([ <<"google">>, <<"gtm">> | Rest ], _Msg, Context) ->
    {ok, {m_config:get_value(seo_google, gtm, Context), Rest}};
m_get([ <<"plausible">>, <<"analytics">> | Rest ], _Msg, Context) ->
    {ok, {m_config:get_boolean(seo_plausible, analytics, Context), Rest}};
m_get([ <<"yandex">>, <<"webmaster_verify">> | Rest ], _Msg, Context) ->
    {ok, {m_config:get_value(seo_yandex, webmaster_verify, Context), Rest}};
m_get([ <<"jsonld">>, Id | Rest ], _Msg, Context) ->
    case seo_jsonld_webpage:generate(Id, z_acl:anondo(Context)) of
        {ok, JSON} ->
            {ok, {z_json:encode(JSON), Rest}};
        {error, _} = Error ->
            Error
    end;
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.
