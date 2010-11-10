%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @date 2010-05-19
%% @doc Translation support for i18n.  Generates .po files by scanning templates.

%% Copyright 2010 Marc Worrell
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

-module(mod_translation).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Translation").
-mod_description("Generate .po files containing translatable texts by scanning templates.").
-mod_prio(500).

-export([
    init/1, 
    event/2,
    generate/1
]).

-include("zotonic.hrl").


%% @doc Make sure that we have the i18n.language_list setting when the site starts up.
init(Context) ->
    case Context#context.host of
        zotonic_status -> 
            ok;
        _Other ->
            case m_config:get(i18n, language_list, Context) of
                undefined ->
                    m_config:set_prop(i18n, language_list, list, [
                            {en, [ {language, <<"English">>}, {is_enabled, true}]},
                            {fr, [ {language, <<"Français">>}, {is_enabled, true}]},
                            {nl, [ {language, <<"Nederlands">>}, {is_enabled, false}]},
                            {tr, [ {language, <<"Türkçe">>}, {is_enabled, true}]}
                        ], Context);
                _Exists ->
                    ok
            end
    end.


%% @doc Set the current session language, reload the user agent's page.
event({postback, {set_language, Args}, _TriggerId, _TargetId}, Context) ->
    Code = case proplists:get_value(code, Args) of
                undefined -> z_context:get_q("triggervalue", Context);
                ArgCode -> ArgCode
            end,
    List = get_language_config(Context),
    Context1 = set_language(z_convert:to_list(Code), List, Context),
    z_render:wire({reload, []}, Context1);

%% @doc Set the default language.
event({postback, {language_default, Args}, _TriggerId, _TargetId}, Context) ->
    case z_acl:is_allowed(use, ?MODULE, Context) of
        true ->
            {code, Code} = proplists:lookup(code, Args),
            m_config:set_value(i18n, language, z_convert:to_binary(Code), Context),
            Context;
        false ->
            z_render:growl_error("Sorry, you don't have permission to set the default language.", Context)
    end;

%% @doc Start rescanning all templates for translation tags.
event({postback, translation_generate, _TriggerId, _TargetId}, Context) ->
    case z_acl:is_allowed(use, ?MODULE, Context) of
        true ->
            spawn(fun() -> generate(Context) end),
            z_render:growl("Started building the .po templates. This may take a while.", Context);
        false ->
            z_render:growl_error("Sorry, you don't have permission to scan for translations.", Context)
    end;
event({postback, translation_reload, _TriggerId, _TargetId}, Context) ->
    case z_acl:is_allowed(use, ?MODULE, Context) of
        true ->
            spawn(fun() -> z_trans_server:load_translations(Context) end),
            z_render:growl("Reloading all .po template in the background.", Context);
        false ->
            z_render:growl_error("Sorry, you don't have permission to reload translations.", Context)
    end;

event({submit, {language_edit, Args}, _TriggerId, _TargetId}, Context) ->
    case z_acl:is_allowed(use, ?MODULE, Context) of
        true ->
            OldCode = proplists:get_value(code, Args, '$empty'),
            language_add(OldCode, z_context:get_q("code", Context), z_context:get_q("language", Context), 
                        z_context:get_q("is_enabled", Context), Context),
            Context1 = z_render:dialog_close(Context),
            z_render:wire({reload, []}, Context1);
        false ->
            z_render:growl_error("Sorry, you don't have permission to change the language list.", Context)
    end;

event({postback, {language_delete, Args}, _TriggerId, _TargetId}, Context) ->
    case z_acl:is_allowed(use, ?MODULE, Context) of
        true ->
            {code, Code} = proplists:lookup(code, Args),
            language_delete(Code, Context),
            Context1 = z_render:dialog_close(Context),
            z_render:wire({reload, []}, Context1);
        false ->
            z_render:growl_error("Sorry, you don't have permission to change the language list.", Context)
    end;

event({postback, {language_enable, Args}, _TriggerId, _TargetId}, Context) ->
    case z_acl:is_allowed(use, ?MODULE, Context) of
        true ->
            {code, Code} = proplists:lookup(code, Args),
            language_enable(Code, z_convert:to_bool(z_context:get_q("triggervalue", Context)), Context),
            Context;
        false ->
            z_render:growl_error("Sorry, you don't have permission to change the language list.", Context)
    end.


%% @doc Set the language of the current user/session
set_language(Code, [{CodeAtom, _Language}|Other], Context) ->
    case z_convert:to_list(CodeAtom) of
        Code ->
            Context1 = z_context:set_language(CodeAtom, Context),
            z_context:set_session(language, CodeAtom, Context1),
            z_notifier:notify({language, CodeAtom}, Context1),
            Context1;
        _Other ->
            set_language(Code, Other, Context)
    end.


%% @doc Add a language to the i18n configuration
language_add(OldIsoCode, NewIsoCode, Language, IsEnabled, Context) ->
    IsoCodeNewAtom = z_convert:to_atom(z_string:to_name(z_string:trim(NewIsoCode))),
    Languages = get_language_config(Context),
    Languages1 = proplists:delete(OldIsoCode, Languages),
    Languages2 = lists:usort([{IsoCodeNewAtom, 
                                [{language, z_convert:to_binary(z_string:trim(z_html:escape(Language)))},
                                 {is_enabled, z_convert:to_bool(IsEnabled)}
                                ]} | Languages1]),
    set_language_config(Languages2, Context).


%% @doc Remove a language from the i18n configuration
language_delete(IsoCode, Context) ->
    Languages = get_language_config(Context),
    Languages1 = proplists:delete(IsoCode, Languages),
    set_language_config(Languages1, Context).

%% @doc Set/reset the is_enabled flag of a language.
language_enable(Code, IsEnabled, Context) ->
    Languages = get_language_config(Context),
    Lang = proplists:get_value(Code, Languages),
    Lang1 = [{is_enabled, IsEnabled} | proplists:delete(is_enabled, Lang)],
    Languages1 = lists:usort([{Code, Lang1} | proplists:delete(Code, Languages)]),
    set_language_config(Languages1, Context).


%% @doc Get the list of languages
get_language_config(Context) ->
    case m_config:get(i18n, language_list, Context) of
        undefined -> [];
        LanguageConfig -> proplists:get_value(list, LanguageConfig, [])
    end.


set_language_config(NewConfig, Context) ->
    m_config:set_prop(i18n, language_list, list, NewConfig, Context).


% @doc Generate all .po templates for the given site
generate(#context{} = Context) ->
    translation_po:generate(translation_scan:scan(Context));
generate(Host) when is_atom(Host) ->
    translation_po:generate(translation_scan:scan(z_context:new(Host))).


