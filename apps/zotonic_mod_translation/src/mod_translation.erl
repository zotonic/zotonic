%% -*- coding: utf-8 -*-

%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2017 Marc Worrell, Arthur Clemens
%% @doc Translation support. Generates .po files by scanning templates.

%% Copyright 2010-2017 Marc Worrell
%% Copyright 2016 Arthur Clemens
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


%% About variable types:
%% Language data (read from z_language) are binary strings.
%% When used in code (such as the language config), language codes are atoms;
%% the selected language in the global Context is an atom, as well as the
%% fallback language.


-module(mod_translation).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Translation").
-mod_description("Handle user’s language and generate .pot files with translatable texts.").
-mod_prio(500).
-mod_depends([admin]).
-mod_provides([translation]).

-export([
    observe_session_init_fold/3,
    observe_session_context/3,
    observe_auth_logon/3,
    observe_user_context/3,
    observe_set_user_language/3,
    observe_url_rewrite/3,
    observe_dispatch_rewrite/3,
    observe_scomp_script_render/2,
    observe_admin_menu/3,

    set_user_language/2,
    set_default_language/2,
    language_add/3,
    language_delete/2,
    language_enable/3,
    set_language_url_rewrite/2,
    language_config/1,
    enabled_languages/1,
    url_strip_language/1,
    valid_config_language/2,

    init/1,
    event/2,
    generate/1,
    generate_core/0
]).


-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").


%% @doc Make sure that we have the i18n.language_list setting when the site starts up.
init(Context) ->
    case z_context:site(Context) of
        zotonic_site_status ->
            ok;
        _Other ->
            case m_config:get(i18n, language_list, Context) of
                undefined ->
                    m_config:set_prop(i18n, language_list, list, default_languages(), Context);
                Existing ->
                    maybe_update_config_list(Existing, Context),
                    ok
            end,
            % set default language
            case m_config:get_value(i18n, language, Context) of
                undefined -> m_config:set_value(i18n, language, z_language:default_language(Context), Context);
                _ -> ok
            end
    end.


%% @private
default_languages() ->
    lists:foldl(fun({Code, IsEnabled}, Acc) ->
        case language_list_item(Code, IsEnabled) of
            undefined -> Acc;
            Item -> [Item|Acc]
        end
    end, [], [
        {ar, false},
        {de, true},
        {en, true},
        {es, true},
        {et, true},
        {fr, true},
        {nl, true},
        {pl, true},
        {'pt-br', true},
        {ru, true},
        {tr, true},
        {zh, false}
    ]).


%% @doc Check if the user has a prefered language (in the user's persistent data). If not
%%      then check the accept-language header (if any) against the available languages.
observe_session_init_fold(#session_init_fold{}, Context, _Context) ->
    case get_q_language(Context) of
        undefined -> maybe_persistent(Context);
        QsLang ->
            set_language(QsLang, Context)
    end.


maybe_persistent(Context) ->
    case z_context:get_persistent(language, Context) of
        undefined ->
            maybe_configuration(Context);
        Language ->
            set_language(Language, Context)
    end.


maybe_configuration(Context) ->
    case z_convert:to_bool(m_config:get_value(?MODULE, force_default, Context)) of
        true ->
            case m_config:get_value(i18n, language, Context) of
                undefined -> maybe_accept_header(Context);
                <<>> -> maybe_accept_header(Context);
                _Lang -> Context  % Already set by context init
            end;
        false ->
            maybe_accept_header(Context)
    end.


maybe_accept_header(Context) ->
    case z_context:get_req_header(<<"accept-language">>, Context) of
        undefined -> Context;
        AcceptHeader ->
            Enabled = acceptable_languages(Context),
            case cowmachine_accept_language:accept_header(Enabled, AcceptHeader) of
                {ok, Lang} -> set_language(binary_to_atom(Lang, utf8), Context);
                {error, _} -> Context
            end
    end.

% Fetch a list of acceptable languages and their fallback languages
% Store this in the depcache (and memo) for quick(er) lookups.
acceptable_languages(Context) ->
    z_depcache:memo(
        fun() ->
            Enabled = enabled_languages(Context),
            lists:map(
                fun({LangAtom,Opts}) ->
                    Lang = atom_to_binary(LangAtom, utf8),
                    case lists:keyfind(fallback, 1, Opts) of
                        {fallback, Fallback} -> {Lang,[Fallback]};
                        false -> {Lang,[]}
                    end
                end,
                Enabled)
        end,
        acceptable_languages,
        3600,
        [config],
        Context).


-spec get_q_language(#context{}) -> atom().
get_q_language(Context) ->
    case z_context:get_q_all(<<"z_language">>, Context) of
        [] -> undefined;
        L ->
            Enabled = acceptable_languages(Context),
            case cowmachine_accept_language:accept_list(Enabled, [lists:last(L)]) of
                {ok, Lang} -> binary_to_atom(Lang, utf8);
                {error, _} -> undefined
            end
    end.


observe_session_context(#session_context{}, Context, _Context) ->
    Context1 = case z_context:get_session(language, Context) of
        undefined -> Context;
        Language -> Context#context{language=Language}
    end,
    case get_q_language(Context1) of
        undefined -> Context1;
        QsLang -> set_language(QsLang, Context1)
    end.


observe_user_context(#user_context{id=UserId}, Context, _Context) ->
    case m_rsc:p_no_acl(UserId, pref_language, Context) of
        Code when Code /= undefined ->
            set_language(Code, Context);
        _ ->
            Context
    end.


observe_auth_logon(#auth_logon{}, Context, _Context) ->
    UserId = z_acl:user(Context),
    case m_rsc:p_no_acl(UserId, pref_language, Context) of
        undefined ->
            % Ensure that the user has a default language
            catch m_rsc:update(UserId, [{pref_language, z_context:language(Context)}], Context),
            Context;
        Code ->
            % Switch the session to the default language of the user
            Context1 = set_language(Code, Context),
            z_context:set_persistent(language, z_context:language(Context1), Context1),
            Context1
    end.


observe_set_user_language(#set_user_language{id=UserId}, Context, _Context) when is_integer(UserId) ->
    case m_rsc:p_no_acl(UserId, pref_language, Context) of
        Code when Code /= undefined -> set_language(Code, Context);
        _ -> Context
    end;
observe_set_user_language(#set_user_language{}, Context, _Context) ->
    Context.


observe_url_rewrite(#url_rewrite{}, Url, #context{language=[_,'x-default']}) ->
    Url;
observe_url_rewrite(#url_rewrite{args=Args}, Url, Context) ->
    case z_context:language(Context) of
        undefined ->
            Url;
        Language ->
            case lists:keyfind(z_language, 1, Args) of
                false ->
                    RewriteUrl = z_convert:to_bool(m_config:get_value(?MODULE, rewrite_url, true, Context)),
                    case RewriteUrl andalso is_multiple_languages_config(Context) of
                        true ->
                            % Insert the current language in front of the url
                            <<"/", (z_convert:to_binary(Language))/binary, Url/binary>>;
                        false ->
                            Url
                    end;
                _ ->
                    Url
            end
    end.


%% @doc Grabs the language from the path parts and sets it as the page language (if that
%%      language is enabled).
%%      Note that this works irrespectively of the rewrite_url setting: when rewrite_url
%%      is false and the URL includes the language, we will still read the language
%%      instead of returning a 404.
%%      For handling 'id' (which can either mean /id/PageId or the language Indonesian):
%%      if id is followed by a number, do not use it as language.
observe_dispatch_rewrite(#dispatch_rewrite{is_dir=IsDir}, {Parts, Args} = Dispatch, Context) ->
    case Parts of
        [<<"id">>, Other] ->
            case z_utils:only_digits(Other) of
                true -> Dispatch;
                false ->
                    case is_enabled_language(<<"id">>, Context) of
                        true -> {[Other], [{z_language, <<"id">>}|Args]};
                        false -> Dispatch
                    end
            end;
        [First|Rest] when IsDir orelse Rest /= [] ->
            case is_enabled_language(First, Context) of
                true -> {Rest, [{z_language, First}|Args]};
                false -> Dispatch
            end;
        _ ->
            Dispatch
    end.


observe_scomp_script_render(#scomp_script_render{}, Context) ->
    Language = z_convert:to_binary(z_context:language(Context)),
    [<<"z_language=\"", Language/binary, "\"">>, $; ].


%% @doc Set the current session (and user) language, reload the user agent's page. Called from language switch. Reloads the page to reflect the new setting.
event(#postback{message={set_language, Args}}, Context) ->
    LanguageCode = case proplists:get_value(code, Args) of
               undefined -> z_context:get_q(<<"triggervalue">>, Context);
        ArgCode -> ArgCode
    end,
    Context1 = set_user_language(LanguageCode, Context),
    reload_page(Context1);

%% @doc Set the default language. Reloads the page to reflect the new setting.
event(#postback{message={language_default, Args}}, Context) ->
    {code, LanguageCode} = proplists:lookup(code, Args),
    Context1 = set_default_language(LanguageCode, Context),
    reload_page(Context1);

%% @doc Add a language to the config. Reloads the page to reflect the updated list.
event(#submit{message={language_add, _Args}}, Context) ->
    case z_acl:is_allowed(use, ?MODULE, Context) of
        true ->
            language_add(
                    z_string:trim(z_context:get_q(<<"code">>, Context)),
                    z_convert:to_bool(z_context:get_q(<<"is_enabled">>, Context)),
                    Context),
            z_render:wire({reload, []}, Context);
        false ->
            z_render:growl_error(?__(<<"Sorry, you don't have permission to change the language list.">>, Context), Context)
    end;

%% @doc Removes the language from the config. Reloads the page to reflect the updated list.
event(#postback{message={language_delete, Args}}, Context) ->
    case z_acl:is_allowed(use, ?MODULE, Context) of
        true ->
            {code, LanguageCode} = proplists:lookup(code, Args),
            Context1 = language_delete(LanguageCode, Context),
            reload_page(Context1);
        false ->
            z_render:growl_error(?__(<<"Sorry, you don't have permission to change the language list.">>, Context), Context)
    end;

%% @doc Toggles the enabled state of a language. Reloads the page to reflect the new setting.
event(#postback{message={language_enable, Args}}, Context) ->
    case z_acl:is_allowed(use, ?MODULE, Context) of
        true ->
            {code, Code} = proplists:lookup(code, Args),
            case language_enable(Code, z_convert:to_bool(z_context:get_q(<<"triggervalue">>, Context)), Context) of
                {error, Reason} -> z_render:growl_error(Reason, Context);
                ok -> z_render:wire({reload, []}, Context)
            end;
        false ->
            z_render:growl_error(?__(<<"Sorry, you don't have permission to change the language list.">>, Context), Context)
    end;

%% @doc Toggles the state of the 'rewrite URL' setting. Reloads the page to reflect the new setting.
event(#postback{message={toggle_url_rewrite, _Args}}, Context) ->
    Value = z_convert:to_bool(z_context:get_q(<<"triggervalue">>, Context)),
    set_language_url_rewrite(Value, Context),
    reload_page(Context);

%% @doc Start rescanning all templates for translation tags.
event(#postback{message={translation_generate, _Args}}, Context) ->
    case z_acl:is_allowed(use, ?MODULE, Context) of
        true ->
            case gettext_installed() of
                true ->
                    spawn(fun() -> generate(Context) end),
                    z_render:growl(?__(<<"Started building the .pot files. This may take a while...">>, Context), Context);
                false ->
                    lager:error("Cannot generate translation files because gettext is not installed. See http://docs.zotonic.com/en/latest/developer-guide/translation.html."),
                    z_render:growl_error(?__(<<"Cannot generate translation files because <a href=\"http://docs.zotonic.com/en/latest/developer-guide/translation.html\">gettext is not installed</a>.">>, Context), Context)
            end;
        false ->
            z_render:growl_error(?__(<<"Sorry, you don't have permission to scan for translations.">>, Context), Context)
    end;

%% @doc Reload all translations from the modules and site. All templates will be recompiled.
event(#postback{message={translation_reload, _Args}}, Context) ->
    case z_acl:is_allowed(use, ?MODULE, Context) of
        true ->
            spawn(fun() -> z_trans_server:load_translations(Context) end),
            z_render:growl(?__(<<"Reloading all .po files in the background.">>, Context), Context);
        false ->
            z_render:growl_error(?__(<<"Sorry, you don't have permission to reload translations.">>, Context), Context)
    end.


%% @doc Strip the language code from the location (if the language code is recognized).
%%      For instance: `<<"/nl-nl/admin/translation">>' becomes `<<"/admin/translation">>'
url_strip_language(<<$/, A, B, $/, Rest/binary>> = Url) ->
    url_strip_language1(Url, [A, B], Rest);
url_strip_language(<<$/, A, B, $-, C, D, $/, Rest/binary>> = Url) ->
    url_strip_language1(Url, [A, B, <<"-">>, C, D], Rest);
url_strip_language(Url) ->
    Url.

url_strip_language1(Url, LanguageCode, Rest) when is_binary(Url) ->
    case z_language:is_valid(LanguageCode) of
        true -> <<$/, Rest/binary>>;
        false -> Url
    end.


%% @doc Set the language, as selected by the user. Persist this choice.
-spec set_user_language(atom(), #context{}) -> #context{}.
set_user_language(Code, Context) ->
    Context1 = set_language(Code, Context),
    Context2 = z_context:set_persistent(language, z_context:language(Context1), Context1),
    case z_acl:user(Context2) of
        undefined ->
            nop;
        UserId ->
            case m_rsc:p_no_acl(UserId, pref_language, Context2) of
                Code -> nop;
                _ -> catch m_rsc:update(UserId, [{pref_language, z_context:language(Context2)}], Context2)
            end
    end,
    Context2.


%% @doc Set the language of the current user/session. Sets to the given language if the language exists in the config language and is enabled; otherwise tries the language's fallback language; if this fails too, sets language to the site's default language.
-spec set_language(atom(), #context{}) -> #context{}.
set_language('x-default', Context) ->
    z_context:set_language('x-default', Context);
set_language(Code0, Context) when is_atom(Code0) ->
    case valid_config_language(Code0, Context) of
        {Code, LanguageData} ->
            FallbackCode = proplists:get_value(fallback, LanguageData),
            Langs = [L || L <- [Code, FallbackCode], L /= undefined],
            Context1 = z_context:set_language(Langs, Context),
            case {z_context:language(Context), z_context:language(Context1)} of
                {Code, Code} ->
                    % nothing changed
                    Context1;
                _ ->
                    z_context:set_session(language, Langs, Context1),
                    Context1
            end;
        undefined ->
            Context
    end;
set_language(Code, Context) when is_binary(Code) ->
    set_language(binary_to_existing_atom(Code, utf8), Context);
set_language(Code, Context) when is_list(Code) ->
    set_language(list_to_existing_atom(Code), Context).


%% @doc Set the default language.
-spec set_default_language(atom(), #context{}) -> #context{}.
set_default_language(Code, Context) ->
    case z_acl:is_allowed(use, ?MODULE, Context) of
        true ->
            m_config:set_value(i18n, language, Code, Context),
            Context;
        false ->
            z_render:growl_error(?__(<<"Sorry, you don't have permission to set the default language.">>, Context), Context)
    end.


%% @doc Returns a valid language from the config language. If the language is not
%%      available or not enabled, tries the language's fallback language (retrieve from
%%      z_language); if this fails too, returns the data for the site's default language.
-spec valid_config_language(atom(), #context{}) -> {atom(), list()}.
valid_config_language(Code, Context) ->
    valid_config_language(Code, Context, [Code]).

valid_config_language(undefined, Context, Tries) ->
    Default = z_language:default_language(Context),
    valid_config_language(Default, Context, [Default,undefined|Tries]);
valid_config_language(Code, Context, Tries) ->
    EnabledLanguages = enabled_languages(Context),
    case proplists:get_value(Code, EnabledLanguages) of
        undefined ->
            % Language code is not listed in config, let's try a fallback
            Fallback = z_language:fallback_language(Code, Context),
            case lists:member(Fallback, Tries) of
                true -> undefined;
                false -> valid_config_language(Fallback, Context, [Fallback|Tries])
            end;
        LanguageData ->
            % Language is listed and enabled
            {Code, LanguageData}
    end.


%% @doc Add a language to the i18n configuration
-spec language_add(atom() | binary(), boolean(), #context{}) -> ok | {error, not_a_language}.
language_add(NewLanguageCode, IsEnabled, Context) ->
    NewCode = z_convert:to_atom(NewLanguageCode),
    NewCodeBin = z_convert:to_binary(NewCode),
    Languages = z_language:all_languages(),
    case proplists:is_defined(NewCodeBin, Languages) of
        false ->
            lager:warning("mod_translation error. language_add: language ~p does not exist", [NewLanguageCode]),
            {error, not_a_language};
        true ->
            Props = proplists:get_value(NewCodeBin, Languages),
            Props1 = [
                {is_enabled, IsEnabled},
                {fallback, proplists:get_value(language, Props)}
            | Props],
            ConfigLanguages = language_config(Context),
            ConfigLanguages1 = lists:usort([{NewCode, Props1} | ConfigLanguages]),
            set_language_config(ConfigLanguages1, Context),
            ok
    end.


%% @doc Remove a language from the i18n configuration
-spec language_delete(atom(), #context{}) -> #context{}.
language_delete(LanguageCode, Context) ->
    DeletesCurrentLanguage = z_context:language(Context) =:= LanguageCode,
    remove_from_config(LanguageCode, Context),
    case DeletesCurrentLanguage of
        true ->
            Fallback = z_language:fallback_language(LanguageCode, Context),
            set_language(Fallback, Context);
        false -> Context
    end.

%% @doc Remove a language from the i18n configuration
-spec remove_from_config(atom(), z:context()) -> ok.
remove_from_config(LanguageCode, Context) ->
    ConfigLanguages = language_config(Context),
    ConfigLanguages1 = proplists:delete(LanguageCode, ConfigLanguages),
    set_language_config(ConfigLanguages1, Context).


%% @doc Set/reset the is_enabled flag of a language.
-spec language_enable(atom(), boolean(), #context{}) -> ok | {error, string()}.
language_enable(Code, IsEnabled, Context) ->
    case ((IsEnabled =:= false) and (Code =:= z_language:default_language(Context))) of
        true -> {error, ?__(<<"Sorry, you can't disable default language.">>, Context)};
        false ->
            ConfigLanguages = language_config(Context),
            Language = proplists:get_value(Code, ConfigLanguages),
            Language1 = [{is_enabled, IsEnabled} | proplists:delete(is_enabled, Language)],
            ConfigLanguages1 = lists:usort([{Code, Language1} | proplists:delete(Code, ConfigLanguages)]),
            set_language_config(ConfigLanguages1, Context),
            ok
    end.


%% @doc Set/reset the state of the 'rewrite URL' setting.
-spec set_language_url_rewrite(boolean(), #context{}) -> #context{}.
set_language_url_rewrite(Value, Context) ->
    m_config:set_value(mod_translation, rewrite_url, Value, Context),
    reload_page(Context).


%% @doc Reloads the page via javascript (zotonic-1.0.js).
-spec reload_page(#context{}) -> #context{}.
reload_page(Context) ->
    RewriteUrl = z_convert:to_bool(m_config:get_value(?MODULE, rewrite_url, true, Context)),
    Language = case RewriteUrl of
        true -> z_context:language(Context);
        false -> ""
    end,
    z_render:wire({reload, [{z_language, Language}, {z_rewrite_url, RewriteUrl}]}, Context).


%% @doc Get the list of configured languages that are enabled.
-spec enabled_languages(#context{}) -> list().
enabled_languages(Context) ->
    case z_memo:get('mod_translation$enabled_languages') of
        V when is_list(V) ->
            V;
        _ ->
            ConfigLanguages = lists:filter(
                fun({_,Props}) ->
                    proplists:get_value(is_enabled, Props) =:= true
                end,
                language_config(Context)),
            z_memo:set('mod_translation$enabled_languages', ConfigLanguages)
    end.


%% @doc Get the list of configured languages.
-spec language_config(#context{}) -> list().
language_config(Context) ->
    case m_config:get(i18n, language_list, Context) of
        undefined -> [];
        LanguageConfig -> proplists:get_value(list, LanguageConfig, [])
    end.


%% @private
is_multiple_languages_config(Context) ->
    length(enabled_languages(Context)) > 1.


%% @private
-spec is_enabled_language(binary(), #context{}) -> boolean().
is_enabled_language(LanguageCode, Context) ->
    case maybe_language_code(LanguageCode) of
        true ->
            Enabled = enabled_languages(Context),
            try
                lists:keymember(erlang:binary_to_existing_atom(LanguageCode, utf8), 1, Enabled)
            catch
                error:badarg -> false
            end;
        false ->
            false
    end.

maybe_language_code(<<A,B>>) when A >= $a, A =< $z, B >= $a, B =< $z -> true;
maybe_language_code(<<A,B,$-,_/binary>>) when A >= $a, A =< $z, B >= $a, B =< $z -> true;
maybe_language_code(<<$x,$-,_/binary>>) -> true;
maybe_language_code(_) -> false.


%% @private
set_language_config(NewConfig, Context) ->
    m_config:set_prop(i18n, language_list, list, NewConfig, Context),
    z_memo:delete('mod_translation$enabled_languages').


%% @private
-spec maybe_update_config_list(Existing::list(), Context::#context{}) -> ok.
maybe_update_config_list(Existing, Context) ->
    case proplists:get_value(list, Existing) of
        undefined -> ok;
        List ->
            [{_Key, Props}|_] = List,
            case proplists:is_defined(name, Props) of
                false ->
                    lager:info("mod_translation: Converting language config list from 0.x to 1.0."),
                    m_config:delete(i18n, language_list, Context),
                    NewList = lists:foldl(fun({Code, ItemProps}, Acc) ->
                        IsEnabled = proplists:get_value(is_enabled, ItemProps),
                        case language_list_item(Code, IsEnabled) of
                            undefined -> Acc;
                            Item -> [Item|Acc]
                        end
                    end, [], List),
                    m_config:set_prop(i18n, language_list, list, NewList, Context);
                true -> ok
            end
    end.


%% @private
-spec language_list_item(Code::atom(), IsEnabled::boolean()) -> {atom(), list()} | undefined.
language_list_item(Code, IsEnabled) ->
    Props = proplists:get_value(atom_to_binary(Code, utf8), z_language:all_languages()),
    case Props of
        undefined ->
            lager:warning("mod_translation error. default_languages: language ~p does not exist in z_language, skipping.", [Code]),
            undefined;
        _ ->
            Fallback = case ((proplists:is_defined(region, Props)) or (proplists:is_defined(script, Props))) of
                true -> proplists:get_value(language, Props);
                false -> undefined
            end,
            {Code, [
                {is_enabled, IsEnabled},
                {fallback, Fallback}
            | proplists:delete(sublanguages, Props)]}
    end.


% @doc Generate all .po templates for the given site
generate(#context{} = Context) ->
    ActiveModules = lists:foldl(
        fun({App, _}, Acc) ->
            lists:keydelete(core_app_to_module_name(App), 1, Acc)
        end,
        z_module_manager:active_dir(Context),
        core_apps()),
    Result = translation_po:generate(translation_scan:scan(ActiveModules)),
    _ = generate_core(),
    Result;
generate(Host) when is_atom(Host) ->
    generate(z_context:new(Host)).

%% @doc Generate consolidated translation file zotonic.pot for all core modules.
%%      Both active and inactive modules are indexed, so the generated
%%      translation files are always complete.
-spec generate_core() -> ok | {error, needs_core_zotonic}.
generate_core() ->
    case zotonic_core:is_zotonic_project() of
        true ->
            translation_po:generate(translation_scan:scan(core_apps())),
            consolidate_core();
        false ->
            {error, needs_core_zotonic}
    end.

%% @doc Return a list of all core modules and sites - only for the zotonic git project.
core_apps() ->
    Apps = filelib:wildcard(filename:join([z_path:get_path(), "apps", "zotonic_*"])),
    [ {z_convert:to_atom(filename:basename(Dir)), Dir} || Dir <- Apps ].

core_app_to_module_name(App) when is_atom(App) ->
    case atom_to_list(App) of
        "zotonic_mod_"++Rest -> list_to_atom("mod_"++Rest);
        _ -> App
    end.

%% @doc Consolidate translation files for core modules
consolidate_core() ->
    Command = lists:flatten([
        "msgcat -o ",
        code:priv_dir(zotonic_core) ++ "/translations/zotonic.pot",
        " ",
        filename:join([z_path:get_path(), "apps", "zotonic_*/priv/translations/template/*.pot"])
    ]),
    [] = os:cmd(Command),
    ok.

observe_admin_menu(#admin_menu{}, Acc, Context) ->
    [
     #menu_item{id=admin_translation,
                parent=admin_structure,
                label=?__(<<"Translation">>, Context),
                url={admin_translation},
                visiblecheck={acl, use, ?MODULE}}

     |Acc].

%% @doc Are the gettext tools available?
-spec gettext_installed() -> boolean().
gettext_installed() ->
    os:find_executable("msgcat") =/= false andalso os:find_executable("msgmerge") =/= false.
