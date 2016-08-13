%% -*- coding: utf-8 -*-

%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2011 Marc Worrell
%% Date: 2010-05-19
%% @doc Translation support. Generates .po files by scanning templates.

%% Copyright 2010-2016 Marc Worrell
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
%% Language data (read from languages.erl) are binary strings.
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

         url_strip_language/1,
         set_user_language/2,
         language_config/1,
         enabled_languages/1,

         init/1,
         event/2,
         generate/1,

         do_choose/2
        ]).

-include("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").


%% @doc Make sure that we have the i18n.language_list setting when the site starts up.
init(Context) ->
    case z_context:site(Context) of
        zotonic_status ->
            ok;
        _Other ->
            case m_config:get(i18n, language_list, Context) of
                undefined ->
                    m_config:set_prop(i18n, language_list, list, default_languages(), Context);
                _Exists ->
                    ok
            end,
            % set default language
            case m_config:get_value(i18n, language, Context) of
                undefined -> m_config:set_value(i18n, language, <<"en">>, Context);
                _ -> ok
            end
    end.

<<<<<<< HEAD

default_languages() ->
    Languages = languages:languages(),
    lists:map(fun({Code, IsEnabled}) ->
        Props = proplists:get_value(atom_to_binary(Code, utf8), Languages),
        case Props of
            undefined ->
                lager:warning("Error: default_languages: language ~p does not exist in languages.erl", [Code]),
                [];
            _ ->
                Fallback = case ((proplists:is_defined(territory, Props)) or (proplists:is_defined(script, Props))) of
                    true -> proplists:get_value(language, Props);
                    false -> undefined
                end,
                {Code, [
                    {is_enabled, IsEnabled},
                    {fallback, Fallback}
                | Props]}
        end
    end, [
        {ar, false},
        {de, true},
        {en, true},
        {es, true},
        {et, true},
        {fr, true},
        {nl, true},
        {pl, true},
        {ru, true},
        {tr, true},
        {zh, false}
    ]).
=======
default_languages() ->
    [
     {ar, [ {language, <<"العربية"/utf8>>}, {is_enabled, false}]},
     {de, [ {language, <<"Deutsch"/utf8>>}, {is_enabled, true}]},
     {en, [ {language, <<"English"/utf8>>}, {is_enabled, true}]},
     {es, [ {language, <<"Español"/utf8>>}, {is_enabled, true}]},
     {et, [ {language, <<"Eesti"/utf8>>}, {is_enabled, true}]},
     {fr, [ {language, <<"Français"/utf8>>}, {is_enabled, true}]},
     {nl, [ {language, <<"Nederlands"/utf8>>}, {is_enabled, true}]},
     {pl, [ {language, <<"Polski"/utf8>>}, {is_enabled, true}]},
     {ru, [ {language, <<"Русский"/utf8>>}, {is_enabled, true}]},
     {tr, [ {language, <<"Türkçe"/utf8>>}, {is_enabled, true}]},
     {zh, [ {language, <<"中文"/utf8>>}, {is_enabled, false}]}
    ].
>>>>>>> master


%% @doc Check if the user has a prefered language (in the user's persistent data). If not
%%      then check the accept-language header (if any) against the available languages.
observe_session_init_fold(session_init_fold, Context, _Context) ->
    case get_q_language(Context) of
        undefined -> maybe_persistent(Context);
        QsLang -> set_language(QsLang, Context)
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
    case z_context:get_req_header("accept-language", Context) of
        undefined -> Context;
        AcceptLanguage -> set_language(list_to_atom(AcceptLanguage), Context)
    end.


-spec get_q_language(#context{}) -> atom().
get_q_language(Context) ->
    case z_context:get_q_all("z_language", Context) of
        [] -> undefined;
        L -> list_to_atom(lists:last(L))
    end.


observe_session_context(session_context, Context, _Context) ->
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
<<<<<<< HEAD
        Code when Code /= undefined ->
            set_language(Code, Context);
=======
        Code when is_atom(Code), Code /= undefined ->
            do_set_language(Code, Context);
>>>>>>> master
        _ ->
            Context
    end.


observe_auth_logon(auth_logon, Context, _Context) ->
    UserId = z_acl:user(Context),
    case m_rsc:p_no_acl(UserId, pref_language, Context) of
        undefined ->
            % Ensure that the user has a default language
            catch m_rsc:update(UserId, [{pref_language, z_context:language(Context)}], Context),
            Context;
        Code ->
<<<<<<< HEAD
            % Switch the session to the default language of the user
            Context1 = set_language(Code, Context),
=======
                                                % Switch the session to the default language of the user
            List = get_language_config(Context),
            Context1 = set_language(z_convert:to_list(Code), List, Context),
>>>>>>> master
            z_context:set_persistent(language, z_context:language(Context1), Context1),
            Context1
    end.


observe_set_user_language(#set_user_language{id=UserId}, Context, _Context) when is_integer(UserId) ->
    case m_rsc:p_no_acl(UserId, pref_language, Context) of
<<<<<<< HEAD
        Code when Code /= undefined -> set_language(Code, Context);
        _ -> Context
=======
        Code when is_atom(Code), Code /= undefined ->
            do_set_language(Code, Context);
        _ ->
            Context
>>>>>>> master
    end;
observe_set_user_language(#set_user_language{}, Context, _Context) ->
    Context.


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


%% @doc Set the current session (and user) language, reload the user agent's page.
event(#postback{message={set_language, Args}}, Context) ->
    Code = case proplists:get_value(code, Args) of
        undefined -> z_context:get_q("triggervalue", Context);
        ArgCode -> ArgCode
    end,
    Context1 = set_user_language(Code, Context),
    reload_page(Context1);

%% @doc Set the default language.
event(#postback{message={language_default, Args}}, Context) ->
    case z_acl:is_allowed(use, ?MODULE, Context) of
        true ->
            {code, Code} = proplists:lookup(code, Args),
            m_config:set_value(i18n, language, Code, Context),
            Context;
        false ->
            z_render:growl_error(?__("Sorry, you don't have permission to set the default language.", Context), Context)
    end;

%% @doc Start rescanning all templates for translation tags.
event(#postback{message={translation_generate, _Args}}, Context) ->
    case z_acl:is_allowed(use, ?MODULE, Context) of
        true ->
            spawn(fun() -> generate(Context) end),
            z_render:growl(?__("Started building the .pot files. This may take a while...", Context), Context);
        false ->
            z_render:growl_error(?__("Sorry, you don't have permission to scan for translations.", Context), Context)
    end;
event(#postback{message={translation_reload, _Args}}, Context) ->
    case z_acl:is_allowed(use, ?MODULE, Context) of
        true ->
            spawn(fun() -> z_trans_server:load_translations(Context) end),
            z_render:growl(?__("Reloading all .po files in the background.", Context), Context);
        false ->
            z_render:growl_error(?__("Sorry, you don't have permission to reload translations.", Context), Context)
    end;

event(#submit{message={language_edit, Args}}, Context) ->
    case z_acl:is_allowed(use, ?MODULE, Context) of
        true ->
            OldCode = proplists:get_value(code, Args, '$empty'),
<<<<<<< HEAD
            language_add(z_string:trim(OldCode), z_string:trim(z_context:get_q("code", Context)), z_context:get_q("is_enabled", Context), Context),
=======
            language_add(OldCode, z_context:get_q("code", Context), z_context:get_q("language", Context), z_context:get_q("fallback", Context),
                         z_context:get_q("is_enabled", Context), Context),
>>>>>>> master
            Context1 = z_render:dialog_close(Context),
            z_render:wire({reload, []}, Context1);
        false ->
            z_render:growl_error(?__("Sorry, you don't have permission to change the language list.", Context), Context)
    end;

event(#postback{message={language_delete, Args}}, Context) ->
    case z_acl:is_allowed(use, ?MODULE, Context) of
        true ->
            {code, Code} = proplists:lookup(code, Args),
            DeletesCurrentLanguage = z_context:language(Context) =:= Code,
            language_delete(Code, Context),
            Context1 = case DeletesCurrentLanguage of
                true ->
                    Fallback = fallback_language_code(Code),
                    set_language(Fallback, Context);
                false -> Context
            end,
            Context2 = z_render:dialog_close(Context1),
            reload_page(Context2);
        false ->
            z_render:growl_error(?__("Sorry, you don't have permission to change the language list.", Context), Context)
    end;

event(#postback{message={language_enable, Args}}, Context) ->
    case z_acl:is_allowed(use, ?MODULE, Context) of
        true ->
            {code, Code} = proplists:lookup(code, Args),
            language_enable(Code, z_convert:to_bool(z_context:get_q("triggervalue", Context)), Context),
            z_render:wire({reload, []}, Context);
        false ->
            z_render:growl_error(?__("Sorry, you don't have permission to change the language list.", Context), Context)
    end;

event(#postback{message={toggle_url_rewrite, _Args}}, Context) ->
    Context1 = mod_admin_config:config_toggle("mod_translation", "rewrite_url", Context),
    reload_page(Context1).


%% @doc Strip the language code from the location (if the language code is recognized).
%%      For instance: <<"/nl-nl/admin/translation">> becomes <<"/admin/translation">>
url_strip_language([$/,A,B,$/ | Rest] = Url) ->
    url_strip_language1(Url, [A,B], Rest);
url_strip_language([$/,A,B,$-,C,D,$/ | Rest] = Url) ->
    url_strip_language1(Url, [A,B,"-",C,D], Rest);
url_strip_language(<<$/,A,B,$/, Rest/binary>> = Url) ->
    url_strip_language1(Url, [A,B], Rest);
url_strip_language(<<$/,A,B,$-,C,D,$/, Rest/binary>> = Url) ->
    url_strip_language1(Url, [A,B,<<"-">>,C,D], Rest);
url_strip_language(Url) ->
    Url.

url_strip_language1(Url, LanguageCode, Rest) when is_list(Url) ->
    case z_trans:is_language(LanguageCode) of
        true -> [$/|Rest];
        false -> Url
    end;
url_strip_language1(Url, LanguageCode, Rest) when is_binary(Url) ->
    case z_trans:is_language(LanguageCode) of
        true -> <<$/, Rest/binary>>;
        false -> Url
    end;
url_strip_language1(Url, _LanguageCode, _Rest) ->
    Url.


%% @doc Set the language, as selected by the user. Persist this choice.
-spec set_user_language(atom(), #context{}) -> #context{}.
set_user_language(Code, Context) ->
    Context1 = set_language(Code, Context),
    z_context:set_persistent(language, z_context:language(Context1), Context1),
    case z_acl:user(Context1) of
        undefined ->
            nop;
        UserId ->
            case m_rsc:p_no_acl(UserId, pref_language, Context1) of
                Code -> nop;
                _ -> catch m_rsc:update(UserId, [{pref_language, z_context:language(Context1)}], Context1)
            end
    end,
    Context1.


<<<<<<< HEAD
%% @doc Set the language of the current user/session. Sets to the given language if the language exists in the config language and is enabled; otherwise tries the language's fallback language; if this fails too, sets language to English.
-spec set_language(atom(), #context{}) -> #context{}.
set_language(Code0, Context) when is_atom(Code0) ->
    {Code, LanguageData} = valid_config_language(Code0, Context),
    FallbackCode = proplists:get_value(fallback, LanguageData),
    Langs = [L || L <- [Code, FallbackCode], L /= undefined],
=======
%% @doc Set the language of the user. Only done when the found language is a known language.
try_set_language(LanguagesRequested, Context) when is_atom(LanguagesRequested) ->
    try_set_language(atom_to_list(LanguagesRequested), Context);
try_set_language(LanguagesRequested, Context) when is_binary(LanguagesRequested) ->
    try_set_language(binary_to_list(LanguagesRequested), Context);
try_set_language(LanguagesRequested, Context) when is_list(LanguagesRequested) ->
    LanguagesAvailable = [ atom_to_list(Lang)
                           || {Lang, Props} <- get_language_config(Context),
                              proplists:get_value(is_enabled, Props) =:= true
                         ],
    case catch do_choose(LanguagesAvailable, LanguagesRequested) of
        Lang when is_list(Lang) ->
            do_set_language(list_to_atom(Lang), Context);
        none ->
            Context;
        _Error ->
                                                % @todo log the error, might be a problem in the accept-language header
            Context
    end.


%% @doc Set the language of the current user/session
set_language(Code, [{CodeAtom, _Language}|Other], Context) ->
    case z_convert:to_list(CodeAtom) of
        Code ->   do_set_language(CodeAtom, Context);
        _Other -> set_language(Code, Other, Context)
    end.

do_set_language(Code, Context) when is_atom(Code) ->
    List = get_language_config(Context),
    LangProps = proplists:get_value(Code, List),
    FallbackLangCode = proplists:get_value(fallback, LangProps),
    Langs = case FallbackLangCode of
        undefined -> [Code];
        _ -> [Code, z_convert:to_atom(FallbackLangCode)]
    end,
>>>>>>> master
    Context1 = z_context:set_language(Langs, Context),
    case {z_context:language(Context), z_context:language(Context1)} of
        {Code, Code} ->
            % nothing changed
            Context1;
        _ ->
            z_context:set_session(language, Langs, Context1),
            Context1
    end;
set_language(Code, Context) ->
    set_language(z_convert:to_atom(Code), Context).


%% @doc Returns a valid language from the config language. If the language is not
%%      available or not enabled, tries the language's fallback language (retrieve from
%%      languages.erl); if this fails too, returns the data for the English language.
-spec valid_config_language(atom(), #context{}) -> {atom(), list()}.
valid_config_language(Code, Context) when Code =:= undefined ->
    valid_config_language(en, Context);
valid_config_language(Code, Context) ->
    EnabledLanguages = enabled_languages(Context),
    case proplists:get_value(Code, EnabledLanguages) of
        undefined ->
            % Language code is not listed in config, let's try a fallback
            FallbackCode = fallback_language_code(Code),
            valid_config_language(FallbackCode, Context);
        LanguageData ->
            % Language is listed and enabled
            {Code, LanguageData}
    end.

-spec fallback_language_code(atom() | undefined) -> atom().
fallback_language_code(Code) when Code =:= undefined ->
    en;
fallback_language_code(Code) ->
    Languages = languages:languages(),
    case proplists:get_value(atom_to_binary(Code, latin1), Languages) of
        undefined -> en;
        LanguageData ->
            LanguageCode = proplists:get_value(language, LanguageData),
            LanguageCodeAtom = binary_to_atom(LanguageCode, latin1),
            case LanguageCodeAtom =:= Code of
                true -> en;
                false -> LanguageCodeAtom
            end
    end.


%% @doc Add a language to the i18n configuration
<<<<<<< HEAD
language_add(OldLanguageCode, NewLanguageCode, IsEnabled, Context) ->
    OldCode = z_convert:to_atom(OldLanguageCode),
    NewCode = z_convert:to_atom(NewLanguageCode),
    NewCodeBin = z_convert:to_binary(NewCode),
    Languages = languages:languages(),
    case proplists:is_defined(NewCodeBin, Languages) of
        false ->
            lager:warning("Error: language_add: language ~p does not exist", [NewLanguageCode]),
            ok;
        true ->
            Props = proplists:get_value(NewCodeBin, Languages),
            Props1 = [
                {is_enabled, z_convert:to_bool(IsEnabled)},
                {fallback, proplists:get_value(language, Props)}
            | Props],
            ConfigLanguages = language_config(Context),
            ConfigLanguages1 = proplists:delete(OldCode, ConfigLanguages),
            ConfigLanguages2 = lists:usort([{NewCode, Props1} | ConfigLanguages1]),
            set_language_config(ConfigLanguages2, Context)
    end.
=======
language_add(OldIsoCode, NewIsoCode, Language, FallbackIsoCode, IsEnabled, Context) ->
    IsoCodeNewAtom = z_convert:to_atom(z_string:to_name(z_string:trim(NewIsoCode))),
    FallbackIsoCodeAtom = z_convert:to_atom(z_string:to_name(z_string:trim(FallbackIsoCode))),
    Languages = get_language_config(Context),
    Languages1 = proplists:delete(OldIsoCode, Languages),
    Languages2 = lists:usort([{IsoCodeNewAtom,
                               [{language, z_convert:to_binary(z_string:trim(z_html:escape(Language)))},
                                {fallback, FallbackIsoCodeAtom},
                                {is_enabled, z_convert:to_bool(IsEnabled)}
                               ]} | Languages1]),
    set_language_config(Languages2, Context).
>>>>>>> master


%% @doc Remove a language from the i18n configuration
-spec language_delete(atom(), #context{}) -> undefined.
language_delete(LanguageCode, Context) ->
    ConfigLanguages = language_config(Context),
    ConfigLanguages1 = proplists:delete(LanguageCode, ConfigLanguages),
    set_language_config(ConfigLanguages1, Context).


%% @doc Set/reset the is_enabled flag of a language.
-spec language_enable(atom(), boolean(), #context{}) -> undefined.
language_enable(Code, IsEnabled, Context) ->
    ConfigLanguages = language_config(Context),
    Language = proplists:get_value(Code, ConfigLanguages),
    Language1 = [{is_enabled, IsEnabled} | proplists:delete(is_enabled, Language)],
    ConfigLanguages1 = lists:usort([{Code, Language1} | proplists:delete(Code, ConfigLanguages)]),
    set_language_config(ConfigLanguages1, Context).


%% @doc Reloads the page via javascript (zotonic-1.0.js).
-spec reload_page(#context{}) -> #context{}.
reload_page(Context) ->
    RewriteUrl = z_convert:to_bool(m_config:get_value(?MODULE, rewrite_url, true, Context)),
    Language = case RewriteUrl of
        true -> z_context:language(Context);
        false -> ""
    end,
    z_render:wire({reload, [{z_language, Language}, {z_rewrite_url, RewriteUrl}]}, Context).


is_multiple_languages_config(Context) ->
    length(enabled_languages(Context)) > 1.


-spec is_enabled_language(binary(), #context{}) -> boolean().
is_enabled_language(LanguageCode, Context) ->
    case lists:keyfind(binary_to_atom(LanguageCode, latin1), 1, enabled_languages(Context)) of
        false -> false;
        _ -> true
    end.


enabled_languages(Context) ->
    case z_memo:get('mod_translation$enabled_languages') of
        V when is_boolean(V) ->
            V;
        _ ->
            ConfigLanguages = lists:filter(fun({_,Props}) -> proplists:get_value(is_enabled, Props) =:= true end, language_config(Context)),
            z_memo:set('mod_translation$enabled_languages', ConfigLanguages)
    end.


%% @doc Get the list of languages
language_config(Context) ->
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


%% do_choose/2 is adapted from webmachine_util:do_choose/3
%% Original code copyright 2007-2008 Basho Technologies
do_choose(Choices, Header) ->
    Accepted = build_conneg_list(string:tokens(Header, ",")),
    StarPrio = [P || {P,C} <- Accepted, C =:= "*"],
    AnyOkay = case StarPrio of
                  [] -> no;
                  [0.0] -> no;
                  _ -> yes
              end,
    do_choose(AnyOkay, Choices, Accepted).
do_choose(_AnyOkay, [], []) ->
    none;
do_choose(_AnyOkay, [], _Accepted) ->
    none;
do_choose(yes, Choices, []) ->
    hd(Choices);
do_choose(no, _Choices, []) ->
    none;
do_choose(AnyOkay, Choices, [AccPair|AccRest]) ->
    {Prio, Acc} = AccPair,
    case Prio of
        0.0 ->
            do_choose(AnyOkay, lists:delete(Acc, Choices), AccRest);
        _ ->
            LAcc = string:to_lower(Acc),
            LChoices = [string:to_lower(X) || X <- Choices],
                                                % doing this a little more work than needed in
                                                % order to be easily insensitive but preserving
            case lists:member(LAcc, LChoices) of
                true ->
                    hd([X || X <- Choices,
                             string:to_lower(X) =:= LAcc]);
                false -> do_choose(AnyOkay, Choices, AccRest)
            end
    end.

build_conneg_list(AccList) ->
    build_conneg_list(AccList, []).
build_conneg_list([], Result) -> lists:reverse(lists:sort(Result));
build_conneg_list([Acc|AccRest], Result) ->
    XPair = list_to_tuple([string:strip(X) || X <- string:tokens(Acc, ";")]),
    Pair = case XPair of
               {Choice, "q=" ++ PrioStr} ->
<<<<<<< HEAD
                    % Simplify "en-us" to "en"
=======
                                                % Simplify "en-us" to "en"
>>>>>>> master
                   [Choice1|_] = string:tokens(Choice, "-"),
                   case PrioStr of
                       "0" -> {0.0, Choice1};
                       "1" -> {1.0, Choice1};
                       [$.|_] -> {list_to_float([$0|PrioStr]), Choice1};
                       _ -> {list_to_float(PrioStr), Choice1}
                   end;
               {Choice} ->
                   {1.0, hd(string:tokens(Choice, "-"))}
           end,
    build_conneg_list(AccRest,[Pair|Result]).


observe_admin_menu(admin_menu, Acc, Context) ->
    [
     #menu_item{id=admin_translation,
                parent=admin_structure,
                label=?__("Translation", Context),
                url={admin_translation},
                visiblecheck={acl, use, ?MODULE}}

     |Acc].

