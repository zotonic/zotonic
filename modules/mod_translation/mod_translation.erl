%% -*- coding: utf-8 -*-

%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2011 Marc Worrell
%% Date: 2010-05-19
%% @doc Translation support for i18n.  Generates .po files by scanning templates.

%% Copyright 2010-2016 Marc Worrell
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
%% Language codes (and other language data) are binary strings;
%% In the language config the language objects are stored with binary strings as keys;
%% The selected language in the global Context is an atom, as well as the
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
         try_set_language/2,

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


default_languages() ->
    Languages = languages:languages(),
    lists:map(fun({Code, IsEnabled}) ->
        Props = proplists:get_value(Code, Languages),
        Fallback = case ((proplists:is_defined(territory, Props)) or (proplists:is_defined(script, Props))) of
            true -> proplists:get_value(language, Props);
            false -> undefined
        end,
        {Code, [
            {is_enabled, IsEnabled},
            {fallback, Fallback}
        | Props]}
    end, [
        {<<"ar">>, false},
        {<<"de">>, true},
        {<<"en">>, true},
        {<<"es">>, true},
        {<<"et">>, true},
        {<<"fr">>, true},
        {<<"nl">>, true},
        {<<"pl">>, true},
        {<<"ru">>, true},
        {<<"tr">>, true},
        {<<"zh">>, false}
    ]).


%% @doc Check if the user has a prefered language (in the user's persistent data). If not
%%      then check the accept-language header (if any) against the available languages.
observe_session_init_fold(session_init_fold, Context, _Context) ->
    case get_q_language(Context) of
        undefined -> maybe_persistent(Context);
        QsLang -> try_set_language(QsLang, Context)
    end.


maybe_persistent(Context) ->
    case z_context:get_persistent(language, Context) of
        undefined ->
            maybe_configuration(Context);
        Language ->
            do_set_language(Language, Context)
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
        undefined ->
            Context;
        AcceptLanguage ->
            try_set_language(AcceptLanguage, Context)
    end.


get_q_language(Context) ->
    case z_context:get_q_all("z_language", Context) of
        [] -> undefined;
        L -> lists:last(L)
    end.


observe_session_context(session_context, Context, _Context) ->
    Context1 = case z_context:get_session(language, Context) of
        undefined -> Context;
        Language -> Context#context{language=Language}
    end,
    case get_q_language(Context1) of
        undefined -> Context1;
        QsLang -> try_set_language(QsLang, Context1)
    end.


observe_user_context(#user_context{id=UserId}, Context, _Context) ->
    case m_rsc:p_no_acl(UserId, pref_language, Context) of
        Code when Code /= undefined ->
            do_set_language(Code, Context);
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
            % Switch the session to the default language of the user
            Context1 = set_language(Code, Context),
            z_context:set_persistent(language, z_context:language(Context1), Context1),
            Context1
    end.


observe_set_user_language(#set_user_language{id=UserId}, Context, _Context) when is_integer(UserId) ->
    case m_rsc:p_no_acl(UserId, pref_language, Context) of
        Code when Code /= undefined -> do_set_language(Code, Context);
        _ -> Context
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


observe_dispatch_rewrite(#dispatch_rewrite{is_dir=IsDir}, {Parts, Args} = Dispatch, Context) ->
    case Parts of
        % Special case for the 'id' controller
        ["id", Other] ->
            case z_utils:only_digits(Other) of
                true -> Dispatch;
                false ->
                    case is_enabled_language("id", Context) of
                        true -> {[Other], [{z_language, "id"}|Args]};
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
    % reloads page via javascript (zotonic-1.0.js)
    RewriteUrl = z_convert:to_bool(m_config:get_value(?MODULE, rewrite_url, true, Context1)),
    Language = case RewriteUrl of
        true -> z_context:language(Context1);
        false -> ""
    end,
    z_render:wire({reload, [{z_language, Language}]}, Context1);

%% @doc Set the default language.
event(#postback{message={language_default, Args}}, Context) ->
    case z_acl:is_allowed(use, ?MODULE, Context) of
        true ->
            {code, Code} = proplists:lookup(code, Args),
            m_config:set_value(i18n, language, z_convert:to_binary(Code), Context),
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
            language_add(OldCode, z_context:get_q("code", Context), z_context:get_q("fallback", Context), z_context:get_q("is_enabled", Context), Context),
            Context1 = z_render:dialog_close(Context),
            z_render:wire({reload, []}, Context1);
        false ->
            z_render:growl_error(?__("Sorry, you don't have permission to change the language list.", Context), Context)
    end;

event(#postback{message={language_delete, Args}}, Context) ->
    case z_acl:is_allowed(use, ?MODULE, Context) of
        true ->
            {code, Code} = proplists:lookup(code, Args),
            language_delete(Code, Context),
            Context1 = z_render:dialog_close(Context),
            z_render:wire({reload, []}, Context1);
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
    end.


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
-spec set_user_language(binary(), #context{}) -> #context{}.
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


%% @doc Set the language of the user. Only done when the found language is a known language.
try_set_language(LanguageRequested, Context) when is_atom(LanguageRequested) ->
    try_set_language(atom_to_list(LanguageRequested), Context);
try_set_language(LanguageRequested, Context) when is_binary(LanguageRequested) ->
    try_set_language(binary_to_list(LanguageRequested), Context);
try_set_language(LanguageRequested, Context) when is_list(LanguageRequested) ->
    LanguagesAvailable = [ z_convert:to_list(Lang)
                           || {Lang, Props} <- get_language_config(Context),
                              proplists:get_value(is_enabled, Props) =:= true
                         ],
    case catch do_choose(LanguagesAvailable, LanguageRequested) of
        Lang when is_list(Lang) ->
            % Base language accepted (f.i. 'en'), now use the full language (f.i. 'en-us')
            do_set_language(LanguageRequested, Context);
        none ->
            Context;
        _Error ->
            % @todo log the error, might be a problem in the accept-language header
            Context
    end.


%% @doc Set the language of the current user/session.
set_language(Code, Context) ->
    ConfigLanguages = get_language_config(Context),
    Language = proplists:get_value(Code, ConfigLanguages),
    case Language of
        undefined ->
            % Error, desired language is not in config
            Context;
        _ ->
            case proplists:get_value(is_enabled, Language) of
                true -> do_set_language(Code, Context);
                _ ->
                    % Error, desired language is not enabled
                    Context
            end
    end.


-spec do_set_language(atom(), #context{}) -> #context{}.
do_set_language(CodeAtom, Context) when is_atom(CodeAtom) ->
    ConfigLanguages = get_language_config(Context),
    FallbackCodeAtom = fallback_language_code(CodeAtom),
    {CodeAtom1, FallbackCodeAtom1} = case proplists:is_defined(z_convert:to_binary(CodeAtom), ConfigLanguages) of
        true -> {CodeAtom, FallbackCodeAtom};
        false -> {FallbackCodeAtom, undefined}
    end,
    Langs = case FallbackCodeAtom1 of
        undefined -> [CodeAtom1];
        _ -> [CodeAtom1, FallbackCodeAtom1]
    end,
    Context1 = z_context:set_language(Langs, Context),
    case {z_context:language(Context), z_context:language(Context1)} of
        {CodeAtom1, CodeAtom1} ->
            Context1;
        _ ->
            z_context:set_session(language, Langs, Context1),
            Context1
    end;
do_set_language(Code, Context) ->
    do_set_language(z_convert:to_atom(Code), Context).


-spec fallback_language_code(atom()) -> atom().
fallback_language_code(CodeAtom) ->
    Languages = languages:languages(),
    case proplists:get_value(z_convert:to_binary(CodeAtom), Languages) of
        undefined -> en; % ultimate fallback
        Props ->
            FallbackLanguage = proplists:get_value(language, Props),
            z_convert:to_atom(FallbackLanguage)
    end.


%% @doc Add a language to the i18n configuration
language_add(OldLanguageCode, NewLanguageCode, _FallbackLanguageCode, IsEnabled, Context) ->
    OldCode = z_convert:to_binary(z_string:trim(OldLanguageCode)),
    NewCode = z_convert:to_binary(z_string:trim(NewLanguageCode)),
    Languages = languages:languages(),
    Props = proplists:get_value(NewCode, Languages),

    Props1 = [
        {is_enabled, z_convert:to_bool(IsEnabled)},
        {fallback, proplists:get_value(language, Props)}
    | Props],
    ConfigLanguages = get_language_config(Context),
    ConfigLanguages1 = proplists:delete(OldCode, ConfigLanguages),
    ConfigLanguages2 = lists:usort([{NewCode, Props1} | ConfigLanguages1]),
    set_language_config(ConfigLanguages2, Context).


%% @doc Remove a language from the i18n configuration
language_delete(LanguageCode, Context) ->
    ConfigLanguages = get_language_config(Context),
    ConfigLanguages1 = proplists:delete(LanguageCode, ConfigLanguages),
    set_language_config(ConfigLanguages1, Context).


%% @doc Set/reset the is_enabled flag of a language.
language_enable(Code, IsEnabled, Context) ->
    ConfigLanguages = get_language_config(Context),
    Language = proplists:get_value(Code, ConfigLanguages),
    Language1 = [{is_enabled, IsEnabled} | proplists:delete(is_enabled, Language)],
    ConfigLanguages1 = lists:usort([{Code, Language1} | proplists:delete(Code, ConfigLanguages)]),
    set_language_config(ConfigLanguages1, Context).


is_multiple_languages_config(Context) ->
    length(get_enabled_languages(Context)) > 1.


-spec is_enabled_language(binary(), #context{}) -> boolean().
is_enabled_language(LanguageCode, Context) ->
    case z_trans:is_language(LanguageCode) of
        true ->
            case lists:keyfind(LanguageCode, 1, get_enabled_languages(Context)) of
                false -> false;
                _ -> true
            end;
        false ->
            false
    end.

get_enabled_languages(Context) ->
    case z_memo:get('mod_translation$enabled_languages') of
        V when is_boolean(V) ->
            V;
        _ ->
            ConfigLanguages = lists:filter(fun({_,Props}) -> proplists:get_value(is_enabled, Props) =:= true end, get_language_config(Context)),
            z_memo:set('mod_translation$enabled_languages', ConfigLanguages)
    end.

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
                    % Simplify "en-us" to "en"
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

