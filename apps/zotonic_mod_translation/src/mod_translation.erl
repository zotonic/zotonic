%% -*- coding: utf-8 -*-

%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2023 Arthur Clemens, Marc Worrell
%% @doc Translation support. Handle the language list and manage translations.
%% @end

%% Copyright 2010-2023 Arthur Clemens, Marc Worrell
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
-mod_provides([translation]).

-export([
    observe_request_context/3,
    observe_user_context/3,
    observe_set_user_language/3,
    observe_url_rewrite/3,
    observe_dispatch_rewrite/3,
    observe_scomp_script_render/2,
    observe_admin_menu/3,
    observe_language_detect/2,

    set_language/2,
    set_user_language/2,
    set_default_language/2,
    language_add/3,
    language_delete/2,
    language_status/3,
    set_language_url_rewrite/2,
    url_strip_language/1,
    valid_config_language/2,
    get_q_language/1,

    match_accept_header/2,

    init/1,
    event/2,
    generate/1,
    generate_core/0
]).


-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").


-define(LANGUAGE_COOKIE, <<"z.lang">>).
-define(LANGUAGE_COOKIE_MAX_AGE, 3600*24*365).

%% @doc Make sure that we have the i18n.language_list setting when the site starts up.
init(Context) ->
    z_language:initialize_config(Context).

%% @doc Set the language of the context. Sets to the given language if the language exists
%%      in the config language and is enabled; otherwise tries the language's fallback language;
%%      if this fails too, sets language to the site's default language.
-spec set_language(atom() | binary() | list( atom() ), z:context()) -> z:context().
set_language('x-default', Context) ->
    z_context:set_language('x-default', Context);
set_language(Code0, Context) when is_atom(Code0) ->
    case z_context:language(Context) of
        Code0 ->
            Context;
        _ ->
            case valid_config_language(Code0, Context) of
                undefined ->
                    Context;
                Code ->
                    Fallback = [ Code | z_language:fallback_language(Code) ],
                    z_context:set_language(Fallback, Context)
            end
    end;
set_language(Code, Context) when is_binary(Code) ->
    case z_language:is_valid(Code) of
        true ->
            set_language(z_convert:to_atom(Code), Context);
        false ->
            Context
    end;
set_language([Code|_] = Langs, Context) when is_atom(Code) ->
    ContextLangs = z_context:languages(Context),
    if
        Langs =:= ContextLangs ->
            Context;
        true ->
            Enabled = z_language:enabled_languages(Context),
            Langs1 = lists:filter(
                fun(Lang) ->
                    lists:member(Lang, Enabled)
                end,
                Langs),
            z_context:set_language(Langs1, Context)
    end.

%% @doc Check if the user has a preferred language (in the user's config). If not
%%      then check the accept-language header (if any) against the available languages.
observe_request_context(#request_context{ phase = init }, Context, _Context) ->
    CookieLangs = unpack_lang_cookie(z_context:get_cookie(?LANGUAGE_COOKIE, Context)),
    maybe_set_cookie(
        CookieLangs,
        case get_q_language(Context) of
            undefined when CookieLangs =:= [] ->
                maybe_user(Context);
            undefined ->
                set_language(CookieLangs, Context);
            QsLang ->
                % Take cookie languages, move request language to front
                Context1 = z_context:set_language(CookieLangs, Context),
                set_language(QsLang, Context1)
        end);
observe_request_context(#request_context{ phase = auth_status, document = #{ <<"language">> := _LangData } }, Context, _Context) ->
    Context;
observe_request_context(#request_context{}, Context, _Context) ->
    Context.

maybe_set_cookie(CookieLangs, Context) ->
    ContextLangs = z_context:languages(Context),
    if
        CookieLangs =:= ContextLangs ->
            Context;
        true ->
            Langs1 = [ z_convert:to_binary(Lang) || Lang <- ContextLangs ],
            Langs2 = iolist_to_binary(lists:join($:, Langs1)),
            z_context:set_cookie(
                ?LANGUAGE_COOKIE,
                Langs2,
                [
                    {max_age, ?LANGUAGE_COOKIE_MAX_AGE},
                    {path, <<"/">>},
                    {secure, true},
                    {same_site, lax}
                ],
                Context)
    end.

maybe_user(Context) ->
    case z_acl:user(Context) of
        undefined ->
            maybe_configuration(Context);
        UserId ->
            case m_rsc:p_no_acl(UserId, pref_language, Context) of
                undefined -> maybe_configuration(Context);
                Lang -> set_language(Lang, Context)
            end
    end.

maybe_configuration(Context) ->
    case m_config:get_boolean(?MODULE, force_default, Context) of
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
    Context1 = z_context:set_resp_header(<<"vary">>, <<"accept-language">>, Context),
    case z_context:get_req_header(<<"accept-language">>, Context1) of
        undefined ->
            Context;
        AcceptHeader ->
            Accepted = match_accept_header(AcceptHeader, Context),
            z_context:set_language(Accepted, Context)
    end.

%% @doc Reorder the acceptable languages according to the accept-language
%% header order.
-spec match_accept_header(AcceptHeader, Context) -> Accepted when
    AcceptHeader :: binary(),
    Context :: z:context(),
    Accepted :: [ binary() ].
match_accept_header(AcceptHeader, Context) ->
    % 1. Parse the accept header
    case cowmachine_accept_language:parse_header(AcceptHeader) of
        {ok, AcceptList} ->
            % 2. Have a lookup of <[alt-]lang> => <lang>
            Map = z_language:acceptable_languages_map(Context),
            % 3. Map parsed header to list of acceptable languages in accept-language order
            Accepted = lists:foldl(
                fun(Lang, Acc) ->
                    case maps:find(Lang, Map) of
                        {ok, Mapped} ->
                            case lists:member(Mapped, Acc) of
                                true ->
                                    Acc;
                                false ->
                                    [ Mapped | Acc ]
                            end;
                        error ->
                            Acc
                    end
                end,
                [],
                AcceptList),
            lists:reverse(Accepted);
        {error, _} ->
            []
    end.

unpack_lang_cookie(undefined) ->
    [];
unpack_lang_cookie(Value) ->
    Split = binary:split(Value, <<":">>, [global, trim_all]),
    lists:filtermap(
        fun(Lang) ->
            case z_language:to_language_atom(Lang) of
                {ok, Code} -> {true, Code};
                {error, _} -> false
            end
        end,
        Split).

-spec get_q_language(Context) -> Language | undefined when
    Context :: z:context(),
    Language :: atom().
get_q_language(Context) ->
    case z_context:get_q_all(<<"z_language">>, Context) of
        [] ->
            undefined;
        L ->
            Lang = lists:last(L),
            case z_language:to_language_atom(Lang) of
                {ok, Code} ->
                    case z_language:is_language_enabled(Code, Context) of
                        true -> Code;
                        false -> get_q_language_1(Lang, Context)
                    end;
                {error, _} ->
                    get_q_language_1(Lang, Context)
            end
    end.

get_q_language_1(<<A, B, $-, _/binary>> = Lang, Context) ->
    Acceptable = z_language:acceptable_languages_map(Context),
    case maps:get(Lang, Acceptable, undefined) of
        undefined ->
            case maps:get(<<A,B>>, Acceptable, undefined) of
                undefined ->
                    undefined;
                Code ->
                    binary_to_atom(Code, utf8)
            end;
        Code ->
            binary_to_atom(Code, utf8)
    end;
get_q_language_1(Lang, Context) ->
    Acceptable = z_language:acceptable_languages_map(Context),
    case maps:get(Lang, Acceptable, undefined) of
        undefined ->
            undefined;
        Code ->
            binary_to_atom(Code, utf8)
    end.

observe_user_context(#user_context{ id = UserId }, Context, _Context) ->
    case m_rsc:p_no_acl(UserId, pref_language, Context) of
        Code when Code /= undefined ->
            set_language(Code, Context);
        _ ->
            Context
    end.

observe_set_user_language(#set_user_language{ id = UserId }, Context, _Context) when is_integer(UserId) ->
    case m_rsc:p_no_acl(UserId, pref_language, Context) of
        Code when Code /= undefined -> set_language(Code, Context);
        _ -> Context
    end;
observe_set_user_language(#set_user_language{}, Context, _Context) ->
    Context.


observe_url_rewrite(#url_rewrite{}, Url, #context{language=[_,'x-default']}) ->
    Url;
observe_url_rewrite(#url_rewrite{}, <<"?", _/binary>> = Url, _Context) ->
    Url;
observe_url_rewrite(#url_rewrite{}, <<"#", _/binary>> = Url, _Context) ->
    Url;
observe_url_rewrite(#url_rewrite{args=Args}, Url, Context) ->
    case z_context:language(Context) of
        undefined ->
            Url;
        'x-default' ->
            Url;
        Language ->
            case lists:keyfind(z_language, 1, Args) of
                false ->
                    RewriteUrl = m_config:get_boolean(?MODULE, rewrite_url, true, Context),
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
                true ->
                    Dispatch;
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
    Languages = [
        [ $", z_convert:to_binary(Lang), $" ] || Lang <- z_context:languages(Context)
    ],
    Languages1 = lists:join($,, Languages),
    [
        <<"z_language=\"", Language/binary, "\"">>, $;,
        <<"z_languages=[">>, Languages1, <<"];">>
    ].


%% @doc Set the current session (and user) language, reload the user agent's page. Called from
%% language switch. Reloads the page to reflect the new setting.
event(#postback{message={set_language, Args}}, Context) ->
    LanguageCode = case proplists:get_value(code, Args) of
               undefined -> z_context:get_q(<<"triggervalue">>, Context);
        ArgCode -> ArgCode
    end,
    Context1 = set_user_language(LanguageCode, Context),
    case m_rsc:rid( proplists:get_value(id, Args), Context1 ) of
        undefined -> reload_page(Context1);
        RscId -> z_render:wire({redirect, [ {id, RscId} ]}, Context1)
    end;

%% @doc Save the language list, as edited in the admin.
event(#submit{ message={language_list, _Args} }, Context) ->
    case z_acl:is_allowed(use, ?MODULE, Context) of
        true ->
            LanguageList = lists:filtermap(
                fun
                    ({<<"status-", Code/binary>>, Status}) ->
                        case z_language:to_language_atom(Code) of
                            {ok, CodeAtom} ->
                                StatusAtom = case Status of
                                    <<"editable">> -> editable;
                                    <<"disabled">> -> false;
                                    <<"enabled">> -> true
                                end,
                                {true, {CodeAtom, StatusAtom}};
                            {error, _} ->
                                false
                        end;
                    (_) ->
                        false
                end,
                z_context:get_q_all_noz(Context)),
            z_language:set_language_config(LanguageList, Context),
            reload_table(Context);
        false ->
            z_render:growl_error(?__(<<"Sorry, you don't have permission to change the language list.">>, Context), Context)
    end;

%% @doc Set the default language. Reloads the page to reflect the new setting.
event(#postback{message={language_default, Args}}, Context) ->
    case z_acl:is_allowed(use, ?MODULE, Context) of
        true ->
            {code, LanguageCode} = proplists:lookup(code, Args),
            case language_status(LanguageCode, true, Context) of
                ok ->
                    Context1 = set_default_language(LanguageCode, Context),
                    reload_table(Context1);
                {error, _} ->
                    z_render:growl_error(?__(<<"Sorry, could not change the language.">>, Context), Context)
            end;
        false ->
            z_render:growl_error(?__(<<"Sorry, you don't have permission to set the default language.">>, Context), Context)
    end;

%% @doc Add a language to the config.
event(#submit{message={language_add, _Args}}, Context) ->
    case z_acl:is_allowed(use, ?MODULE, Context) of
        true ->
            language_add(
                    z_string:trim(z_context:get_q(<<"code">>, Context)),
                    z_convert:to_bool(z_context:get_q(<<"is_enabled">>, Context)),
                    Context),
            reload_table(Context);
        false ->
            z_render:growl_error(?__(<<"Sorry, you don't have permission to change the language list.">>, Context), Context)
    end;

%% @doc Removes the language from the config. Reloads the page to reflect the updated list.
event(#postback{message={language_delete, Args}}, Context) ->
    case z_acl:is_allowed(use, ?MODULE, Context) of
        true ->
            {code, LanguageCode} = proplists:lookup(code, Args),
            Context1 = language_delete(LanguageCode, Context),
            Context2 = reload_table(Context1),
            z_render:dialog_close(Context2);
        false ->
            z_render:growl_error(?__(<<"Sorry, you don't have permission to change the language list.">>, Context), Context)
    end;

%% @doc Toggles the enabled/editable/default state of a language.
event(#postback{message={language_status, Args}}, Context) ->
    case z_acl:is_allowed(use, ?MODULE, Context) of
        true ->
            {code, Code} = proplists:lookup(code, Args),
            Result = case z_convert:to_binary(z_context:get_q("triggervalue", Context)) of
                <<"enabled">> ->
                    language_status(Code, true, Context);
                <<"editable">> ->
                    language_status(Code, editable, Context);
                <<"disabled">> ->
                    language_status(Code, false, Context)
            end,
            case Result of
                ok ->
                    reload_table(Context);
                {error, _} ->
                    z_render:growl_error(?__(<<"Sorry, could not change the language.">>, Context), Context)
            end;
        false ->
            z_render:growl_error(?__("Sorry, you don't have permission to change the language list.", Context), Context)
    end;

%% @doc Toggles the state of the 'rewrite URL' setting. Reloads the page to reflect the new setting.
event(#postback{message={toggle_url_rewrite, _Args}}, Context) ->
    Value = z_convert:to_bool(z_context:get_q(<<"triggervalue">>, Context)),
    set_language_url_rewrite(Value, Context),
    reload_page(Context);

%% @doc Start rescanning all templates for translation tags.
event(#postback{message={translation_generate, _Args}}, Context) ->
    case z_acl:is_allowed(use, ?MODULE, Context)
        andalso (      z_acl:is_admin(Context)
                orelse z_acl:is_allowed(use, mod_development, Context))
    of
        true ->
            case gettext_installed() of
                true ->
                    spawn(fun() -> generate(Context) end),
                    z_render:growl(?__(<<"Started building the .pot files. This may take a while...">>, Context), Context);
                false ->
                    ?LOG_ERROR(#{
                        text => <<
                            "Cannot generate translation files because gettext is not installed. "
                            "See http://docs.zotonic.com/en/latest/developer-guide/translation.html."
                            >>,
                        in => zotonic_mod_translation,
                        result => error,
                        reason => gettext
                    }),
                    z_render:growl_error(?__(
                        <<
                            "Cannot generate translation files because "
                            "<a href=\"http://docs.zotonic.com/en/latest/developer-guide/translation.html\">gettext "
                            "is not installed</a>."
                        >>, Context),
                        Context)
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
%%      Any hostname is stripped, and only the path is returned.
-spec url_strip_language(binary()) -> binary().
url_strip_language(<<"/", Path/binary>> = Url) ->
    case binary:split(Path, <<"/">>) of
        [ MaybeLang, Rest ] ->
            case z_language:is_valid(MaybeLang) of
                true -> <<"/", Rest/binary>>;
                false -> Url
            end;
        _ ->
            Url
    end;
url_strip_language(<<"https://", _/binary>> = Path) ->
    url_strip_language(url_path(Path));
url_strip_language(<<"http://", _/binary>> = Path) ->
    url_strip_language(url_path(Path));
url_strip_language(<<"//", _/binary>> = Path) ->
    url_strip_language(url_path(Path));
url_strip_language(Path) when is_binary(Path) ->
    Path.

url_path(Url) ->
    case uri_string:parse(Url) of
        #{ path := Path } -> Path;
        _ -> <<"/">>
    end.


%% @doc Set the language, as selected by the user. Persist this choice.
-spec set_user_language(atom(), z:context()) -> z:context().
set_user_language(Code, Context) ->
    Context1 = set_language(Code, Context),
    case z_acl:user(Context1) of
        undefined ->
            nop;
        UserId ->
            NewCode = z_context:language(Context1),
            case m_rsc:p_no_acl(UserId, pref_language, Context1) of
                NewCode ->
                    nop;
                _ ->
                    catch m_rsc:update(
                            UserId,
                            [ {pref_language, NewCode} ],
                            [ no_touch ],
                            Context1)
            end
    end,
    Context1.

%% @doc Event handler to set the default language. Ignores non-enabled languages.
-spec set_default_language(LanguageCode, Context) -> NewContext when
    LanguageCode :: z_language:language_code(),
    Context :: z:context(),
    NewContext :: z:context().
set_default_language(Code, Context) ->
    case z_acl:is_allowed(use, ?MODULE, Context) of
        true ->
            case language_status(Code, true, Context) of
                ok ->
                    CodeB = z_convert:to_binary(Code),
                    case m_config:get_value(i18n, language, Context) of
                        CodeB -> ok;
                        _ -> m_config:set_value(i18n, language, Code, Context)
                    end,
                    Context;
                {error, _} ->
                    z_render:growl_error(?__(<<"Sorry, that language is unknown or not enabled.">>, Context), Context)
            end;
        false ->
            z_render:growl_error(?__(<<"Sorry, you don't have permission to set the default language.">>, Context), Context)
    end.


%% @doc Returns a valid language from the config language. If the language is not
%%      available or not enabled, tries the language's fallback language (retrieve from
%%      z_language); if this fails too, returns the data for the site's default language.
-spec valid_config_language(atom() | undefined, z:context()) -> z_language:language_code() | undefined.
valid_config_language(Code, Context) ->
    valid_config_language(Code, Context, [Code]).

valid_config_language(undefined, Context, Tries) ->
    Default = z_language:default_language(Context),
    valid_config_language(Default, Context, [ Default, undefined | Tries ]);
valid_config_language(Code, Context, Tries) ->
    EnabledLanguages = z_language:enabled_languages(Context),
    case proplists:get_value(Code, EnabledLanguages, false) of
        false ->
            % Language code is not listed in config, let's try a fallback
            Fallback = z_language:fallback_language(Code, Context),
            % Bail out if we got into a loop
            case lists:member(Fallback, Tries) of
                true -> undefined;
                false -> valid_config_language(Fallback, Context, [ Fallback | Tries ])
            end;
        true ->
            % Language is listed and enabled
            Code
    end.

%% @doc Set the enabled/editable status of a language. Returns an error if the
%% language is unknown or the default language is being disabled.
-spec language_status(Code, Status, Context) -> ok | {error, nolang|default} when
    Code :: z_language:language_code(),
    Status :: z_language:language_status(),
    Context :: z:context().
language_status(Code, Status, Context) when is_atom(Code), is_atom(Status) ->
    case z_language:default_language(Context) of
        Code when Status =/= true ->
            % Can't disable the default language
            {error, default};
        Code ->
            ok;
        _ ->
            case z_language:is_valid(Code) of
                true ->
                    CL = z_language:language_config(Context),
                    CL1 = proplists:delete(Code, CL),
                    CL2 = case Status of
                        true -> [ {Code, true} | CL1 ];
                        false -> [ {Code, false} | CL1 ];
                        editable -> [ {Code, editable} | CL1 ]
                    end,
                    z_language:set_language_config(CL2, Context);
                false ->
                    {error, nolang}
            end
    end.


%% @doc Add a language to the i18n configuration
-spec language_add(Language, Status, Context) -> ok | {error, not_a_language} when
    Language :: z_language:language(),
    Status :: z_language:language_status(),
    Context :: z:context().
language_add(Language, Status, Context) when is_boolean(Status); Status =:= editable ->
    case z_language:to_language_atom(Language) of
        {ok, NewCode} ->
            ConfigLanguages = z_language:language_config(Context),
            ConfigLanguages1 = lists:map(
                fun
                    ({Code, _}) when Code =:= NewCode -> {Code, Status};
                    (Other) -> Other
                end,
                ConfigLanguages),
            ConfigLanguages2 = case lists:keymember(NewCode, 1, ConfigLanguages1) of
                true ->
                    ConfigLanguages1;
                false ->
                    ConfigLanguages1 ++ [ {NewCode, Status} ]
            end,
            z_language:set_language_config(ConfigLanguages2, Context),
            ok;
        {error, _} ->
            ?LOG_WARNING(#{
                text => <<"mod_translation error. language_add: language does not exist">>,
                in => zotonic_mod_translation,
                result => error,
                reason => not_a_language,
                language => Language
            }),
            {error, not_a_language}
    end.


%% @doc Remove a language from the i18n configuration
-spec language_delete(z_language:language_code(), z:context()) -> z:context().
language_delete(LanguageCode, Context) when is_atom(LanguageCode) ->
    DeletesCurrentLanguage = z_context:language(Context) =:= LanguageCode,
    remove_from_config(LanguageCode, Context),
    case DeletesCurrentLanguage of
        true ->
            Fallback = z_language:fallback_language(LanguageCode, Context),
            set_language(Fallback, Context);
        false ->
            Context
    end.

%% @doc Remove a language from the i18n configuration
-spec remove_from_config(z_language:language_code(), z:context()) -> ok.
remove_from_config(LanguageCode, Context) ->
    ConfigLanguages = z_language:language_config(Context),
    ConfigLanguages1 = proplists:delete(LanguageCode, ConfigLanguages),
    z_language:set_language_config(ConfigLanguages1, Context).


%% @doc Set/reset the state of the 'rewrite URL' setting.
-spec set_language_url_rewrite(boolean(), z:context()) -> z:context().
set_language_url_rewrite(Value, Context) ->
    m_config:set_value(mod_translation, rewrite_url, Value, Context),
    reload_page(Context).

%% @doc Reloads the page via javascript (zotonic-wired.js).
-spec reload_page(z:context()) -> z:context().
reload_page(Context) ->
   RewriteUrl = z_convert:to_bool(m_config:get_value(?MODULE, rewrite_url, true, Context)),
   Language = z_context:language(Context),
   z_render:wire({reload, [{z_language, Language}, {z_rewrite_url, RewriteUrl}]}, Context).

%% @doc Reloads the table with translations.
reload_table(Context) ->
    z_render:update(
        "translation-language-status",
        #render{ template = "_translation_language_status.tpl" },
        Context).

%% @private
is_multiple_languages_config(Context) ->
    length(z_language:enabled_languages(Context)) > 1.


%% @private
-spec is_enabled_language(binary() | atom(), z:context()) -> boolean().
is_enabled_language(LanguageCode, Context) ->
    case maybe_language_code(LanguageCode) of
        true ->
            Enabled = z_language:enabled_languages(Context),
            try
                lists:member(z_convert:to_atom(LanguageCode), Enabled)
            catch
                error:badarg -> false
            end;
        false ->
            false
    end.

maybe_language_code(<<A,B>> = Code) when A >= $a, A =< $z, B >= $a, B =< $z ->
    z_language:is_valid(Code);
maybe_language_code(<<A,B,$-,_/binary>> = Code) when A >= $a, A =< $z, B >= $a, B =< $z ->
    z_language:is_valid(Code);
maybe_language_code(<<A,B,C>> = Code) when A >= $a, A =< $z, B >= $a, B =< $z, C >= $a, C =< $z ->
    z_language:is_valid(Code);
maybe_language_code(<<$x,$-,_/binary>> = Code) ->
    % x-default, x-klingon, etc.
    z_language:is_valid(Code);
maybe_language_code(Code) when is_atom(Code) ->
    maybe_language_code( atom_to_binary(Code, utf8) );
maybe_language_code(_) ->
    false.


% @doc Generate all .po templates for the given site
generate(Host) when is_atom(Host) ->
    generate(z_context:new(Host));
generate(Context) ->
    ActiveModules = lists:foldl(
        fun({App, _}, Acc) ->
            lists:keydelete(core_app_to_module_name(App), 1, Acc)
        end,
        z_module_manager:active_dir(Context),
        core_apps()),
    Result = translation_po:generate(translation_scan:scan(ActiveModules)),
    _ = generate_core(),
    Result.

%% @doc Generate consolidated translation file zotonic.pot for all core modules.
%%      Both active and inactive modules are indexed, so the generated
%%      translation files are always complete.
-spec generate_core() -> ok | {error, needs_core_zotonic}.
generate_core() ->
    case zotonic_core:is_zotonic_project() of
        true ->
            ?LOG_NOTICE(#{
                in => zotonic_mod_translation,
                text => <<"Generating .pot files...">>
            }),
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
    ZotonicPot = filename:join([ code:priv_dir(zotonic_core), "translations", "zotonic.pot" ]),
    PotFiles = filename:join([
        z_path:get_path(), "apps",
        "zotonic_*", "priv", "translations", "template", "*.pot"
    ]),
    ?LOG_NOTICE(#{
        in => zotonic_mod_translation,
        text => <<"Merging .pot files">>,
        path => ZotonicPot
    }),
    Command = lists:flatten([
        "msgcat -o ",
        z_filelib:os_filename(ZotonicPot),
        lists:map(
            fun(F) ->
                [ " ", z_filelib:os_filename(F) ]
            end,
            filelib:wildcard(PotFiles))
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

observe_language_detect(#language_detect{ text = Text, is_editable_only = true }, Context) ->
    case translation_detect:detect(Text, Context) of
        {ok, Lang} ->
            Lang;
        {error, _} ->
            undefined
    end;
observe_language_detect(#language_detect{ text = Text, is_editable_only = false }, _Context) ->
    case translation_detect:detect(Text) of
        {ok, Lang} ->
            Lang;
        {error, _} ->
            undefined
    end.


%% @doc Are the gettext tools available?
-spec gettext_installed() -> boolean().
gettext_installed() ->
    os:find_executable("msgcat") =/= false andalso os:find_executable("msgmerge") =/= false.
