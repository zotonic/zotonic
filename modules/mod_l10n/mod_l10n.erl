%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011-2014 Marc Worrell
%% @doc Localization of Zotonic.  Country, timezone, and other lookups.

%% Copyright 2011-2014 Marc Worrell
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

-module(mod_l10n).

-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Localization").
-mod_description("Localization, timezones, translations for country names etc.").

-export([
    observe_session_init_fold/3,
    observe_session_context/3,
    observe_auth_logon/3,
    observe_user_context/3,
    observe_pivot_rsc_data/3,
    observe_rsc_update_done/2,
    observe_admin_menu/3,

    set_user_timezone/2
]).

-include("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").


%% @doc Check if the user has a prefered timezone (in the user's persistent data).
observe_session_init_fold(session_init_fold, Context, _Context) ->
    case is_fixed_timezone(Context) of
        true ->
            Context;
        false ->
            case get_q_timezone(Context) of
                undefined ->
                    case z_context:get_persistent(tz, Context) of
                        undefined -> Context;
                        Tz -> set_timezone(Tz, Context)
                    end;
                QTz ->
                    try_set_timezone(QTz, Context)
            end
    end.

get_q_timezone(Context) ->
    case z_context:get_q_all("z_timezone", Context) of
        [] -> undefined;
        L -> z_convert:to_binary(lists:last(L))
    end.


observe_session_context(session_context, Context, _Context) ->
    case is_fixed_timezone(Context) of
        true ->
            Context;
        false ->
            Context1 = case z_context:get_session(tz, Context) of
                            undefined -> Context;
                            Tz -> z_context:set_tz(Tz, Context)
                       end,
            case get_q_timezone(Context1) of
                undefined -> Context1;
                QTz -> try_set_timezone(QTz, Context1)
            end
    end.

observe_auth_logon(auth_logon, Context, _Context) ->
    case is_fixed_timezone(Context) of
        true ->
            Context;
        false ->
            UserId = z_acl:user(Context),
            case m_rsc:p_no_acl(UserId, pref_tz, Context) of
                Tz when is_binary(Tz), Tz =/= <<>> ->
                    % Switch the session to the default timezone of the user
                    Context1 = try_set_timezone(Tz, Context),
                    z_context:set_persistent(tz, z_context:tz(Context1), Context1),
                    Context1;
                _Undefined ->
                    % Ensure that the user has a default timezone
                    catch m_rsc:update(UserId, [{pref_tz, z_context:tz(Context)}], Context),
                    Context
            end
    end.

observe_user_context(#user_context{id=UserId}, Context, _Context) ->
    case is_fixed_timezone(Context) of
        true ->
            Context;
        false ->
            case m_rsc:p_no_acl(UserId, pref_tz, Context) of
                Tz when is_binary(Tz), Tz =/= <<>> ->
                    z_context:set_tz(Tz, Context);
                _Undefined ->
                    Context
            end
    end.

observe_rsc_update_done(#rsc_update_done{id=Id, pre_props=Pre, post_props=Post}, Context) ->
    case is_fixed_timezone(Context) of
        true ->
            ok;
        false ->
            case z_acl:user(Context) of
                Id ->
                    PreTz = z_convert:to_binary(proplists:get_value(pref_tz, Pre)), 
                    PostTz = z_convert:to_binary(proplists:get_value(pref_tz, Post)),
                    case PostTz of
                        PreTz ->
                            ok;
                        <<>> ->
                            ok;
                        _NewTz ->
                            z_context:set_session(tz, PostTz, Context),
                            z_context:set_persistent(tz, PostTz, Context)
                    end;
                _Other ->
                    ok
            end
    end.


is_fixed_timezone(Context) ->
    z_convert:to_bool(m_config:get_value(mod_l10n, timezone_is_fixed, Context)).

%% @doc Set the timezone, as selected by the user. Persist this choice.
set_user_timezone(Tz, Context) ->
    Context1 = try_set_timezone(Tz, Context),
    z_context:set_persistent(tz, z_context:tz(Context1), Context1),
    case z_acl:user(Context1) of
        undefined -> 
            nop;
        UserId ->
            case m_rsc:p_no_acl(UserId, pref_tz, Context1) of
                Tz -> nop;
                _ -> catch m_rsc:update(UserId, [{pref_tz, z_context:tz(Context1)}], Context1)
            end
    end,
    Context1.


%% @doc Set the timezone of the user. Only done when the found timezone is a known timezone.
try_set_timezone(Tz, Context) ->
    case localtime:tz_name({{2008,12,10},{15,30,0}}, z_convert:to_list(Tz)) of
        {error, _} ->
            lager:warning("~p: Unknown timezone ~p", [z_context:site(Context), Tz]),
            Context;
        Tz when is_list(Tz) ->
            set_timezone(Tz, Context);
        {Tz1, Tz2} when is_list(Tz1), is_list(Tz2) ->
            set_timezone(Tz, Context)
    end.


%% @doc Set the timezone of the current context/session
set_timezone(Tz, Context) ->
    case z_context:tz(Context) of
        Tz -> 
            Context;
        _ ->
            Context1 = z_context:set_tz(Tz, Context),
            z_context:set_session(tz, Tz, Context1),
            Context1
    end.


observe_admin_menu(admin_menu, Acc, Context) ->
    [
     #menu_item{id=admin_l10n,
                parent=admin_modules,
                label=?__("Localization", Context),
                url={admin_l10n},
                visiblecheck={acl, use, mod_config}}
     
     |Acc].

%% @doc Expand the two letter iso code country depending on the languages in the resource.
observe_pivot_rsc_data(pivot_rsc_data, Rsc, Context) ->
    Languages = lists:usort([ en, default_language(Context) | resource_languages(Rsc) ]),
    Rsc2 = expand_country(address_country, Rsc, Languages, Context),
    expand_country(mail_country, Rsc2, Languages, Context).

expand_country(Prop, Rsc, Languages, Context) ->
	Rsc1 = map_country(Prop, Rsc),
    case proplists:get_value(Prop, Rsc1) of
        <<>> -> Rsc1;
        undefined -> Rsc1;
        <<_,_>> = Iso ->
            Countries = lists:map(
                            fun(Lang) -> 
                                m_l10n:country_name(Iso, Lang, Context)
                            end,
                            Languages),
            add_alias(Iso, Rsc1 ++ [ {extra_pivot_data, C} || C <- lists:usort(Countries)]);
        _Other -> Rsc1
    end.

    add_alias(<<"us">>, R) ->
        [{extra_pivot_data, <<"USA">>} | R];
    add_alias(<<"uk">>, R) ->
        [{extra_pivot_data, <<"England">>} | R];
    add_alias(<<"nl">>, R) ->
        [{extra_pivot_data, <<"Holland">>} | R];
    add_alias(<<"be">>, R) ->
        [{extra_pivot_data, <<"België">>}, {extra_pivot_data, "Belgique"} | R];
    add_alias(_, R) ->
        R.

%% @doc Map a country name to its iso code.  This very crude and should be more comprehensive.
map_country(Prop, Rsc) ->
    case proplists:get_value(Prop, Rsc) of
        <<>> -> Rsc;
        undefined -> Rsc;
        <<A,B>> when A =< $Z orelse B =< $Z ->
            [{Prop, z_convert:to_binary(z_string:to_lower([A,B]))} | Rsc];
        Country ->
            case z_string:to_lower(Country) of
                "usa" -> [{Prop, <<"us">>} | Rsc];
                "holland" -> [{Prop, <<"nl">>} | Rsc];
                "nederland" -> [{Prop, <<"nl">>} | Rsc];
                "netherlands" -> [{Prop, <<"nl">>} | Rsc];
                "the netherlands" -> [{Prop, <<"nl">>} | Rsc];
                "belgië" -> [{Prop, <<"be">>} | Rsc];
                "belgie" -> [{Prop, <<"be">>} | Rsc];
                "belgique" -> [{Prop, <<"be">>} | Rsc];
                % typos
                "nethlerlands" -> [{Prop, <<"nl">>} | Rsc];
                % just keep as-is
                _ -> Rsc
            end
    end.


default_language(Context) ->
    case m_config:get_value(i18n, language, Context) of
        undefined -> en;
        <<>> -> en;
        Lang -> z_convert:to_atom(Lang)
    end.

resource_languages(Rsc) ->
    case proplists:get_value(language, Rsc) of
        undefined -> [];
        <<>> -> [];
        Langs -> [ z_convert:to_atom(Lang) || Lang <- Langs, Lang /= <<>> ]
    end.
