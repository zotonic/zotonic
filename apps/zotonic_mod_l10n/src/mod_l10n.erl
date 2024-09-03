% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%% coding: utf-8

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
    % observe_auth_logon/3,
    observe_request_context/3,
    observe_user_context/3,
    observe_pivot_rsc_data/3,
    % observe_rsc_update_done/2,
    observe_admin_menu/3,

    set_user_timezone/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").


-define(TZ_COOKIE, <<"z.tz">>).
-define(TZ_COOKIE_MAX_AGE, 3600*24*365).


%% @doc Check if the user has a preferred timezone
observe_request_context(#request_context{ phase = init }, Context, _Context) ->
    case is_fixed_timezone(Context) of
        true ->
            Context;
        false ->
            maybe_user(Context)
    end;
observe_request_context(#request_context{ phase = auth_status, document = #{ <<"timezone">> := TzData } }, Context, _Context) ->
    case is_fixed_timezone(Context) of
        true ->
            Context;
        false ->
            Tz = maps:get(<<"user_agent">>, TzData, <<>>),

            % Check the user-agent timezone against the user's or the cookie's timezone
            % Optionally initialize the user's timezone (if not set)
            case z_acl:user(Context) of
                undefined ->
                    Context1 = try_set_timezone(Tz, Context),
                    maybe_set_cookie(Context1);
                UserId ->
                    case m_rsc:p_no_acl(UserId, <<"pref_tz">>, Context) of
                        None when None =:= undefined; None =:= <<>> ->
                            Context1 = try_set_timezone(Tz, Context),
                            _ = m_rsc:update(
                                    UserId,
                                    #{ <<"pref_tz">> => z_context:tz(Context1) },
                                    [ no_touch ],
                                    Context1),
                            maybe_set_cookie(Context1);
                        UserTz ->
                            Context1 = try_set_timezone(UserTz, Context),
                            maybe_set_cookie(Context1)
                    end
            end
    end;
observe_request_context(#request_context{}, Context, _Context) ->
    Context.

% maybe_q_timezone(Context) ->
%     case z_context:get_q_all(<<"z_timezone">>, Context) of
%         [] -> maybe_user(Context);
%         L -> try_set_timezone(lists:last(L), Context)
%     end.

maybe_user(Context) ->
    case z_acl:user(Context) of
        undefined ->
            maybe_cookie(Context);
        UserId ->
            case m_rsc:p_no_acl(UserId, <<"pref_tz">>, Context) of
                undefined ->
                    maybe_cookie(Context);
                Tz ->
                    maybe_set_cookie(try_set_timezone(Tz, Context))
            end
    end.

maybe_cookie(Context) ->
    case z_context:get_cookie(?TZ_COOKIE, Context) of
        undefined ->
            Context;
        Tz ->
            try_set_timezone(Tz, Context)
    end.


maybe_set_cookie(Context) ->
    Tz = z_context:tz(Context),
    case z_context:get_cookie(?TZ_COOKIE, Context) of
        Tz -> Context;
        _ -> set_cookie(Tz, Context)
    end.

set_cookie(Tz, Context) ->
    z_context:set_cookie(
        ?TZ_COOKIE,
        Tz,
        [
            {max_age, ?TZ_COOKIE_MAX_AGE},
            {path, <<"/">>},
            {secure, true},
            {same_site, lax}
        ],
        Context).

observe_user_context(#user_context{ id = UserId }, Context, _Context) ->
    case is_fixed_timezone(Context) of
        true ->
            Context;
        false ->
            case m_rsc:p_no_acl(UserId, <<"pref_tz">>, Context) of
                Tz when is_binary(Tz), Tz =/= <<>> ->
                    z_context:set_tz(Tz, Context);
                _Undefined ->
                    Context
            end
    end.

% observe_rsc_update_done(#rsc_update_done{id=Id, pre_props=Pre, post_props=Post}, Context) ->
%     case is_fixed_timezone(Context) of
%         true ->
%             ok;
%         false ->
%             case z_acl:user(Context) of
%                 Id ->
%                     PreTz = z_convert:to_binary(proplists:get_value(pref_tz, Pre)),
%                     PostTz = z_convert:to_binary(proplists:get_value(pref_tz, Post)),
%                     case PostTz of
%                         PreTz ->
%                             ok;
%                         <<>> ->
%                             ok;
%                         _NewTz ->
%                             % TODO: push new prefs to all clients (publish on user topic)
%                             ok
%                     end;
%                 _Other ->
%                     ok
%             end
%     end.

is_fixed_timezone(Context) ->
    z_convert:to_bool(m_config:get_value(mod_l10n, timezone_is_fixed, Context)).

%% @doc Set the timezone, as selected by the user. Persist this choice.
set_user_timezone(Tz, Context) ->
    Context1 = try_set_timezone(Tz, Context),
    case z_acl:user(Context1) of
        undefined ->
            nop;
        UserId ->
            case m_rsc:p_no_acl(UserId, <<"pref_tz">>, Context1) of
                Tz -> nop;
                _ -> catch m_rsc:update(UserId, #{ <<"pref_tz">> => z_context:tz(Context1) }, Context1)
            end
    end,
    Context1.


%% @doc Set the timezone of the user. Only done when the found timezone is a known timezone.
try_set_timezone(<<>>, Context) ->
    Context;
try_set_timezone(Tz, Context) ->
    case m_l10n:is_timezone(Tz) of
        true -> set_timezone(Tz, Context);
        false -> Context
    end.

%% @doc Set the timezone of the current context
set_timezone(Tz, Context) ->
    case z_context:tz(Context) of
        Tz -> Context;
        _ -> z_context:set_tz(Tz, Context)
    end.


observe_admin_menu(#admin_menu{}, Acc, Context) ->
    [
     #menu_item{id=admin_l10n,
                parent=admin_modules,
                label=?__("Localization", Context),
                url={admin_l10n},
                visiblecheck={acl, use, mod_config}}

     |Acc].

%% @doc Expand the two letter iso code country depending on the languages in the resource.
observe_pivot_rsc_data(#pivot_rsc_data{}, Rsc, Context) ->
    Languages = lists:usort([ en, default_language(Context) | resource_languages(Rsc) ]),
    Rsc2 = expand_country(<<"address_country">>, Rsc, Languages, Context),
    expand_country(<<"mail_country">>, Rsc2, Languages, Context).

expand_country(Prop, Rsc, Languages, Context) ->
	Rsc1 = map_country(Prop, Rsc),
    case maps:get(Prop, Rsc1, undefined) of
        <<>> ->
            Rsc1;
        undefined ->
            Rsc1;
        <<_,_>> = Iso ->
            Countries = lists:map(
                            fun(Lang) ->
                                m_l10n:country_name(Iso, Lang, Context)
                            end,
                            Languages),
            Rsc2 = lists:foldl(
                fun(C, Acc) ->
                    extra_pivot_data(C, Acc)
                end,
                Rsc1,
                lists:usort(Countries)),
            add_alias(Iso, Rsc2);
        _Other ->
            Rsc1
    end.

add_alias(<<"us">>, R) -> extra_pivot_data(<<"USA">>, R);
add_alias(<<"uk">>, R) -> extra_pivot_data(<<"England">>, R);
add_alias(<<"nl">>, R) -> extra_pivot_data(<<"Holland">>, R);
add_alias(<<"be">>, R) -> extra_pivot_data(<<"België Belgique"/utf8>>, R);
add_alias(_, R) -> R.

extra_pivot_data(Text, #{ <<"extra_pivot_data">> := Extra } = R) when is_binary(Extra) ->
    R#{
        <<"extra_pivot_data">> => <<Extra/binary, " ", Text/binary>>
    };
extra_pivot_data(Text, R) ->
    R#{
        <<"extra_pivot_data">> => Text
    }.

%% @doc Map a country name to its iso code. This is very crude and should be more comprehensive.
map_country(Prop, Rsc) ->
    case maps:get(Prop, Rsc, undefined) of
        undefined -> Rsc;
        <<>> -> Rsc;
        <<A,B>> when A =< $Z orelse B =< $Z ->
            Rsc#{ Prop => z_string:to_lower(<<A,B>>) };
        Country ->
            Country1 = value(Country),
            case z_string:to_lower(Country1) of
                <<"usa">> ->                Rsc#{ Prop => <<"us">>};
                <<"holland">> ->            Rsc#{ Prop => <<"nl">>};
                <<"nederland">> ->          Rsc#{ Prop => <<"nl">>};
                <<"netherlands">> ->        Rsc#{ Prop => <<"nl">>};
                <<"the netherlands">> ->    Rsc#{ Prop => <<"nl">>};
                <<"netherlands, the">> ->   Rsc#{ Prop => <<"nl">>};
                <<"belgië"/utf8>> ->        Rsc#{ Prop => <<"be">>};
                <<"belgie">> ->             Rsc#{ Prop => <<"be">>};
                <<"belgique">> ->           Rsc#{ Prop => <<"be">>};
                % typos
                <<"nehterlands">> ->        Rsc#{ Prop => <<"nl">>};
                % Try country tables
                _ ->
                    case l10n_country2iso:country2iso(Country1) of
                        undefined -> Rsc;
                        Iso -> Rsc#{ Prop => Iso}
                    end
            end
    end.

value(#trans{ tr = Tr }) ->
    case proplists:get_value(en, Tr) of
        undefined when Tr =:= [] -> <<>>;
        undefined -> {_, T} = hd(Tr), T;
        T -> T
    end;
value(V) when is_list(V) ->
    z_convert:to_binary(V);
value(V) when is_binary(V) ->
    V;
value(_) ->
    <<>>.

default_language(Context) ->
    case m_config:get_value(i18n, language, Context) of
        undefined -> en;
        <<>> -> en;
        Lang -> z_convert:to_atom(Lang)
    end.

resource_languages(Rsc) ->
    case maps:get(<<"language">>, Rsc, undefined) of
        undefined -> [];
        <<>> -> [];
        Atom when is_atom(Atom) -> [ Atom ];
        Langs -> [ z_convert:to_atom(Lang) || Lang <- Langs, Lang /= <<>> ]
    end.
