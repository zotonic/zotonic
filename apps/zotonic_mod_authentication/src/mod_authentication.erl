%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2014 Marc Worrell
%% @doc Authentication and identification of users.

%% Copyright 2010-2014 Marc Worrell
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

-module(mod_authentication).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Authentication").
-mod_description("Handles authentication and identification of users.").
-mod_prio(500).
-mod_depends([base, acl]).
-mod_provides([authentication]).

%% gen_server exports
-export([
    init/1,
    event/2,
    observe_logon_submit/2,
    observe_auth_autologon/2,
    observe_auth_validated/2,

    observe_admin_menu/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").


init(Context) ->
    % Ensure password_min_length config
    case m_config:get(?MODULE, password_min_length, Context) of
        undefined -> m_config:set_value(?MODULE, password_min_length, "6", Context);
        _ -> nop
    end,
    ok.


%% @doc Handle logon submits in case we cannot use controller_logon. Pass on the  data to the page controller.
event(#submit{message={logon, WireArgs}}, Context) ->
    Args = z_context:get_q_all(Context),
    controller_logon:logon(Args, WireArgs, Context);
event(#submit{message={reminder, _Args}}, Context) ->
    Args = z_context:get_q_all(Context),
    controller_logon:reminder(Args, Context);
event(#submit{message={expired, _Args}}, Context) ->
    Args = z_context:get_q_all(Context),
    controller_logon:expired(Args, Context);
event(#submit{message={reset, _Args}}, Context) ->
    lager:info("reset"),
    Args = z_context:get_q_all(Context),
    controller_logon:reset(Args, Context).

observe_admin_menu(#admin_menu{}, Acc, Context) ->
    [
     #menu_item{id=admin_authentication_services,
                parent=admin_auth,
                label=?__("External Services", Context),
                url={admin_authentication_services},
                visiblecheck={acl, use, mod_admin_config}}

     |Acc].

%% @doc Check the logon event for the Zotonic native username/password registration.
observe_logon_submit(#logon_submit{query_args=Args}, Context) ->
    Username = proplists:get_value(<<"username">>, Args),
    Password = proplists:get_value(<<"password">>, Args),
    case Username /= undefined andalso Password /= undefined of
        true ->
            case m_identity:check_username_pw(Username, Password, Context) of
                {ok, Id} ->
                    case Password of
                        [] ->
                            %% When empty password existed in identity table, prompt for a new password.
                            %% FIXME do real password expiration here.
                            {expired, Id};
                        _ -> {ok, Id}
                    end;
                E -> E
            end;
        false ->
            undefined
    end.

observe_auth_autologon(#auth_autologon{}, Context) ->
    case controller_logon:get_rememberme_cookie(Context) of
        undefined -> undefined;
        {ok, UserId} -> {ok, UserId}
    end.


%% @doc Handle a validation against an (external) authentication service.
%%      If identity is known: log on the associated user and session
%%      If unknown, add identity to current user or signup a new user
observe_auth_validated(#auth_validated{} = Auth, Context) ->
    z_context:set_persistent(auth_method, Auth#auth_validated.service, Context),
    maybe_add_identity(z_acl:user(Context), Auth, Context).

maybe_add_identity(undefined, Auth, Context) ->
    case auth_identity(Auth, Context) of
        undefined -> maybe_signup(Auth, Context);
        Ps -> logon_identity(Auth, Ps, Context)
    end;
maybe_add_identity(CurrUserId, Auth, Context) ->
    case auth_identity(Auth, Context) of
        undefined ->
            % Unknown identity, add it to the current user
            {ok, _} = insert_identity(CurrUserId, Auth, Context),
            {ok, Context};
        Ps ->
            {rsc_id, IdnRscId} = proplists:lookup(rsc_id, Ps),
            case {IdnRscId, Auth#auth_validated.is_connect} of
                {CurrUserId, _} ->
                    {ok, Context};
                {_UserId, false} ->
                    logon_identity(Auth, Ps, Context);
                {_UserId, true} ->
                    {error, duplicate}
            end
    end.

maybe_update_identity(Ps, Ps, _IdnPs, _Context) ->
    ok;
maybe_update_identity(_Ps1, _Ps2, [], _Context) ->
    ok;
maybe_update_identity(_Ps1, _Ps2, undefined, _Context) ->
    ok;
maybe_update_identity(_Ps, NewProps, IdnPs, Context) ->
    {key, Key} = proplists:lookup(key, IdnPs),
    {type, Type} = proplists:lookup(type, IdnPs),
    {rsc_id, RscId} = proplists:lookup(rsc_id, IdnPs),
    m_identity:set_by_type(RscId, Key, Type, NewProps, Context).


logon_identity(Auth, IdnPs, Context) ->
    {propb, IdnPropb} = proplists:lookup(propb, IdnPs),
    {rsc_id, IdnRscId} = proplists:lookup(rsc_id, IdnPs),
    maybe_update_identity(
        IdnPropb,
        Auth#auth_validated.service_props,
        IdnPs,
        Context),
    z_auth:logon(IdnRscId, Context).


maybe_signup(Auth, Context) ->
    Signup = #signup{
        id = undefined,
        signup_props = maybe_email_identity(Auth#auth_validated.props),
        props = Auth#auth_validated.props,
        request_confirm = false
    },
    case z_notifier:first(Signup, Context) of
        {ok, NewUserId} ->
            case auth_identity(Auth, Context) of
                undefined -> insert_identity(NewUserId, Auth, Context);
                _ -> nop
            end,
            ok = m_identity:ensure_username_pw(NewUserId, z_acl:sudo(Context)),
            z_auth:logon(NewUserId, Context);
        {error, _Reason} = Error ->
            Error;
        undefined ->
            % No signup accepted
            undefined
    end.

maybe_email_identity(Props) ->
    case proplists:get_value(email, Props) of
        undefined -> [];
        Email -> [ {identity, {email, Email, false, false}} ]
    end.

insert_identity(UserId, Auth, Context) ->
    Type = Auth#auth_validated.service,
    Key = Auth#auth_validated.service_uid,
    Props = [
        {is_unique, true},
        {is_verified, true},
        {propb, {term, Auth#auth_validated.service_props}}
    ],
    m_identity:insert(UserId, Type, Key, Props, Context).


auth_identity(#auth_validated{service=Service, service_uid=Uid}, Context) ->
    m_identity:lookup_by_type_and_key(Service, Uid, Context).
