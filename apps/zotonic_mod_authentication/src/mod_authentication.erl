%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2019 Marc Worrell
%% @doc Authentication and identification of users.

%% Copyright 2010-2019 Marc Worrell
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

    observe_request_context/3,
    observe_auth_options_update/3,
    observe_logon_submit/2,
    observe_admin_menu/3,
    observe_auth_validated/2
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

event(#submit{ message={signup_confirm, Props} }, Context) ->
    {auth, Auth} = proplists:get_value(auth, Props),
    Auth1 = Auth#auth_validated{ is_signup_confirm = true },
    case z_notifier:first(Auth1, Context) of
        undefined ->
            lager:warning("mod_authentication: 'undefined' return for auth of ~p", [Auth]),
            z_render:wire({show, [{target, "signup_error"}]}, Context);
        {error, _} = Err ->
            lager:warning("mod_authentication: Error return of ~p for auth of ~p", [Err, Auth]),
            z_render:wire({show, [{target, "signup_error"}]}, Context);
        {ok, Context1} ->
            z_render:wire({script, [{script, "window.close()"}]}, Context1)
    end.

%% @doc Check for authentication cookies in the request.
-spec observe_request_context( #request_context{}, z:context(), z:context() ) -> z:context().
observe_request_context(#request_context{ phase = init }, Context, _Context) ->
    case z_context:get(anonymous, Context, false) of
        true ->
            Context;
        false ->
            Context1 = z_authentication_tokens:req_auth_cookie(Context),
            case z_auth:is_auth(Context1) of
                false ->
                    z_authentication_tokens:req_autologon_cookie(Context1);
                true ->
                    Context1
            end
    end;
observe_request_context(#request_context{}, Context, _Context) ->
    Context.


observe_auth_options_update(#auth_options_update{ request_options = ROpts }, AccOpts, Context) ->
    case ROpts of
        #{ <<"acl_user_groups_state">> := undefined } ->
            maps:remove(acl_user_groups_state, AccOpts);
        #{ <<"acl_user_groups_code">> := SignedCode, <<"acl_user_groups_state">> := State } ->
            case {m_acl_rule:is_valid_code(SignedCode, Context), State} of
                {true, <<"edit">>} -> AccOpts#{ acl_user_groups_state => edit };
                {true, <<"publish">>} -> maps:remove(acl_user_groups_state, AccOpts);
                {error, _} -> AccOpts
            end;
        #{} ->
            AccOpts
    end.

%% @doc Check username/password against the identity tables.
observe_logon_submit(#logon_submit{ payload = Args }, Context) ->
    Username = maps:get(<<"username">>, Args, undefined),
    Password = maps:get(<<"password">>, Args, undefined),
    case is_binary(Username) andalso is_binary(Password) of
        true ->
            case m_identity:check_username_pw(Username, Password, Context) of
                {ok, UserId} ->
                    case Password of
                        <<>> ->
                            %% If empty password existed in identity table, prompt for a new password.
                            {expired, UserId};
                        _ ->
                            {ok, UserId}
                    end;
                {error, _} = E ->
                    E
            end;
        false ->
            undefined
    end.

observe_admin_menu(#admin_menu{}, Acc, Context) ->
    [
        #menu_item{
            id = admin_authentication_services,
            parent = admin_auth,
            label = ?__("External Services", Context),
            url = {admin_authentication_services},
            visiblecheck = {acl, use, mod_admin_config}}
        | Acc
    ].

%% @doc Handle a validation against an (external) authentication service.
%%      If identity is known: log on the associated user and set auth cookies.
%%      If unknown, add identity to current user or signup a new user
observe_auth_validated(#auth_validated{} = Auth, Context) ->
    z_context:set(auth_method, Auth#auth_validated.service, Context),
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
    {rsc_id, UserId} = proplists:lookup(rsc_id, IdnPs),
    m_identity:set_by_type(UserId, Key, Type, NewProps, Context).


logon_identity(Auth, IdnPs, Context) ->
    {propb, IdnPropb} = proplists:lookup(propb, IdnPs),
    {rsc_id, UserId} = proplists:lookup(rsc_id, IdnPs),
    maybe_update_identity(
        IdnPropb,
        Auth#auth_validated.service_props,
        IdnPs,
        Context),
    Context1 = z_acl:logon_prefs(UserId, Context),
    z_authentication_tokens:set_auth_cookie(UserId, #{}, Context1).


maybe_signup(Auth, Context) ->
    Email = proplists:get_value(email, Auth#auth_validated.props),
    case not Auth#auth_validated.is_connect
        andalso is_user_email_exists(Email, Context)
    of
        true -> {error, {duplicate_email, Email}};
        false -> try_signup(Auth, Context)
    end.

try_signup(Auth, Context) ->
    case not Auth#auth_validated.is_signup_confirm
        andalso z_convert:to_bool(m_config:get_value(mod_authentication, is_signup_confirm, Context))
    of
        true ->
            {error, signup_confirm};
        false ->
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
                    _ = m_identity:ensure_username_pw(NewUserId, z_acl:sudo(Context)),
                    Context1 = z_acl:logon_prefs(NewUserId, Context),
                    z_authentication_tokens:set_auth_cookie(NewUserId, #{}, Context1);
                {error, _Reason} = Error ->
                    Error;
                undefined ->
                    % No signup accepted
                    undefined
            end
    end.

is_user_email_exists(undefined, _Context) ->
    false;
is_user_email_exists(Email, Context) ->
    case m_identity:lookup_users_by_verified_type_and_key(email, Email, Context) of
        undefined -> false;
        [] -> false;
        _ -> true
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
