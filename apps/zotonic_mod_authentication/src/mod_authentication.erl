%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2024 Marc Worrell
%% @doc Authentication and identification of users.
%% @end

%% Copyright 2010-2024 Marc Worrell
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
    observe_logon_options/3,
    observe_admin_menu/3,
    observe_auth_validated/2,
    observe_auth_client_logon_user/2,
    observe_auth_client_switch_user/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").


init(Context) ->
    % Ensure password_min_length config
    case m_config:get(?MODULE, password_min_length, Context) of
        undefined -> m_config:set_value(?MODULE, password_min_length, "8", Context);
        _ -> nop
    end,
    ok.

event(#submit{ message={signup_confirm, Props} }, Context) ->
    {auth, Auth} = proplists:get_value(auth, Props),
    Auth1 = Auth#auth_validated{ is_signup_confirmed = true },
    case z_notifier:first(Auth1, Context) of
        undefined ->
            ?LOG_ERROR(#{
                text => <<"mod_authentication: 'undefined' return for auth">>,
                in => zotonic_mod_authentication,
                result => error,
                reason => no_auth,
                auth => Auth
            }),
            z_render:wire({show, [{target, "signup_error"}]}, Context);
        {error, Reason} ->
            ?LOG_WARNING(#{
                text => <<"mod_authentication: Error return for auth">>,
                in => zotonic_mod_authentication,
                result => error,
                reason => Reason,
                auth => Auth
            }),
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
            Context2 = case z_auth:is_auth(Context1) of
                false ->
                    z_authentication_tokens:req_autologon_cookie(Context1);
                true ->
                    Context1
            end,
            z_notifier:foldl(#session_context{ request_type = http, payload = undefined }, Context2, Context2)
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
                {false, _} -> AccOpts
            end;
        #{} ->
            AccOpts
    end.

%% @doc Check username/password against the identity tables.
observe_logon_submit(#logon_submit{
            payload = #{
                <<"username">> := Username,
                <<"password">> := Password
            } = Payload
        }, Context) when is_binary(Username), is_binary(Password) ->
    case m_identity:check_username_pw(Username, Password, Payload, Context) of
        {ok, 1} ->
            {ok, 1};
        {ok, UserId} ->
            case m_authentication:is_valid_password(Password, Context) of
                true ->
                    {ok, UserId};
                false ->
                    % If empty or invalid password existed in identity
                    % table then prompt for a new password.
                    {expired, UserId}
            end;
        {error, {expired, UserId}} ->
            {expired, UserId};
        {error, _} = E ->
            E
    end;
observe_logon_submit(#logon_submit{}, _Context) ->
    undefined.

observe_logon_options(#logon_options{
            payload = #{
                <<"username">> := Username,
                <<"password">> := undefined
            }
        },
        Acc,
        Context) when is_binary(Username) ->
    case z_string:to_lower( z_string:trim( Username ) ) of
        <<>> ->
            Acc;
        UsernameOrEmail ->
            IsUserLocal = is_user_local(UsernameOrEmail, Context)
                   orelse is_user_local_email(UsernameOrEmail, Context)
                   orelse maps:get(is_user_local, Acc, false),
            Acc#{
                is_username_checked => true,
                is_user_local => IsUserLocal,
                username => UsernameOrEmail
            }
    end;
observe_logon_options(#logon_options{}, Acc, _Context) ->
    Acc.


%% @doc Send a request to the client to login a user. The zotonic.auth.worker.js will
%% send a request to controller_authentication to exchange the one time token with
%% a z,auth cookie for the given user. The client will redirect to the Url.
observe_auth_client_logon_user(#auth_client_logon_user{ user_id = UserId, url = Url }, Context) ->
    case z_context:client_topic(Context) of
        {ok, ClientTopic} ->
            case z_authentication_tokens:encode_onetime_token(UserId, Context) of
                {ok, Token} ->
                    z_mqtt:publish(
                        ClientTopic ++ [ <<"model">>, <<"auth">>, <<"post">>, <<"onetime-token">> ],
                        #{
                            token => Token,
                            url => Url
                        },
                        Context),
                    ok;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc Send a request to the client to switch users. The zotonic.auth.worker.js will
%% send a request to controller_authentication to perform the switch.
observe_auth_client_switch_user(#auth_client_switch_user{ user_id = UserId }, Context) ->
    CurrentUser = z_acl:user(Context),
    case UserId of
        1 when CurrentUser =/= 1 ->
            % Only the admin is allowed to switch back to admin
            {error, eacces};
        _ ->
            case z_acl:is_admin(Context) of
                true ->
                    case z_context:client_topic(Context) of
                        {ok, ClientTopic} ->
                            z_mqtt:publish(
                                    ClientTopic ++ [ <<"model">>, <<"auth">>, <<"post">>, <<"switch-user">> ],
                                    #{ user_id => UserId },
                                    Context),
                            ok;
                        {error, _} = Error ->
                            Error
                    end;
                false ->
                    {error, eacces}
            end
    end.

is_user_local(<<"admin">>, _Context) ->
    true;
is_user_local(Handle, Context) when is_binary(Handle) ->
    case m_identity:lookup_by_username(Handle, Context) of
        undefined -> false;
        _Row -> true
    end.

is_user_local_email(Handle, Context) ->
    case binary:match(Handle, <<"@">>) of
        nomatch -> false;
        _ -> length(m_identity:lookup_users_by_type_and_key(email, Handle, Context)) > 0
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
observe_auth_validated(#auth_validated{ is_connect = IsConnect } = Auth, Context) ->
    Context1 = z_context:set(auth_method, Auth#auth_validated.service, Context),
    case IsConnect of
        true ->
            maybe_add_identity_connect(z_acl:user(Context1), Auth, Context1);
        false ->
            maybe_add_identity_logon(Auth, Context1)
    end.

maybe_add_identity_logon(Auth, Context) ->
    case auth_identity(Auth, Context) of
        undefined ->
            VerifiedUserEmails = auth_match_email(Auth, true, Context),
            UnverifiedUserEmails = auth_match_email(Auth, false, Context),
            {VerifiedUserIds, VerifiedEmails} = lists:unzip(VerifiedUserEmails),
            {UnVerifiedUserIds, UnVerifiedEmails} = lists:unzip(UnverifiedUserEmails),
            case {lists:usort(VerifiedUserIds), lists:usort(UnVerifiedUserIds)} of
                {[], []} ->
                    % The SSO supplied email addresses do not match any locally verified
                    % email address.
                    maybe_signup(Auth, Context);
                {[1|_], _} ->
                    % Never add an external identity to the admin user during log on.
                    {error, duplicate};
                {_, [1|_]} ->
                    {error, duplicate};
                {[UserId], _} when Auth#auth_validated.is_signup_confirmed ->
                    % Local user where the user has confirmed their identity by
                    % logging in into their account.
                    {ok, _} = insert_identity(UserId, Auth, Context),
                    {ok, UserId};
                {[], [UserId]} when Auth#auth_validated.is_signup_confirmed ->
                    {ok, _} = insert_identity(UserId, Auth, Context),
                    {ok, UserId};
                {[UserId], _} when not Auth#auth_validated.is_signup_confirmed ->
                    % Local user with matching verified email identity.
                    case z_notifier:first(#auth_postcheck{ id = UserId, query_args = #{} }, Context) of
                        {error, need_passcode} ->
                            % Local 2FA enabled - let the user enter their code
                            {error, {need_passcode, UserId}};
                        undefined ->
                            % As both SSO and local email addresses are confirmed AND there
                            % is no local 2FA enabled, add SSO identities and allow direct logon.
                            {ok, _} = insert_identity(UserId, Auth, Context),
                            {ok, UserId}
                    end;
                {[], [UserId]} when not Auth#auth_validated.is_signup_confirmed ->
                    % As the external email address is not verified, the user has to log on
                    % using their local username and password.
                    {error, {logon_confirm, UserId, hd(UnVerifiedEmails)}};
                {_, []}  ->
                    % Ambiguous - multiple matching accounts
                    {error, {multiple_email, hd(VerifiedEmails)}};
                {[], _}  ->
                    {error, {multiple_email, hd(UnVerifiedEmails)}}
            end;
        Ps when is_list(Ps) ->
            update_identity(Auth, Ps, Context)
    end.

maybe_add_identity_connect(CurrUserId, Auth, Context) ->
    case auth_identity(Auth, Context) of
        undefined ->
            % Unknown identity, add it to the current user
            {ok, _} = insert_identity(CurrUserId, Auth, Context),
            {ok, CurrUserId};
        Ps ->
            {rsc_id, IdnRscId} = proplists:lookup(rsc_id, Ps),
            if
                IdnRscId =:= CurrUserId ->
                    {ok, CurrUserId};
                true ->
                    {error, duplicate}
            end
    end.

% @doc Update the props of the matched identity record.
update_identity(Auth, IdnPs, Context) ->
    {propb, IdnPropb} = proplists:lookup(propb, IdnPs),
    {rsc_id, UserId} = proplists:lookup(rsc_id, IdnPs),
    maybe_update_identity(
        IdnPropb,
        Auth#auth_validated.service_props,
        IdnPs,
        Context),
    {ok, UserId}.

maybe_update_identity(Props, Props, _IdnPs, _Context) ->
    % props unchanged
    ok;
maybe_update_identity(_OldProps, _NewProps, [], _Context) ->
    % no identity
    ok;
maybe_update_identity(_OldProps, NewProps, IdnPs, Context) ->
    {key, Key} = proplists:lookup(key, IdnPs),
    {type, Type} = proplists:lookup(type, IdnPs),
    {rsc_id, UserId} = proplists:lookup(rsc_id, IdnPs),
    m_identity:set_by_type(UserId, Type, Key, NewProps, Context).

maybe_signup(Auth, Context) ->
    case auth_match_primary_email(Auth, Context) of
        [] ->
            try_signup(Auth, Context);
        [Email|_] ->
            % External service uses an email address that is connected as
            % a primary email address to an account here.
            % Either the external service is not verified or our local
            % email identity is not verified.
            {error, {duplicate_email, Email}}
    end.

-spec auth_match_email(#auth_validated{}, IsVerified, Context) -> list( {UserId, Email} ) when
    IsVerified :: boolean(),
    Context :: z:context(),
    UserId :: m_rsc:resource_id(),
    Email :: binary().
auth_match_email(#auth_validated{ identities = Identities }, IsVerified, Context) ->
    Emails = lists:filtermap(
        fun
            (#{ type := <<"email">>, key := E, is_verified := IsIdnVerified }) when IsIdnVerified =:= IsVerified ->
                {true, m_identity:normalize_key(email, E)};
            (_) ->
                false
        end,
        Identities),
    find_verified_email_idns(Emails, Context).

%% Find all user ids with a verified email address matching the given email addresses.
find_verified_email_idns(Emails, Context) ->
    lists:usort(lists:flatten(lists:map(
        fun(Email) ->
            Idns = m_identity:lookup_users_by_verified_type_and_key(email, Email, Context),
            RscIds = lists:map(
                fun(Idn) ->
                    Id = proplists:get_value(rsc_id, Idn),
                    {Id, Email}
                end, Idns),
            lists:filter(fun({Id, _}) -> m_identity:is_user(Id, Context) end, RscIds)
        end,
        Emails))).



-spec auth_match_primary_email(#auth_validated{}, z:context()) -> list( Email::binary() ).
auth_match_primary_email(#auth_validated{ identities = Identities }, Context) ->
    ExtEmails = lists:filtermap(
        fun
            (#{ type := <<"email">>, key := E }) ->
                {true, m_identity:normalize_key(email, E)};
            (_) ->
                false
        end,
        Identities),
    lists:usort(lists:flatten(lists:map(
        fun(ExtEmail) ->
            Idns = m_identity:lookup_users_by_type_and_key(email, ExtEmail, Context),
            lists:filtermap(
                fun(Idn) ->
                    RscId = proplists:get_value(rsc_id, Idn),
                    PrimaryEmail = m_identity:normalize_key(email, m_rsc:p_no_acl(RscId, email_raw, Context)),
                    if
                        ExtEmail =:= PrimaryEmail -> {true, ExtEmail};
                        true -> false
                    end
                end,
                Idns)
        end,
        ExtEmails))).


try_signup(Auth, Context) ->
    case not Auth#auth_validated.is_signup_confirmed
        andalso m_config:get_boolean(mod_authentication, is_signup_confirm, Context)
    of
        true ->
            {error, signup_confirm};
        false ->
            Signup = #signup{
                id = undefined,
                signup_props = email_identities(Auth#auth_validated.identities),
                props = Auth#auth_validated.props,
                request_confirm = false
            },
            case z_notifier:first(Signup, Context) of
                {ok, NewUserId} ->
                    maybe_add_auth_identity(Auth, NewUserId, Context),
                    maybe_ensure_username_pw(Auth, NewUserId, Context),
                    {ok, NewUserId};
                {error, _Reason} = Error ->
                    Error;
                undefined ->
                    % No signup accepted
                    ?LOG_WARNING(#{
                        text => <<"Authentication not accepted because no signup handler defined for Auth">>,
                        in => zotonic_mod_authentication,
                        result => error,
                        reason => no_auth_signup,
                        auth => Auth
                    }),
                    undefined
            end
    end.

%% @doc Add the identity of the authentication provider to the user. Add the identity iff the
%% identity is not already connected to any resource.
maybe_add_auth_identity(Auth, UserId, Context) ->
    case auth_identity(Auth, Context) of
        undefined -> insert_identity(UserId, Auth, z_acl:sudo(Context));
        _ -> ok
    end.

%% @doc Ensure a username_pw identity when signing up, unless the identity service explicitly asks to not
%% add the username_pw identity.
%% @todo Delete the username_pw identity if the service asks not to add it?
maybe_ensure_username_pw(#auth_validated{ ensure_username_pw = true, is_connect = false }, UserId, Context) ->
    m_identity:ensure_username_pw(UserId, z_acl:sudo(Context));
maybe_ensure_username_pw(#auth_validated{}, _UserId, _Context) ->
    ok.

email_identities(Identities) ->
    lists:filtermap(
        fun
            (#{ type := <<"email">>, key := Email, is_verified := IsVerified }) ->
                IsUnique = false,
                Idn = {identity, {email, Email, IsUnique, IsVerified}},
                {true, Idn};
            (_) ->
                false
        end,
        Identities).

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
