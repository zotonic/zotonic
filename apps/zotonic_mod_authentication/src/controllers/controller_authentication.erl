%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019-2021 Marc Worrell
%% @doc Handle HTTP authentication of users.

%% Copyright 2019-2021 Marc Worrell
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

-module(controller_authentication).

-author("Marc Worrell <marc@worrell.nl>").

-export([
    service_available/1,
    allowed_methods/1,
    content_types_accepted/1,
    content_types_provided/1,
    process/4
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(RESET_TOKEN_MAXAGE, 48*3600).

service_available(Context) ->
    Context1 = z_context:set_noindex_header(Context),
    Context2 = z_context:set_nocache_headers(Context1),
    {true, Context2}.

allowed_methods(Context) ->
    {[ <<"POST">> ], Context}.

content_types_accepted(Context) ->
    {[
        {<<"application">>, <<"json">>, []},
        {<<"text">>, <<"x-ubf">>, []},
        {<<"application">>, <<"x-bert">>, []},
        {<<"application">>, <<"x-www-form-urlencoded">>, []},
        {<<"multipart">>, <<"form-data">>, []}
    ], Context}.

content_types_provided(Context) ->
    {[
        {<<"application">>, <<"json">>, []},
        {<<"text">>, <<"x-ubf">>, []},
        {<<"application">>, <<"x-bert">>, []}
    ], Context}.

process(<<"POST">>, AcceptedCT, ProvidedCT, Context) ->
    case fetch_body(AcceptedCT, Context) of
        {ok, {Payload, Context1}} ->
            {Result, Context2} = handle_cmd( maps:get(<<"cmd">>, Payload, undefined), Payload, Context1 ),
            {z_controller_helper:encode_response(ProvidedCT, Result), Context2};
        {error, timeout} ->
            {{halt, 500}, Context}
    end.

fetch_body(AcceptedCT, Context) ->
    try
        Ret = z_controller_helper:decode_request(AcceptedCT, Context),
        {ok, Ret}
    catch
        exit:timeout ->
            % Timeout reading the request body
            {error, timeout}
    end.

-spec handle_cmd( binary(), map(), z:context() ) -> { map(), z:context() }.
handle_cmd(<<"logon">>, Payload, Context) ->
    logon(Payload, Context);
handle_cmd(<<"switch_user">>, Payload, Context) ->
    switch_user(Payload, Context);
handle_cmd(<<"onetime_token">>, Payload, Context) ->
    onetime_token(Payload, Context);
handle_cmd(<<"logoff">>, Payload, Context) ->
    logoff(Payload, Context);
handle_cmd(<<"refresh">>, Payload, Context) ->
    refresh(Payload, Context);
handle_cmd(<<"setautologon">>, Payload, Context) ->
    setautologon(Payload, Context);
handle_cmd(<<"change">>, Payload, Context) ->
    change(Payload, Context);
handle_cmd(<<"reset_check">>, Payload, Context) ->
    { check_reminder_secret(Payload, Context), Context };
handle_cmd(<<"reset">>, Payload, Context) ->
    reset(Payload, Context);
handle_cmd(<<"status">>, Payload, Context) ->
    status(Payload, Context);
handle_cmd(Cmd, _Payload, Context) ->
    lager:info("controller_authentication: unknown cmd ~p", [ Cmd ]),
    {
        #{
            status => error,
            error => <<"unknown_cmd">>,
            message => <<"Unknown cmd, use one of 'logon', 'logoff', 'refresh', 'setautologon', "
                         "'reset_check', 'reset', 'change', switch_user' or 'status'">>
        },
        Context
    }.

-spec logon( map(), z:context() ) -> { map(), z:context() }.
logon(Payload, Context) ->
    {Result, Context1} = logon_1(z_notifier:first(#logon_submit{ payload = Payload }, Context), Payload, Context),
    maybe_add_logon_options(Result, Payload, Context1).

logon_1({ok, UserId}, Payload, Context) when is_integer(UserId) ->
    case z_auth:logon(UserId, Context) of
        {ok, Context1} ->
            log_logon(UserId, Payload, Context),
            % - (set cookie in handlers - like device-id) --> needs notification
            Options = z_context:get(auth_options, Context, #{}),
            Context2 = z_authentication_tokens:set_auth_cookie(UserId, Options, Context1),
            Context3 = maybe_setautologon(Payload, Context2),
            return_status(Payload, Context3);
        {error, user_not_enabled} ->
            Reply = #{ status => error, user_id => UserId },
            case m_rsc:p_no_acl(UserId, is_verified_account, Context) of
                false ->
                    % The account is awaiting verification
                    Token = z_utils:pickle( #{ user_id => UserId, timestamp => calendar:universal_time() }, Context),
                    { Reply#{ error => verification_pending, token => Token }, Context };
                V when V == true orelse V == undefined ->
                    % The account has been disabled after verification, or
                    % verification flag not set, account didn't need verification
                    { Reply#{ error => disabled }, Context }
            end
        % {error, _Reason} ->
        %     % Hide other error codes, map to generic 'pw' error
        %     { #{ status => error, error => pw }, Context }
    end;
logon_1({expired, UserId}, _Payload, Context) when is_integer(UserId) ->
    % The password is expired and needs a reset - this is similar to password reset
    case m_identity:get_username(UserId, Context) of
        undefined ->
            { #{ status => error, error => pw }, Context };
        Username ->
            Code = m_authentication:set_reminder_secret(UserId, Context),
            { #{ status => error, error => password_expired, username => Username, secret => Code }, Context }
    end;
logon_1({error, ratelimit}, _Payload, Context) ->
    { #{ status => error, error => ratelimit }, Context };
logon_1({error, need_passcode}, _Payload, Context) ->
    { #{ status => error, error => need_passcode }, Context };
logon_1({error, passcode}, _Payload, Context) ->
    { #{ status => error, error => passcode }, Context };
logon_1({error, _Reason}, _Payload, Context) ->
    % Hide other error codes, map to generic 'pw' error
    { #{ status => error, error => pw }, Context };
logon_1(undefined, _Payload, Context) ->
    { #{ status => error, error => pw }, Context }.


log_logon(UserId, #{ <<"username">> := Username }, Context) when is_binary(Username) ->
    lager:info("~p: Logon of user ~p (~s) using username '~s'",
                [ z_context:site(Context), UserId, username(UserId, Context), Username ]);
log_logon(UserId, #{ <<"token">> := Token }, Context) when is_binary(Token) ->
    lager:info("~p: Logon of user ~p (~s) using token",
                [ z_context:site(Context), UserId, username(UserId, Context) ]);
log_logon(UserId, #{}, Context) ->
    lager:info("~p: Logon of user ~p (~s)",
                [ z_context:site(Context), UserId, username(UserId, Context) ]).

username(UserId, Context) ->
    case m_identity:get_username(UserId, Context) of
        Username when is_binary(Username) -> Username;
        undefined -> <<>>
    end.


-spec maybe_add_logon_options( map(), map(), z:context() ) -> { map(), z:context() }.
maybe_add_logon_options(#{ error := ratelimit } = Result, _Payload, Context) ->
    {Result, Context};
maybe_add_logon_options(#{ status := error } = Result, Payload, Context) ->
    Options = #{
        is_username_checked => false,
        is_user_local => false,
        is_user_external => false,
        username => maps:get(<<"username">>, Payload, undefined),
        user_external => [],
        page => maps:get(<<"page">>, Payload, undefined),
        is_password_entered => not z_utils:is_empty(maps:get(<<"password">>, Payload, <<>>))
    },
    Options1 = case Result of #{ token := Token } -> Options#{ token => Token}; _ -> Options end, 
    Options2 = z_notifier:foldr(#logon_options{ payload = Payload }, Options1, Context),
    Result1 = Result#{ options => Options2 },
    case Options2 of
        #{ is_user_external := true } -> {Result1, Context};
        #{ is_user_local := true } -> {Result1, Context};
        #{ is_user_local := false, is_user_external := false, username := Username } when is_binary(Username), Username =/= <<>> ->
            % Hide the fact an user is unknown, this prevents fishing for known usernames.
            Options3 = Options2#{
                is_user_local => true,
                is_username_checked => true
            },
            {Result1#{ options => Options3 }, Context};
        _ ->
            {Result1, Context}
    end;
maybe_add_logon_options(#{ status := ok } = Result, _Payload, Context) ->
    {Result, Context}.


-spec onetime_token( map(), z:context() ) -> { map(), z:context() }.
onetime_token(#{ <<"token">> := Token } = Payload, Context) ->
    case z_authentication_tokens:decode_onetime_token(Token, Context) of
        {ok, UserId} ->
            logon_1({ok, UserId}, Payload, Context);
        {error, _} ->
            { #{ status => error, error => token }, Context }
    end;
onetime_token(_Payload, Context) ->
    { #{ status => error, error => missing_token }, Context }.

-spec switch_user( map(), z:context() ) -> { map(), z:context() }.
switch_user(#{ <<"user_id">> := UserId } = Payload, Context) when is_integer(UserId) ->
    AuthOptions = z_context:get(auth_options, Context, #{}),
    SudoUserId = maps:get(sudo_user_id, AuthOptions, z_acl:user(Context)),
    case z_auth:logon_switch(UserId, SudoUserId, Context) of
        {ok, Context1} ->
            z:warning(
                "Sudo as user ~p (~s) by user ~p (~s)",
                [
                    UserId, z_convert:to_binary( m_rsc:p_no_acl(UserId, email, Context) ),
                    z_acl:user(Context), z_convert:to_binary( m_rsc:p_no_acl(z_acl:user(Context), email, Context) )
                ],
                [
                    {module, ?MODULE}, {line, ?LINE}, {auth_user_id, UserId}
                ],
                Context),
            Options2 = AuthOptions#{
                sudo_user_id => SudoUserId
            },
            Context2 = z_authentication_tokens:set_auth_cookie(UserId, Options2, Context1),
            return_status(Payload, Context2);
        {error, _Reason} ->
            { #{ status => error, error => eacces }, Context }
    end;
switch_user(_Payload, Context) ->
    { #{ status => error, error => missing_user_id }, Context }.


%% @doc Remove authentication cookie(s), signal user logoff
-spec logoff( map(), z:context() ) -> { map(), z:context() }.
logoff(Payload, Context) ->
    Context1 = z_auth:logoff(Context),
    Context2 = z_authentication_tokens:reset_cookies(Context1),
    return_status(Payload, Context2).

%% @doc Refresh the current authentication cookie
-spec refresh( map(), z:context() ) -> { map(), z:context() }.
refresh(Payload, Context) ->
    Options = case maps:get(<<"options">>, Payload, #{}) of
        V when is_map(V) -> V;
        _ -> #{}
    end,
    Context1 = z_authentication_tokens:refresh_auth_cookie(Options, Context),
    return_status(Payload, Context1).

%% @doc Set an autologon cookie for the current user
-spec setautologon( map(), z:context() ) -> { map(), z:context() }.
setautologon(_Payload, Context) ->
    case z_acl:user(Context) of
        undefined ->
            { #{ status => error, error => nouser }, Context };
        UserId ->
            Context1 = z_authentication_tokens:set_autologon_cookie(UserId, Context),
            { #{ status => ok }, Context1 }
    end.

%% @doc Set the autologon cookie for the user if the flag was speficied in the request.
-spec maybe_setautologon( map(), z:context() ) -> z:context().
maybe_setautologon(#{ <<"setautologon">> := SetAutoLogon }, Context) ->
    case z_convert:to_bool(SetAutoLogon) of
        true ->
            z_authentication_tokens:set_autologon_cookie(z_acl:user(Context), Context);
        false ->
            z_authentication_tokens:reset_autologon_cookie(Context)
    end;
maybe_setautologon(_Payload, Context) ->
    z_authentication_tokens:reset_autologon_cookie(Context).


%% @doc Change the password for the current user, use the (optional) 2FA code
-spec change( map(), z:context() ) -> { map(), z:context() }.
change(#{
        <<"password">> := Password,
        <<"password_reset">> := NewPassword,
        <<"passcode">> := Passcode
    }, Context) when is_binary(NewPassword), is_binary(Password), is_binary(Passcode) ->
    case z_acl:user(Context) of
        undefined ->
            { #{ status => error, error => no_user }, Context };
        UserId ->
            case m_identity:get_username(UserId, Context) of
                undefined ->
                    lager:error("Password change: User ~p does not have an username defined.", [ UserId ]),
                    { #{ status => error, error => username }, Context };
                Username ->
                    case auth_precheck(Username, Context) of
                        ok ->
                            PasswordMinLength = z_convert:to_integer(m_config:get_value(mod_authentication, password_min_length, 8, Context)),

                            case size(Password) of
                                N when N < PasswordMinLength ->
                                    { #{ status => error, error => tooshort }, Context };
                                _ ->
                                    change_1(UserId, Username, Password, NewPassword, Passcode, Context)
                            end;
                        {error, ratelimit} ->
                            { #{ status => error, error => ratelimit }, Context };
                        _ ->
                            { #{ status => error, error => error }, Context }
                    end
            end
    end;
change(_Payload, Context) ->
    { #{
        status => error,
        error => args,
        message => <<"Missing one of: password, password_reset, passcode">>
    }, Context }.


change_1(UserId, Username, Password, NewPassword, Passcode, Context) ->
    Payload = #{
        <<"username">> => Username,
        <<"password">> => Password,
        <<"passcode">> => Passcode
    },
    case z_notifier:first(#logon_submit{ payload = Payload }, Context) of
        {ok, UserId} ->
            case reset_1(UserId, Username, NewPassword, Passcode, Context) of
                ok ->
                    logon_1({ok, UserId}, Payload, Context);
                {error, Reason} ->
                    { #{ status => error, error => Reason }, Context }
            end;
        {error, ratelimit} ->
            { #{ status => error, error => ratelimit }, Context };
        {error, need_passcode} ->
            { #{ status => error, error => need_passcode }, Context };
        {error, passcode} ->
            { #{ status => error, error => passcode }, Context };
        {ok, _} ->
            { #{ status => error, error => pw }, Context }
    end.

%% @doc Reset the password for an user, using the mailed reset secret and (optional) 2FA code.
-spec reset( map(), z:context() ) -> { map(), z:context() }.
reset(#{
        <<"secret">> := Secret,
        <<"username">> := Username,
        <<"password">> := Password,
        <<"passcode">> := Passcode
    } = Payload, Context) when is_binary(Secret), is_binary(Username), is_binary(Password), is_binary(Passcode) ->
    case auth_precheck(Username, Context) of
        ok ->
            PasswordMinLength = z_convert:to_integer(m_config:get_value(mod_authentication, password_min_length, 8, Context)),

            case size(Password) of
                N when N < PasswordMinLength ->
                    { #{ status => error, error => tooshort }, Context };
                _ ->
                    case get_by_reminder_secret(Secret, Context) of
                        {ok, UserId} ->
                            case m_identity:get_username(UserId, Context) of
                                undefined ->
                                    lager:error("Password reset: User ~p does not have an username defined.", [ UserId ]),
                                    { #{ status => error, error => username }, Context };
                                Username ->
                                    case reset_1(UserId, Username, Password, Passcode, Context) of
                                        ok ->
                                            logon_1({ok, UserId}, Payload, Context);
                                        {error, Reason} ->
                                            { #{ status => error, error => Reason }, Context }
                                    end
                            end;
                        undefined ->
                            { #{ status => error, error => unknown_code }, Context }
                    end
            end;
        {error, ratelimit} ->
            { #{ status => error, error => ratelimit }, Context };
        _ ->
            { #{ status => error, error => error }, Context }
    end;
reset(_Payload, Context) ->
    { #{
        status => error,
        error => args,
        message => <<"Missing one of: secret, username, password, passcode">>
    }, Context }.


reset_1(UserId, Username, Password, Passcode, Context) ->
    QArgs = #{
        <<"username">> => Username,
        <<"passcode">> => Passcode
    },
    case auth_postcheck(UserId, QArgs, Context) of
        ok ->
            case m_identity:set_username_pw(UserId, Username, Password, z_acl:sudo(Context)) of
                ok ->
                    ContextLoggedon = z_acl:logon(UserId, Context),
                    delete_reminder_secret(UserId, ContextLoggedon),
                    ok;
                {error, password_match} ->
                    {error, password_change_match};
                {error, _} ->
                    {error, error}
            end;
        {error, need_passcode} ->
            {error, need_passcode};
        {error, passcode} ->
            z_notifier:notify_sync(
                #auth_checked{
                    id = UserId,
                    username = Username,
                    is_accepted = false
                },
                Context),
            {error, passcode};
        _Error ->
            {error, error}
    end.

%% @doc Return information about the current user and request language/timezone
-spec status( map(), z:context() ) -> { map(), z:context() }.
status(Payload, Context) ->
    Context1 = z_authentication_tokens:ensure_auth_cookie(Context),
    return_status(Payload, Context1).

return_status(Payload, Context) ->
    Context1 = z_notifier:foldl(
        #request_context{
            phase = auth_status,
            document = maps:get(<<"document">>, Payload, #{})
        },
        Context,
        Context),
    Status = #{
        status => ok,
        is_authenticated => z_auth:is_auth(Context1),
        user_id => z_acl:user(Context1),
        username => m_identity:get_username(Context1),
        preferences => #{
            language => z_context:language(Context1),
            timezone => z_context:tz(Context1)
        },
        options => z_context:get(auth_options, Context1, #{}),
        url => maps:get(<<"url">>, Payload, undefined)
    },
    Status1 = case z_auth:is_auth(Context1) of
        true ->
            Status#{ expires => z_context:get(auth_expires, Context1) };
        false ->
            Status
    end,
    { Status1, Context1 }.


-spec check_reminder_secret( map(), z:context() ) -> map().
check_reminder_secret(#{ <<"secret">> := Secret, <<"username">> := Username }, Context) when is_binary(Secret), is_binary(Username) ->
    case z_notifier:first( #auth_reset{ username = Username }, Context ) of
        Ok when Ok =:= undefined; Ok =:= ok ->
            case get_by_reminder_secret(Secret, Context) of
                {ok, UserId} ->
                    case m_identity:get_username(UserId, Context) of
                        undefined ->
                            {[], Context};
                        Username ->
                            NeedPasscode = case auth_postcheck(UserId, #{}, Context) of
                                {error, need_passcode} -> true;
                                _ -> false
                            end,
                            #{
                                status => ok,
                                user_id => UserId,
                                username => Username,
                                need_passcode => NeedPasscode
                            };
                        OtherUsername ->
                            lager:error("Password reset with username mismatch: got \"~s\", expected \"~s\"",
                                        [ Username, OtherUsername ]),
                            #{
                                status => error,
                                error => unknown_code
                            }
                    end;
                undefined ->
                    #{
                        status => error,
                        error => unknown_code
                    }
            end;
        {error, Reason} ->
            #{
                status => error,
                error => Reason
            }
    end;
check_reminder_secret(_Payload, _Context) ->
    #{
        status => error,
        error => args,
        message => <<"Missing username and/or secret">>
    }.

get_by_reminder_secret(Code, Context) ->
    MaxAge = case z_convert:to_integer( m_config:get_value(mod_authentication, reset_token_maxage, Context) ) of
        undefined -> ?RESET_TOKEN_MAXAGE;
        MA -> MA
    end,
    case m_identity:lookup_by_type_and_key(logon_reminder_secret, Code, Context) of
        undefined ->
            undefined;
        Row ->
            {rsc_id, UserId} = proplists:lookup(rsc_id, Row),
            {modified, Modified} = proplists:lookup(modified, Row),
            ModifiedTm = z_datetime:datetime_to_timestamp(Modified),
            case z_datetime:timestamp() < ModifiedTm + MaxAge of
                true ->
                    {ok, UserId};
                false ->
                    lager:info("Accessing expired reminder secret for user ~p", [UserId]),
                    delete_reminder_secret(UserId, Context),
                    undefined
            end
    end.

%% @doc Delete the reminder secret of the user
delete_reminder_secret(Id, Context) ->
    m_identity:delete_by_type(Id, logon_reminder_secret, Context).


auth_precheck(Username, Context) when is_binary(Username) ->
    case z_notifier:first(#auth_precheck{ username = Username }, Context) of
        undefined -> ok;
        ok -> ok;
        Error -> Error
    end.

auth_postcheck(UserId, QueryArgs, Context) when is_map(QueryArgs) ->
    case z_notifier:first(#auth_postcheck{ id = UserId, query_args = QueryArgs }, Context) of
        undefined -> ok;
        ok -> ok;
        Error -> Error
    end.

