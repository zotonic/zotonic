%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019 Marc Worrell
%% @doc Handle HTTP authentication of users.

%% Copyright 2019 Marc Worrell
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
    allowed_methods/1,
    content_types_accepted/1,
    content_types_provided/1,
    process/4
]).

-include_lib("zotonic_core/include/zotonic.hrl").


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
    {Payload, Context1} = z_controller_helper:decode_request(AcceptedCT, Context),
    {Result, Context2} = handle_cmd( maps:get(<<"cmd">>, Payload, undefined), Payload, Context1 ),
    {z_controller_helper:encode_response(ProvidedCT, Result), Context2}.

-spec handle_cmd( binary(), map(), z:context() ) -> { map(), z:context() }.
handle_cmd(<<"logon">>, Payload, Context) ->
    logon(Payload, Context);
handle_cmd(<<"switch_user">>, Payload, Context) ->
    switch_user(Payload, Context);
handle_cmd(<<"logoff">>, Payload, Context) ->
    logoff(Payload, Context);
handle_cmd(<<"refresh">>, Payload, Context) ->
    refresh(Payload, Context);
handle_cmd(<<"setautologon">>, Payload, Context) ->
    setautologon(Payload, Context);
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
                         "'reset_check', 'reset', 'switch_user' or 'status'">>
        },
        Context
    }.

-spec logon( map(), z:context() ) -> { map(), z:context() }.
logon(Payload, Context) ->
    logon_1(z_notifier:first(#logon_submit{ payload = Payload }, Context), Payload, Context).

logon_1({ok, UserId}, Payload, Context) when is_integer(UserId) ->
    case z_auth:logon(UserId, Context) of
        {ok, Context1} ->
            % - (set cookie in handlers - like device-id) --> needs notification
            Options = z_context:get(auth_options, Context, #{}),
            Context2 = z_authentication_tokens:set_auth_cookie(UserId, Options, Context1),
            status(Payload, Context2);
        {error, user_not_enabled} ->
            case m_rsc:p_no_acl(UserId, is_verified_account, Context) of
                false ->
                    % The account is awaiting verification
                    { #{ status => error, error => verification_pending, user_id => UserId }, Context };
                V when V == true orelse V == undefined ->
                    % The account has been disabled after verification, or
                    % verification flag not set, account didn't need verification
                    { #{ status => error, error => disabled, user_id => UserId }, Context }
            end;
        {error, _Reason} ->
            % Hide other error codes, map to generic 'pw' error
            { #{ status => error, error => pw }, Context }
    end;
logon_1({expired, UserId}, _Payload, Context) when is_integer(UserId) ->
    case m_identity:get_username(UserId, Context) of
        undefined ->
            { #{ status => error, error => pw }, Context };
        _Username ->
            { #{ status => error, error => password_expired }, Context }
    end;
logon_1({error, ratelimit}, _Payload, Context) ->
    { #{ status => error, error => ratelimit }, Context };
logon_1({error, need2fa}, _Payload, Context) ->
    { #{ status => error, error => need2fa }, Context };
logon_1({error, _Reason}, _Payload, Context) ->
    % Hide other error codes, map to generic 'pw' error
    { #{ status => error, error => pw }, Context };
logon_1(undefined, _Payload, Context) ->
    lager:warning("Authentication error: #logon_submit{} returned undefined."),
    { #{ status => error, error => pw }, Context }.


-spec switch_user( map(), z:context() ) -> { map(), z:context() }.
switch_user(#{ <<"user_id">> := UserId } = Payload, Context) when is_integer(UserId) ->
    AuthOptions = z_context:get(auth_options, Context, #{}),
    case z_auth:logon_switch(UserId, Context) of
        {ok, Context1} ->
            lager:warning("[~p] Authentication: user ~p is switching to user ~p",
                          [ z_context:site(Context), z_acl:user(Context), UserId ]),
            Context2 = z_authentication_tokens:set_auth_cookie(UserId, AuthOptions, Context1),
            status(Payload, Context2);
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
    status(Payload, Context2).

%% @doc Refresh the current authentication cookie
-spec refresh( map(), z:context() ) -> { map(), z:context() }.
refresh(Payload, Context) ->
    Options = case maps:get(<<"options">>, Payload, #{}) of
        V when is_map(V) -> V;
        _ -> #{}
    end,
    Context1 = z_authentication_tokens:refresh_auth_cookie(Options, Context),
    status(Payload, Context1).

%% @doc Set an autologon cookie for the current user
%% @todo Do not set the cookie if the user has 2fa enabled
-spec setautologon( map(), z:context() ) -> { map(), z:context() }.
setautologon(_Payload, Context) ->
    case z_acl:user(Context) of
        undefined ->
            { #{ status => error, error => nouser }, Context };
        UserId ->
            Context1 = z_authentication_tokens:set_autologon_cookie(UserId, Context),
            { #{ status => ok }, Context1 }
    end.

%% @doc Set an autologon cookie for the current user
%% @todo Do not set the cookie if the user has 2fa enabled
-spec reset( map(), z:context() ) -> { map(), z:context() }.
reset(#{
        <<"secret">> := Secret,
        <<"username">> := Username,
        <<"password">> := Password,
        <<"passcode">> := Passcode
    } = Payload, Context) when is_binary(Secret), is_binary(Username), is_binary(Password), is_binary(Passcode) ->
    case auth_precheck(Username, Context) of
        ok ->
            PasswordMinLength = z_convert:to_integer(m_config:get_value(mod_authentication, password_min_length, 6, Context)),

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
                                    case reset_1(UserId, Username, Password, Context) of
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


reset_1(UserId, Username, Password, Context) ->
    case auth_postcheck(UserId, z_context:get_q_all(Context), Context) of
        ok ->
            ContextLoggedon = z_acl:logon(UserId, Context),
            m_identity:set_username_pw(UserId, Username, Password, z_acl:sudo(ContextLoggedon)),
            m_identity:delete_by_type(UserId, "logon_reminder_secret", ContextLoggedon),
            ok;
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
        options => z_context:get(auth_options, Context1, #{})
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
                            NeedPasscode = case auth_postcheck(UserId, [], Context) of
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
    case m_identity:lookup_by_type_and_key("logon_reminder_secret", Code, Context) of
        undefined -> undefined;
        Row -> {ok, proplists:get_value(rsc_id, Row)}
    end.


auth_precheck(Username, Context) when is_binary(Username) ->
    case z_notifier:first(#auth_precheck{ username = Username }, Context) of
        undefined -> ok;
        ok -> ok;
        Error -> Error
    end.

auth_postcheck(UserId, QueryArgs, Context) ->
    case z_notifier:first(#auth_postcheck{ id = UserId, query_args = QueryArgs }, Context) of
        undefined -> ok;
        ok -> ok;
        Error -> Error
    end.

