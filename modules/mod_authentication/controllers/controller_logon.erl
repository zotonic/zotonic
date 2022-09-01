%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc Log on an user. Set optional "rememberme" cookie.

%% Copyright 2010 Marc Worrell
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

-module(controller_logon).
-author("Marc Worrell <marc@worrell.nl>").

-export([init/1, service_available/2, charsets_provided/2, content_types_provided/2]).
-export([resource_exists/2, previously_existed/2, moved_temporarily/2]).
-export([provide_content/2]).
-export([event/2]).
-export([get_rememberme_cookie/1, set_rememberme_cookie/2, reset_rememberme_cookie/1]).

%% Convenience export for other modules.
-export([logon/2, logon/3, reminder/2]).

%% Convenience export for other auth implementations.
-export([send_reminder/2, lookup_identities/2]).

%% Debugging
-export([is_tos_agreed/2]).


-include_lib("controller_webmachine_helper.hrl").
-include_lib("include/zotonic.hrl").

-define(LOGON_REMEMBERME_COOKIE, "z_logon").
-define(LOGON_REMEMBERME_DAYS, 365).
-define(RESET_TOKEN_MAXAGE, 48*3600).

init(DispatchArgs) -> {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context1 = z_context:new_request(ReqData, DispatchArgs, ?MODULE),
    ?WM_REPLY(true, Context1).

charsets_provided(ReqData, Context) ->
    {[{"utf-8", fun(X) -> X end}], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"text/html", provide_content}], ReqData, Context}.


resource_exists(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = z_context:ensure_all(Context1),
    case z_convert:to_bool(z_context:get(is_only_anonymous, Context2)) of
        true -> check_auth(Context2);
        false -> ?WM_REPLY(true, Context2)
    end.

check_auth(Context) ->
    case z_auth:is_auth(Context) of
        true ->
            case z_utils:is_empty(z_context:get_q("p", Context, [])) of
                true -> ?WM_REPLY(false, Context);
                false -> ?WM_REPLY(true, Context)
            end;
        false ->
            case get_rememberme_cookie(Context) of
                {ok, UserId} ->
                    Context1 = z_context:set(user_id, UserId, Context),
                    case z_auth:logon(UserId, Context1) of
                        {ok, ContextUser} -> ?WM_REPLY(false, ContextUser);
                        {error, _Reason} -> ?WM_REPLY(true, Context1)
                    end;
                undefined ->
                    ?WM_REPLY(true, Context)
            end
    end.


previously_existed(ReqData, Context) ->
    {true, ReqData, Context}.

%% @doc Temporary redirect if we have an automatic log on due to a rememberme cookie or if
%% the user was already logged on and we don't have a redirect page.
moved_temporarily(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Location = z_context:site_url(get_page(Context1), Context1),
    ?WM_REPLY({true, Location}, Context1).


provide_content(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = z_context:set_resp_header("X-Robots-Tag", "noindex", Context1),
    Vars = [
        {noindex, true},
        {notrack, true},
        {page, safe_url(get_page(Context2), Context2)}
        | z_context:get_all(Context2)
    ],
    Secret = z_context:get_q("secret", Context2),
    Username = z_context:get_q("u", Context2),
    {SecretVars, Context3} = reminder_secrets(Secret, Username, Context2),
    ResetVars = reset_vars(Context3),
    Vars1 = ResetVars ++ SecretVars ++ Vars,
    ErrorUId = z_context:get_q("error_uid", Context3),
    ContextVerify = case ErrorUId /= undefined andalso z_utils:only_digits(ErrorUId) of
                        false -> Context3;
                        true -> check_verified(list_to_integer(ErrorUId), Context3)
                    end,
    Template = z_context:get(template, ContextVerify, "logon.tpl"),
    Rendered = z_template:render(Template, Vars1, ContextVerify),
    {Output, OutputContext} = z_context:output(Rendered, ContextVerify),
    ?WM_REPLY(Output, OutputContext).

safe_url(undefined, _Context) -> undefined;
safe_url("", _Context) -> "";
safe_url(<<>>, _Context) -> "";
safe_url(Url, Context) -> z_context:site_url(Url, Context).

reminder_secrets(undefined, _Username, Context) ->
    {[], Context};
reminder_secrets(_Secret, undefined, Context) ->
    {[], Context};
reminder_secrets(Secret, Username, Context) ->
    UsernameB = z_convert:to_binary(Username),
    case z_notifier:first( #auth_reset{ username = UsernameB }, Context ) of
        Ok when Ok =:= undefined; Ok =:= ok ->
            case get_by_reminder_secret(Secret, Context) of
                {ok, UserId} ->
                    case m_identity:get_username(UserId, Context) of
                        undefined ->
                            {[], Context};
                        UsernameB ->
                            NeedPasscode = case auth_postcheck(UserId, [], Context) of
                                {error, need_passcode} -> true;
                                _ -> false
                            end,
                            Vs = [
                                {user_id, UserId},
                                {secret, Secret},
                                {username, UsernameB},
                                {need_passcode, NeedPasscode}
                            ],
                            {Vs, Context};
                        OtherUsername ->
                            lager:error("Password reset with username mismatch: got \"~s\", expected \"~s\"",
                                        [ UsernameB, OtherUsername ]),
                            {[], Context}
                    end;
                undefined ->
                    {[], Context}
            end;
        {error, Reason} ->
            Vs = [
                {error, Reason}
            ],
            {Vs, Context}
    end.

reset_vars(Context) ->
    case z_context:get(zotonic_dispatch, Context) of
        logon_change ->
            case z_acl:user(Context) of
                undefined ->
                    [];
                UserId ->
                    NeedPasscode = case auth_postcheck(UserId, [], Context) of
                        {error, need_passcode} -> true;
                        _ -> false
                    end,
                    [
                        {need_passcode, NeedPasscode}
                    ]
            end;
        _ ->
            []
    end.


%% @doc Get the page we should redirect to after a successful log on.
%%      This location will be stored in the logon form ("page" on submit).
get_page(Context) ->
    HasBackArg = z_convert:to_bool(z_context:get_q("back", Context)),
    case z_context:get_q("p", Context, "") of
        "" when HasBackArg ->
            RD = z_context:get_reqdata(Context),
            case wrq:get_req_header("referer", RD) of
                undefined -> "";
                Referrer -> Referrer
            end;
        Other ->
            Other
    end.

%% @doc User logged on, fetch the location of the next page to show
get_ready_page(Context) ->
    get_ready_page(z_context:get_q("page", Context, ""), Context).

get_ready_page(undefined, Context) ->
    get_ready_page("", Context);
get_ready_page(Page, Context) when is_binary(Page) ->
    get_ready_page(z_convert:to_list(Page), Context);
get_ready_page(Page, Context) when is_list(Page) ->
    case z_notifier:first(#logon_ready_page{request_page=Page}, Context) of
        undefined -> Page;
        Url -> Url
    end.


%% @doc Handle the submit of the logon form, this will be handed over to the
%% different authentication handlers.

event(#postback{message={send_verification, [{user_id, UserId}]}}, Context) ->
    case z_notifier:first(#identity_verification{user_id=UserId}, Context) of
        ok -> logon_stage("verification_sent", Context);
        _Other -> logon_stage("verification_error", Context)
    end;

event(#submit{message={expired, Args}, form="password_expired"}, Context) ->
    {secret, Secret} = proplists:lookup(secret, Args),
    {username, Username} = proplists:lookup(username, Args),
    reset(Secret, Username, Context);

event(#submit{message={reset, Args}, form="password_reset"}, Context) ->
    {secret, Secret} = proplists:lookup(secret, Args),
    {username, Username} = proplists:lookup(username, Args),
    reset(Secret, Username, Context);

event(#submit{message={change, Args}, form="password_reset"}, Context) ->
    {user_id, UserId} = proplists:lookup(user_id, Args),
    UserId = z_acl:user(Context),
    change(UserId, Context);

event(#submit{message={reminder, _Args}, form="password_reminder"}, Context) ->
    reminder(z_context:get_q_validated("reminder_address", Context), Context);

event(#submit{message={logon_confirm, Args}, form="logon_confirm_form"}, Context) ->
    LogonArgs = [{"username", binary_to_list(m_identity:get_username(Context))}
                  | z_context:get_q_all(Context)],
    case z_notifier:first(#logon_submit{query_args=LogonArgs}, Context) of
        {ok, UserId} when is_integer(UserId) ->
            z_auth:confirm(UserId, Context),
            z_render:wire(proplists:get_all_values(on_success, Args), Context);
        {error, _Reason} ->
            z_render:wire({show, [{target, "logon_confirm_error"}]}, Context);
        undefined ->
            lager:warning("Auth module error: #logon_submit{} returned undefined."),
            z_render:growl_error("Configuration error: please enable a module for #logon_submit{}", Context)
    end;

event(#submit{message={logon, WireArgs}}, Context) ->
    Args = z_context:get_q_all(Context),
    logon(Args, WireArgs, Context);

event(#submit{message={logon_tos_agree, WireArgs}}, Context) ->
    case z_convert:to_bool( z_context:get_q_validated("tos_agree", Context) ) of
        true ->
            logon_tos_agreed(WireArgs, Context);
        false ->
            Context
    end;

event(#z_msg_v1{data=Data}, Context) when is_list(Data) ->
    case proplists:get_value(<<"msg">>, Data) of
        <<"logon_redirect">> ->
            Action = case get_ready_page(proplists:get_value(<<"page">>, Data, []), Context) of
                "#reload" ->
                    [{reload, []}];
                <<"#reload">> ->
                    [{reload, []}];
                Location ->
                    LocalUrl = z_context:site_url(Location, Context),
                    [{redirect, [{location, LocalUrl}]}]
            end,
            z_render:wire(Action, Context);
        Msg ->
            lager:warning("controller_logon: unknown msg: ~p", [Msg]),
            Context
    end.

%%@doc Handle submit data.
logon(Args, Context) ->
    logon(Args, [], Context).

-spec logon(list(), list(), #context{}) -> #context{}.
logon(Args, WireArgs, Context) ->
    case z_notifier:first(#logon_submit{query_args=Args}, Context) of
        {ok, UserId} when is_integer(UserId) ->
            logon_user(UserId, WireArgs, Context);
        {error, ratelimit} ->
            logon_error("ratelimit", Context);
        {error, need_passcode} ->
            logon_error("need_passcode", Context);
        {error, set_passcode} ->
            logon_error("set_passcode", Context);
        {error, set_passcode_error} ->
            logon_error("set_passcode_error", Context);
        {error, passcode} ->
            logon_error("passcode", Context);
        {error, password} ->
            logon_error("pw", Context);
        {error, nouser} ->
            logon_error("pw", Context);
        {error, Reason} when is_atom(Reason) ->
            logon_error(z_convert:to_list(Reason), Context);
        {error, _Reason} ->
            logon_error("pw", Context);
        {expired, UserId} when is_integer(UserId) ->
            case m_identity:get_username(UserId, Context) of
                undefined ->
                    logon_error("pw", Context);
                Username ->
                    Vars = [
                        {user_id, UserId},
                        {secret, set_reminder_secret(UserId, Context)},
                        {username, Username},
                        {need_passcode, has_passcode(Context)}
                    ],
                    logon_stage("password_expired", Vars, Context)
            end;
        undefined ->
            lager:warning("Auth module error: #logon_submit{} returned undefined."),
            logon_error("pw", Context)
    end.

has_passcode(Context) ->
    case z_context:get_q("passcode", Context) of
        undefined -> false;
        "" -> false;
        <<>> -> false;
        _ -> true
    end.

%% @doc Send password reminders to everybody with the given email address
reminder(Email, Context) ->
    EmailNorm = m_identity:normalize_key(email, Email),
    case z_notifier:first( #auth_reset{ username = EmailNorm }, Context) of
        Ok when Ok =:= undefined; Ok =:= ok ->
            case lookup_identities(EmailNorm, Context) of
                [] ->
                    case z_convert:to_bool(m_config:get_value(mod_authentication, email_reminder_if_nomatch, Context)) of
                        true ->
                            send_reminder(undefined, EmailNorm, Context);
                        false ->
                            nop
                    end;
                Identities ->
                    lists:foreach(
                        fun(RscId) ->
                            send_reminder(RscId, EmailNorm, Context)
                        end,
                        Identities)
            end,
            logon_stage("reminder_sent", [{email, EmailNorm}], Context);
        {error, Reason} ->
            logon_stage("reminder_sent", [{error, Reason}, {email, EmailNorm}], Context)
    end.

change(UserId, Context) ->
    case m_identity:get_username(Context) of
        undefined ->
            logon_error("pw", Context);
        Username ->
            case auth_precheck(Username, Context) of
                ok ->
                    change_1(UserId, Username, Context);
                {error, ratelimit} ->
                    logon_error("ratelimit", Context);
                _ ->
                    logon_error("error", Context)
            end
    end.

change_1(UserId, Username, Context) ->
    LogonArgs = [
        {"username", binary_to_list(Username)}
        | z_context:get_q_all_noz(Context)
    ],
    case z_notifier:first(#logon_submit{query_args=LogonArgs}, Context) of
        {ok, UserId} when is_integer(UserId) ->
            Password1 = z_string:trim(z_context:get_q("password_reset1", Context)),
            Password2 = z_string:trim(z_context:get_q("password_reset2", Context)),
            PasswordMinLength = z_convert:to_integer(m_config:get_value(mod_authentication, password_min_length, "6", Context)),

            case {Password1,Password2} of
                {A,_} when length(A) < PasswordMinLength ->
                    logon_error("tooshort", Context);
                {P,P} ->
                    reset_1(UserId, Username, Password1, Context);
                {_,_} ->
                    logon_error("unequal", Context)
            end;
        {error, ratelimit} ->
            logon_error("ratelimit", Context);
        {error, need_passcode} ->
            logon_error("need_passcode", Context);
        {error, passcode} ->
            logon_error("passcode", Context);
        {error, set_passcode} ->
            logon_error("set_passcode", Context);
        {error, set_passcode_error} ->
            logon_error("set_passcode_error", Context);
        {error, _Reason} ->
            logon_error("pw", Context);
        {expired, UserId} when is_integer(UserId) ->
            case m_identity:get_username(UserId, Context) of
                undefined ->
                    logon_error("pw", Context);
                Username ->
                    Vars = [
                        {user_id, UserId},
                        {secret, set_reminder_secret(UserId, Context)},
                        {username, Username},
                        {need_passcode, has_passcode(Context)}
                    ],
                    logon_stage("password_expired", Vars, Context)
            end;
        undefined ->
            logon_error("error", Context)
    end.

reset(Secret, Username, Context) when is_binary(Username) ->
    case auth_precheck(Username, Context) of
        ok ->
            Password1 = z_string:trim(z_context:get_q("password_reset1", Context)),
            Password2 = z_string:trim(z_context:get_q("password_reset2", Context)),
            PasswordMinLength = z_convert:to_integer(m_config:get_value(mod_authentication, password_min_length, "6", Context)),

            case {Password1,Password2} of
                {A,_} when length(A) < PasswordMinLength ->
                    logon_error("tooshort", Context);
                {P,P} ->
                    {ok, UserId} = get_by_reminder_secret(Secret, Context),
                    case m_identity:get_username(UserId, Context) of
                        undefined ->
                            throw({error, "User does not have an username defined."});
                        Username ->
                            reset_1(UserId, Username, Password1, Context)
                    end;
                {_,_} ->
                    logon_error("unequal", Context)
            end;
        {error, ratelimit} ->
            logon_error("ratelimit", Context);
        _ ->
            logon_error("error", Context)
    end.

reset_1(UserId, Username, Password, Context) ->
    case auth_postcheck(UserId, z_context:get_q_all(Context), Context) of
        ok ->
            case m_identity:set_username_pw(UserId, Username, Password, z_acl:sudo(Context)) of
                ok ->
                    ContextLoggedon = logon_user(UserId, [], Context),
                    delete_reminder_secret(UserId, ContextLoggedon),
                    ContextLoggedon;
                {error, password_match} ->
                    logon_error("password_change_match", Context);
                {error, _} ->
                    logon_error("error", Context)
            end;
        {error, need_passcode} ->
            logon_error("need_passcode", Context);
        {error, passcode} ->
            z_notifier:notify_sync(
                #auth_checked{
                    id = UserId,
                    username = Username,
                    is_accepted = false
                },
                Context),
            logon_error("passcode", Context);
        {error, set_passcode} ->
            logon_error("set_passcode", Context);
        {error, set_passcode_error} ->
            logon_error("set_passcode_error", Context);
        _Error ->
            logon_error("error", Context)
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

logon_error(Reason, Context) ->
    Context1 = z_notifier:foldl(#auth_logon_error{reason=Reason}, Context, Context),
    Context2 = z_render:wire({add_class, [{target, "signup_logon_box"}, {class, "z-logon-error"}]}, Context1),
    Context3 = if
        Reason =:= "need_passcode"; Reason =:= "passcode" ->
            z_render:wire([
                {add_class, [{target, "signup_logon_box"}, {class, "z-logon-passcode"}]},
                {remove_class, [{target, "signup_logon_box"}, {class, "z-logon-set-passcode"}]},
                {focus, [{target, "passcode"}]}
            ], Context2);
        Reason =:= "set_passcode"; Reason =:= "set_passcode_error" ->
            z_render:wire([
                {add_class, [{target, "signup_logon_box"}, {class, "z-logon-set-passcode"}]},
                {remove_class, [{target, "signup_logon_box"}, {class, "z-logon-passcode"}]}
            ], Context2);
        true ->
            z_render:wire([
                {remove_class, [{target, "signup_logon_box"}, {class, "z-logon-passcode"}]},
                {remove_class, [{target, "signup_logon_box"}, {class, "z-logon-set-passcode"}]},
                {set_value, [{target, "password"}, {value, ""}]}
            ], Context2)
    end,
    z_render:update("logon_error", z_template:render("_logon_error.tpl", [{reason, Reason}], Context3), Context3).

remove_logon_error(Context) ->
    z_render:wire({remove_class, [{target, "signup_logon_box"}, {class, "z-logon-error"}]}, Context).


logon_stage(Stage, Context) ->
    logon_stage(Stage, [], Context).

logon_stage(Stage, Args, Context) ->
    Context1 = remove_logon_error(Context),
    z_render:update("signup_logon_box", z_template:render("_logon_stage.tpl", [{stage, Stage}|Args], Context1), Context1).


logon_user(UserId, WireArgs, Context) ->
    case is_tos_agreed(UserId, Context) of
        true ->
            case z_auth:logon(UserId, Context) of
                {ok, ContextUser} ->
                    ContextRemember = case z_context:get_q("rememberme", ContextUser, []) of
                        [] -> ContextUser;
                        _ -> set_rememberme_cookie(UserId, ContextUser)
                    end,
                    Actions = get_post_logon_actions(WireArgs, ContextRemember),
                    z_render:wire(Actions, ContextRemember);
                {error, user_not_enabled} ->
                    check_verified(UserId, Context);
                {error, _Reason} ->
                    % Could not log on, some error occured
                    logon_error("unknown", Context)
            end;
        false ->
            Secret = z_convert:to_binary( z_ids:id() ),
            Encoded = termit:encode({user, UserId}, Secret),
            z_context:set_session(logon_tos_secret, Secret, Context),
            TosArgs = [
                {user_id, UserId},
                {secret, Encoded},
                {wire_args, WireArgs}
            ],
            logon_stage("tos_agree", TosArgs, Context)
    end.

logon_tos_agreed(Args, Context) ->
    case try_set_tos_agreed(Args, Context) of
        true ->
            {user_id, UserId} = proplists:lookup(user_id, Args),
            WireArgs = proplists:get_value(wire_args, Args, []),
            logon_user(UserId, WireArgs, Context);
        false ->
            z_render:wire({alert, [
                    {text, ?__("Something went wrong, please retry.", Context)},
                    {action, {reload, []}}
                ]},
                Context)
    end.

try_set_tos_agreed(Args, Context) when is_list(Args) ->
    case z_context:get_session(logon_tos_secret, Context) of
        undefined ->
            lager:error("try_set_tos_agreed: no session secret"),
            false;
        Secret when is_binary(Secret) ->
            {user_id, UserId} = proplists:lookup(user_id, Args),
            case termit:decode(proplists:get_value(secret, Args), Secret) of
                {ok, {user, UserId}} ->
                    z_context:set_session(logon_tos_secret, undefined, Context),
                    {ok, _} = m_rsc:update(
                        UserId,
                        [ {tos_agreed, calendar:universal_time()} ],
                        [ no_touch ],
                        z_acl:sudo(Context)),
                    true;
                {error, _} ->
                    lager:error("try_set_tos_agreed: could not decode payload"),
                    false
            end
    end.

is_tos_agreed(UserId, Context) ->
    case z_convert:to_bool( m_config:get_value(mod_authentication, tos_update_agree, Context) ) of
        true ->
            LastTC = date_tos_agreed(UserId, Context),
            is_tos_document_agreed(LastTC, signup_tos, Context)
            andalso is_tos_document_agreed(LastTC, signup_privacy, Context);
        false ->
            true
    end.

date_tos_agreed(UserId, Context) ->
    case m_rsc:p_no_acl(UserId, tos_agreed, Context) of
        undefined ->
            AgreeOnCreate = case m_config:get_value(mod_authentication, tos_agreed_on_create, Context) of
                undefined -> true;
                V -> z_convert:to_bool(V)
            end,
            case AgreeOnCreate of
                true ->
                    m_rsc:p_no_acl(UserId, created, Context);
                false ->
                    undefined
            end;
        DT ->
            % Protect against dates in the future
            case DT > calendar:universal_time() of
                true ->
                    undefined;
                false ->
                    DT
            end
    end.

is_tos_document_agreed(AgreeDate, DocId, Context) ->
    case m_rsc:p_no_acl(DocId, is_published, Context) of
        undefined -> true;
        false -> true;
        true ->
            case m_rsc:p_no_acl(DocId, publication_start, Context) of
                undefined -> true;
                DT when DT =:= undefined -> false;
                DT when DT > AgreeDate -> false;
                _ -> true
            end
    end.


check_verified(UserId, Context) ->
    case m_rsc:p(UserId, is_verified_account, z_acl:sudo(Context)) of
        false ->
            % The account is awaiting verification
            logon_stage("verification_pending", [{user_id, UserId}], Context);
        V when V == true orelse V == undefined ->
            % The account has been disabled after verification, or
            % verification flag not set, account didn't need verification
            logon_error("unknown", Context)
    end.


%% @doc Check if there is a "rememberme" cookie.  If so then return the user id
%% belonging to the cookie.
get_rememberme_cookie(Context) ->
    Rq = z_context:get_reqdata(Context),
    case wrq:get_cookie_value(?LOGON_REMEMBERME_COOKIE, Rq) of
        undefined ->
            undefined;
        Value ->
            try
                Value1 = mochiweb_util:unquote(Value),
                case z_utils:decode_value_expire(Value1, Context) of
                    {error, expired} ->
                        undefined;
                    {ok, Cookie} ->
                        case check_rememberme_cookie_value(Cookie, Context) of
                            {ok, UserId} ->
                                case z_auth:is_enabled(UserId, Context)
                                    andalso is_tos_agreed(UserId, Context)
                                of
                                    true -> {ok, UserId};
                                    false -> undefined
                                end;
                            {error, expired} ->
                                lager:debug("Expired rememberme cookie value ~p", [Cookie]),
                                undefined;
                            {error, enoent} ->
                                lager:debug("Unknown rememberme cookie value ~p", [Cookie]),
                                undefined;
                            {error, Error} ->
                                lager:warning("Illegal rememberme cookie value ~p (~p)", [Cookie, Error]),
                                undefined
                        end
                end
            catch
                _:_ -> undefined
            end
    end.


%% @doc Set the 'rememberme' cookie.  Let it expire after some days.
set_rememberme_cookie(UserId, Context) ->
    Expire = add_days(?LOGON_REMEMBERME_DAYS, calendar:universal_time()),
    ToEncode = make_rememberme_cookie_value(UserId, Context),
    Value = z_utils:url_encode(z_convert:to_list(z_utils:encode_value_expire(ToEncode, Expire, Context))),
    RD = z_context:get_reqdata(Context),
    Options = [
        {max_age, ?LOGON_REMEMBERME_DAYS*3600*24},
        {path, "/"},
        {http_only, true},
        {same_site, lax},
        {domain, z_context:cookie_domain(Context)}
    ],
    Options1 = case z_convert:to_binary(m_config:get_value(site, protocol, Context)) of
        <<"https">> -> [ {secure, true} | Options ];
        _ -> Options
    end,
    Hdr = mochiweb_cookies:cookie(?LOGON_REMEMBERME_COOKIE, Value, Options1),
    RD1 = wrq:merge_resp_headers([Hdr], RD),
    z_context:set_reqdata(RD1, Context).

    add_days(N, Date) when N =< 0 ->
        Date;
    add_days(N, Date) ->
        add_days(N-1, z_datetime:next_day(Date)).


% @doc Reset the rememberme cookie.
reset_rememberme_cookie(Context) ->
    RD = z_context:get_reqdata(Context),
    Options = [
        {path, "/"},
        {http_only, true},
        {domain, z_context:cookie_domain(Context)}
    ],
    Hdr = mochiweb_cookies:cookie(?LOGON_REMEMBERME_COOKIE, "", Options),
    RD1 = wrq:merge_resp_headers([Hdr], RD),
    z_context:set_reqdata(RD1, Context).


check_rememberme_cookie_value({ok, UserId}, _Context) when is_integer(UserId) ->
    {error, expired};
check_rememberme_cookie_value({ok, {v1, Token}}, Context) ->
    case m_identity:lookup_by_rememberme_token(Token, Context) of
        {ok, UserId} ->
            {ok, UserId};
        {error, _} = Error ->
            Error
    end.

make_rememberme_cookie_value(UserId, Context) ->
    {ok, Token} = m_identity:get_rememberme_token(UserId, Context),
    {ok, {v1, Token}}.

%% @doc Find all users with a certain e-mail address or username.
%%      Filter on found resources having an username.
lookup_identities(undefined, _Context) -> [];
lookup_identities("", _Context) -> [];
lookup_identities(<<>>, _Context) -> [];
lookup_identities(EmailOrUsername, Context) ->
    Es = m_identity:lookup_by_type_and_key_multi(email, EmailOrUsername, Context),
    Us = m_identity:lookup_by_type_and_key_multi(username_pw, EmailOrUsername, Context),
    RscIds = lists:usort([ proplists:get_value(rsc_id, Row) || Row <- Es ++ Us ]),
    lists:filter(
    fun (RscId) ->
        case m_identity:get_username(RscId, Context) of
            undefined -> false;
            <<"admin">> -> false;
            _ -> true
        end
    end,
    RscIds).


%% @doc Exported convenience function to email password reminder to an user
send_reminder(Id, Context) ->
    Email = m_rsc:p_no_acl(Id, email_raw, Context),
    send_reminder(Id, Email, Context).

send_reminder(_Id, undefined, _Context) ->
    {error, noemail};
send_reminder(1, _Email, _Context) ->
    lager:info("Ignoring password reminder request for 'admin' (user 1)"),
    {error, admin};
send_reminder(undefined, Email, Context) ->
    z_email:send_render(Email, "email_password_reset.tpl", [], Context);
send_reminder(Id, Email, Context) ->
    PrefEmail = case m_rsc:p_no_acl(Id, email_raw, Context) of
        undefined -> Email;
        <<>> -> Email;
        E -> m_identity:normalize_key(email, E)
    end,
    case m_identity:get_username(Id, Context) of
        undefined ->
            send_reminder(undefined, Email, Context);
        Username when Username =/= <<"admin">> ->
            Vars = [
                {recipient_id, Id},
                {id, Id},
                {secret, set_reminder_secret(Id, Context)},
                {username, Username},
                {email, PrefEmail}
            ],
            z_email:send_render(Email, "email_password_reset.tpl", Vars, Context),
            case Email of
                PrefEmail -> ok;
                _ -> z_email:send_render(PrefEmail, "email_password_reset.tpl", Vars, Context)
            end
    end.

%% @doc Set the unique reminder code for the account.
set_reminder_secret(Id, Context) ->
    Code = z_ids:id(24),
    ok = m_identity:set_by_type(Id, "logon_reminder_secret", Code, Context),
    Code.

%% @doc Delete the reminder secret of the user
delete_reminder_secret(Id, Context) ->
    m_identity:delete_by_type(Id, "logon_reminder_secret", Context).


get_by_reminder_secret(Code, Context) ->
    MaxAge = case z_convert:to_integer( m_config:get_value(mod_authentication, reset_token_maxage, Context) ) of
        undefined -> ?RESET_TOKEN_MAXAGE;
        MA -> MA
    end,
    case m_identity:lookup_by_type_and_key("logon_reminder_secret", Code, Context) of
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


%% @doc Get actions that should be wired after successful logon
-spec get_post_logon_actions(list(), #context{}) -> list().
get_post_logon_actions(WireArgs, Context) ->
    case z_notifier:foldl(#logon_actions{args=WireArgs}, [], Context) of
        [] ->
            case get_ready_page(Context) of
                "#reload" ->
                    [{reload, []}];
                <<"#reload">> ->
                    [{reload, []}];
                Location ->
                    LocalUrl = z_context:site_url(Location, Context),
                    [{redirect, [{location, LocalUrl}]}]
            end;
        Actions ->
            Actions
    end.

