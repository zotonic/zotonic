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


-include_lib("controller_webmachine_helper.hrl").
-include_lib("include/zotonic.hrl").

-define(LOGON_REMEMBERME_COOKIE, "z_logon").
-define(LOGON_REMEMBERME_DAYS, 365).


init(DispatchArgs) -> {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:set(DispatchArgs, Context),
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
    Location = z_context:abs_url(cleanup_url(get_page(Context1)), Context1),
    ?WM_REPLY({true, Location}, Context1).


provide_content(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = z_context:set_resp_header("X-Robots-Tag", "noindex", Context1),
    Vars = [
        {noindex, true},
        {notrack, true},
        {page, get_page(Context2)}
        | z_context:get_all(Context2)
    ],
    Secret = z_context:get_q("secret", Context2),
    Username = z_context:get_q("u", Context2),
    {SecretVars, Context3} = reminder_secrets(Secret, Username, Context2),
    Vars1 = SecretVars ++ Vars,
    ErrorUId = z_context:get_q("error_uid", Context3),
    ContextVerify = case ErrorUId /= undefined andalso z_utils:only_digits(ErrorUId) of
                        false -> Context3;
                        true -> check_verified(list_to_integer(ErrorUId), Context3)
                    end,
    Template = z_context:get(template, ContextVerify, "logon.tpl"),
    Rendered = z_template:render(Template, Vars1, ContextVerify),
    {Output, OutputContext} = z_context:output(Rendered, ContextVerify),
    ?WM_REPLY(Output, OutputContext).


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
                        {ok, UsernameB} ->
                            Vs = [
                                {user_id, UserId},
                                {secret, Secret},
                                {username, UsernameB}
                            ],
                            {Vs, Context};
                        {ok, OtherUsername} ->
                            lager:error("Password reset with username mismatch: got \"~s\", expected \"~s\"",
                                        [ UsernameB, OtherUsername ]),
                            {[], Context};
                        {error, _} ->
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


%% @doc Get the page we should redirect to after a successful log on.
%%      This location will be stored in the logon form ("page" on submit).
get_page(Context) ->
    HasBackArg = z_convert:to_bool(z_context:get_q("back", Context)),
    case z_context:get_q("p", Context, []) of
        [] when HasBackArg ->
            RD = z_context:get_reqdata(Context),
            case wrq:get_req_header("referer", RD) of
                undefined -> [];
                Referrer -> z_html:noscript(Referrer)
            end;
        Other ->
            Other
    end.

%% @doc User logged on, fetch the location of the next page to show
get_ready_page(Context) ->
    get_ready_page(z_context:get_q("page", Context, []), Context).

get_ready_page(undefined, Context) ->
    get_ready_page([], Context);
get_ready_page(Page, Context) when is_binary(Page) ->
    get_ready_page(z_convert:to_list(Page), Context);
get_ready_page(Page, Context) when is_list(Page) ->
    case z_notifier:first(#logon_ready_page{request_page=Page}, Context) of
        undefined -> Page;
        Url -> Url
    end.


cleanup_url(undefined) -> "/";
cleanup_url([]) -> "/";
cleanup_url(Url) -> z_html:noscript(Url).


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

%%@doc Handle submit form post.
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

%%@doc Handle submit form post.
event(#submit{message={logon, _Args}}, Context) ->
    Args = z_context:get_q_all(Context),
    logon(Args, Context);

event(#z_msg_v1{data=Data}, Context) when is_list(Data) ->
    case proplists:get_value(<<"msg">>, Data) of
        <<"logon_redirect">> ->
            Location = get_ready_page(proplists:get_value(<<"page">>, Data, []), Context),
            z_render:wire({redirect, [{location, cleanup_url(Location)}]}, Context);
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
        {error, _Reason} ->
            logon_error("pw", Context);
        {expired, UserId} when is_integer(UserId) ->
            {ok, Username} = m_identity:get_username(UserId, Context),
            Vars = [
                {user_id, UserId},
                {secret, set_reminder_secret(UserId, Context)},
                {username, Username}
            ],
            logon_stage("password_expired", Vars, Context);
        undefined ->
            lager:warning("Auth module error: #logon_submit{} returned undefined."),
            logon_error("pw", Context)
    end.

%% @doc Send password reminders to everybody with the given email address
reminder(Email, Context) ->
    EmailNorm = m_identity:normalize_key(email, Email),
    case z_notifier:first( #auth_reset{ username = EmailNorm }, Context) of
        Ok when Ok =:= undefined; Ok =:= ok ->
            case lookup_identities(EmailNorm, Context) of
                [] ->
                    send_reminder(undefined, EmailNorm, Context);
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

reset(Secret, Username, Context) ->
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
                    ContextLoggedon = logon_user(UserId, [], Context),
                    m_identity:set_username_pw(UserId, Username, Password1, z_acl:sudo(ContextLoggedon)),
                    delete_reminder_secret(UserId, ContextLoggedon),
                    ContextLoggedon
            end;
        {_,_} ->
            logon_error("unequal", Context)
    end.


logon_error(Reason, Context) ->
    Context1 = z_notifier:foldl(#auth_logon_error{reason=Reason}, Context, Context),
    Context2 = z_render:set_value("password", "", Context1),
    Context3 = z_render:wire({add_class, [{target, "signup_logon_box"}, {class, "z-logon-error"}]}, Context2),
    z_render:update("logon_error", z_template:render("_logon_error.tpl", [{reason, Reason}], Context3), Context3).


remove_logon_error(Context) ->
    z_render:wire({remove_class, [{target, "signup_logon_box"}, {class, "z-logon-error"}]}, Context).


logon_stage(Stage, Context) ->
    logon_stage(Stage, [], Context).

logon_stage(Stage, Args, Context) ->
    Context1 = remove_logon_error(Context),
    z_render:update("signup_logon_box", z_template:render("_logon_stage.tpl", [{stage, Stage}|Args], Context1), Context1).


logon_user(UserId, WireArgs, Context) ->
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
                                case z_auth:is_enabled(UserId, Context) of
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
        {domain, z_context:cookie_domain(Context)}
    ],
    Hdr = mochiweb_cookies:cookie(?LOGON_REMEMBERME_COOKIE, Value, Options),
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

%% @doc Find all users with a certain e-mail address
lookup_identities(undefined, _Context) -> [];
lookup_identities("", _Context) -> [];
lookup_identities(<<>>, _Context) -> [];
lookup_identities(Email, Context) ->
    Rows = m_identity:lookup_by_type_and_key_multi(email, Email, Context),
    lists:usort([ proplists:get_value(rsc_id, Row) || Row <- Rows ]).


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
    Code = z_ids:id(),
    m_identity:set_by_type(Id, "logon_reminder_secret", Code, Context),
    Code.

%% @doc Delete the reminder secret of the user
delete_reminder_secret(Id, Context) ->
    m_identity:delete_by_type(Id, "logon_reminder_secret", Context).


get_by_reminder_secret(Code, Context) ->
    case m_identity:lookup_by_type_and_key("logon_reminder_secret", Code, Context) of
        undefined -> undefined;
        Row -> {ok, proplists:get_value(rsc_id, Row)}
    end.


%% @doc Get actions that should be wired after successful logon
-spec get_post_logon_actions(list(), #context{}) -> list().
get_post_logon_actions(WireArgs, Context) ->
    case z_notifier:foldl(#logon_actions{args=WireArgs}, [], Context) of
        [] ->
            case cleanup_url(get_ready_page(Context)) of
                "#reload" -> [{reload, []}];
                Location -> [{redirect, [{location, Location}]}]
            end;
        Actions ->
            Actions
    end.

