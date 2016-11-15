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

-export([charsets_provided/1]).
-export([resource_exists/1, previously_existed/1, moved_temporarily/1]).
-export([to_html/1]).
-export([event/2]).
-export([get_rememberme_cookie/1, set_rememberme_cookie/2, reset_rememberme_cookie/1]).

%% Convenience export for other modules.
-export([logon/2, logon/3, reminder/2, expired/2, reset/2]).

%% Convenience export for other auth implementations.
-export([send_reminder/2, lookup_identities/2]).

-include_lib("include/zotonic.hrl").

-define(LOGON_REMEMBERME_COOKIE, <<"z_logon">>).
-define(LOGON_REMEMBERME_DAYS, 365).

charsets_provided(Context) ->
    {[<<"utf-8">>], Context}.

resource_exists(Context) ->
    Context2 = z_context:ensure_all(Context),
    case z_convert:to_bool(z_context:get(is_only_anonymous, Context2)) of
        true -> check_auth(Context2);
        false -> {true, Context2}
    end.

check_auth(Context) ->
    case z_auth:is_auth(Context) of
        true ->
            case z_utils:is_empty(z_context:get_q(<<"p">>, Context)) of
                true -> {false, Context};
                false -> {true, Context}
            end;
        false ->
            case get_rememberme_cookie(Context) of
                {ok, UserId} ->
                    Context1 = z_context:set(user_id, UserId, Context),
                    case z_auth:logon(UserId, Context1) of
                        {ok, ContextUser} -> {false, ContextUser};
                        {error, _Reason} -> {true, Context1}
                    end;
                undefined ->
                    {true, Context}
            end
    end.


previously_existed(Context) ->
    {true, Context}.

%% @doc Temporary redirect if we have an automatic log on due to a rememberme cookie or if
%% the user was already logged on and we don't have a redirect page.
moved_temporarily(Context) ->
    Location = z_context:abs_url(cleanup_url(get_page(Context)), Context),
    {{true, Location}, Context}.


to_html(Context) ->
    Context2 = z_context:set_resp_header(<<"x-robots-tag">>, <<"noindex">>, Context),
    Secret = z_context:get_q(<<"secret">>, Context2),
    Vars = [
        {page, get_page(Context2)}
        | z_context:get_all(Context2)
    ],
    Vars1 = case get_by_reminder_secret(Secret, Context2) of
                {ok, UserId} ->
                    [ {user_id, UserId},
                      {secret, Secret},
                      {username, m_identity:get_username(UserId, Context2)}
                      | Vars ];
                undefined ->
                    Vars
            end,
    ErrorUId = z_context:get_q(<<"error_uid">>, Context2),
    ContextVerify = case ErrorUId /= undefined andalso z_utils:only_digits(ErrorUId) of
                        false -> Context2;
                        true -> check_verified(z_convert:to_integer(ErrorUId), Context2)
                    end,
    Template = z_context:get(template, ContextVerify, "logon.tpl"),
    Rendered = z_template:render(Template, Vars1, ContextVerify),
    z_context:output(Rendered, ContextVerify).


%% @doc Get the page we should redirect to after a successful log on.
%%      This location will be stored in the logon form ("page" on submit).
get_page(Context) ->
    HasBackArg = z_convert:to_bool(z_context:get_q(<<"back">>, Context)),
    case z_context:get_q(<<"p">>, Context, <<>>) of
        <<>> when HasBackArg ->
            case cowmachine_req:get_req_header(<<"referer">>, Context) of
                undefined -> [];
                Referrer -> z_html:noscript(Referrer)
            end;
        Other ->
            Other
    end.

%% @doc User logged on, fetch the location of the next page to show
get_ready_page(Context) ->
    get_ready_page(z_context:get_q(<<"page">>, Context, <<>>), Context).

get_ready_page(undefined, Context) ->
    get_ready_page(<<>>, Context);
get_ready_page(Page, Context) when is_binary(Page) ->
    case z_notifier:first(#logon_ready_page{request_page=Page}, Context) of
        undefined -> Page;
        Url -> Url
    end.


cleanup_url(undefined) -> <<"/">>;
cleanup_url(<<>>) -> <<"/">>;
cleanup_url(Url) -> z_html:noscript(Url).


%% @doc Handle the submit of the logon form, this will be handed over to the
%% different authentication handlers.

event(#postback{message={send_verification, [{user_id, UserId}]}}, Context) ->
    case z_notifier:first(#identity_verification{user_id=UserId}, Context) of
        ok -> logon_stage("verification_sent", Context);
        _Other -> logon_stage("verification_error", Context)
    end;

event(#submit{message= <<>>, form= <<"password_expired">>}, Context) ->
    Args = z_context:get_q_all(Context),
    expired(Args, Context);

event(#submit{message= <<>>, form= <<"password_reset">>}, Context) ->
    Args = z_context:get_q_all(Context),
    reset(Args, Context);

%%@doc Handle submit form post.
event(#submit{message= <<>>, form= <<"password_reminder">>}, Context) ->
    Args = z_context:get_q_all(Context),
    reminder(Args, Context);

event(#submit{message={logon_confirm, Args}, form= <<"logon_confirm_form">>}, Context) ->
    LogonArgs = [{<<"username">>, m_identity:get_username(Context)}
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
event(#submit{message= <<>>}, Context) ->
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
        {error, _Reason} ->
            logon_error("pw", Context);
        {expired, UserId} when is_integer(UserId) ->
            logon_stage("password_expired", [{user_id, UserId}, {secret, set_reminder_secret(UserId, Context)}], Context);
        undefined ->
            lager:warning("Auth module error: #logon_submit{} returned undefined."),
            logon_error("pw", Context)
    end.

%%@doc Handle submit data.
reminder(Args, Context) ->
    case z_string:trim(proplists:get_value(<<"reminder_address">>, Args, <<>>)) of
        <<>> ->
            logon_error("reminder", Context);
        Reminder ->
            case lookup_identities(Reminder, Context) of
                [] ->
                    logon_error("reminder", Context);
                Identities ->
                    % @todo TODO check if reminder could be sent (maybe there is no e-mail address)
                    send_reminder(Identities, Context),
                    logon_stage("reminder_sent", Context)
            end
    end.

expired(Args, Context) ->
    reset(Args, Context).

reset(Args, Context) ->
    Secret = proplists:get_value(<<"secret">>, Args),
    Password1 = z_string:trim(proplists:get_value(<<"password_reset1">>, Args)),
    Password2 = z_string:trim(proplists:get_value(<<"password_reset2">>, Args)),
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
                    delete_reminder_secret(UserId, ContextLoggedon),
                    m_identity:set_username_pw(UserId, Username, Password1, ContextLoggedon),
                    ContextLoggedon
            end;
        {_,_} ->
            logon_error("unequal", Context)
    end.


logon_error(Reason, Context) ->
    Context1 = z_render:set_value("password", "", Context),
    Context2 = z_render:wire({add_class, [{target, "signup_logon_box"}, {class, "z-logon-error"}]}, Context1),
    z_render:update("logon_error", z_template:render("_logon_error.tpl", [{reason, Reason}], Context2), Context2).


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
            ContextRemember = case z_context:get_q(<<"rememberme">>, ContextUser, <<>>) of
                <<>> -> ContextUser;
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
    case m_rsc:p_no_acl(UserId, is_verified_account, Context) of
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
    case cowmachine_req:get_cookie_value(?LOGON_REMEMBERME_COOKIE, Context) of
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
    Value = z_url:url_encode(z_convert:to_list(z_utils:encode_value_expire(ToEncode, Expire, Context))),
    Options = [
        {max_age, ?LOGON_REMEMBERME_DAYS*3600*24},
        {path, <<"/">>},
        {http_only, true},
        {domain, z_context:cookie_domain(Context)}
    ],
    z_context:set_cookie(?LOGON_REMEMBERME_COOKIE, z_convert:to_binary(Value), Options, Context).

add_days(N, Date) when N =< 0 ->
    Date;
add_days(N, Date) ->
    add_days(N-1, z_datetime:next_day(Date)).


% @doc Reset the rememberme cookie.
reset_rememberme_cookie(Context) ->
    Options = [
        {path, <<"/">>},
        {http_only, true},
        {domain, z_context:cookie_domain(Context)}
    ],
    z_context:set_cookie(?LOGON_REMEMBERME_COOKIE, <<>>, Options, Context).


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

% @doc Find all identities with the given handle.  The handle is either an e-mail address or an username.
lookup_identities(Handle, Context) ->
    Handle1 = z_string:trim(Handle),
    Set = sets:from_list(lookup_by_username(Handle1, Context) ++ lookup_by_email(Handle1, Context)),
    sets:to_list(Set).


lookup_by_username(<<"admin">>, _Context) ->
    [];
lookup_by_username(Handle, Context) ->
    case m_identity:lookup_by_username(Handle, Context) of
        undefined -> [];
        Row -> [ proplists:get_value(rsc_id, Row) ]
    end.


%% @doc Find all users with a certain e-mail address
lookup_by_email(Handle, Context) ->
    case z_email_utils:is_email(Handle) of
        true ->
            Rows = m_identity:lookup_by_type_and_key_multi(email, Handle, Context),
            [ proplists:get_value(rsc_id, Row) || Row <- Rows ];
        false ->
            []
    end.


%% Send an e-mail reminder to the listed ids.
send_reminder(Ids, Context) ->
    case send_reminder(Ids, z_acl:sudo(Context), []) of
        [] -> {error, no_email};
        _ -> ok
    end.

send_reminder([], _Context, Acc) ->
    Acc;
send_reminder([Id|Ids], Context, Acc) ->
    case find_email(Id, Context) of
        undefined ->
            send_reminder(Ids, Context, Acc);
        Email ->
            case m_identity:get_username(Id, Context) of
                undefined ->
                    send_reminder(Ids, Context, Acc);
                <<"admin">> ->
                    send_reminder(Ids, Context, Acc);
                Username ->
                    Vars = [
                        {recipient_id, Id},
                        {id, Id},
                        {secret, set_reminder_secret(Id, Context)},
                        {username, Username},
                        {email, Email}
                    ],
                    send_email(Email, Vars, Context),
                    send_reminder(Ids, Context, [Id|Acc])
            end
    end.


%% @doc Find the preferred e-mail address of an user.
find_email(Id, Context) ->
    m_rsc:p_no_acl(Id, email_raw, Context).

%% @doc Sent the reminder e-mail to the user.
send_email(Email, Vars, Context) ->
    z_email:send_render(Email, "email_password_reset.tpl", Vars, Context),
    ok.


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

