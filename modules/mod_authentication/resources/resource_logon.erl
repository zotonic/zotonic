%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% Date: 2010-05-07
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

-module(resource_logon).
-author("Marc Worrell <marc@worrell.nl>").

-export([init/1, service_available/2, charsets_provided/2, content_types_provided/2]).
-export([resource_exists/2, previously_existed/2, moved_temporarily/2]).
-export([provide_content/2]).
-export([event/2]).
-export([get_rememberme_cookie/1, reset_rememberme_cookie/1]).

-include_lib("webmachine_resource.hrl").
-include_lib("include/zotonic.hrl").

-define(LOGON_REMEMBERME_COOKIE, "z_logon").
-define(LOGON_REMEMBERME_DAYS, 3650).


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
    case z_auth:is_auth(Context2) of
        true ->
            case z_context:get_q("p", Context2, []) of
                [] -> ?WM_REPLY(false, Context2);
                _P -> ?WM_REPLY(true, Context2)
            end;
        false ->
            case get_rememberme_cookie(Context2) of
                {ok, UserId} ->
                    Context3 = z_context:set(user_id, UserId, Context2),
                    case z_auth:logon(UserId, Context3) of
                        {ok, ContextUser} -> ?WM_REPLY(false, ContextUser);
                        {error, _Reason} -> ?WM_REPLY(true, Context3)
                    end;
                undefined ->
                    ?WM_REPLY(true, Context2)
            end
    end.


previously_existed(ReqData, Context) ->
    {true, ReqData, Context}.

%% @doc Temporary redirect if we have an automatic log on due to a rememberme cookie or if
%% the user was already logged on and we don't have a redirect page.
moved_temporarily(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = z_context:ensure_all(Context1),
    Location = z_context:abs_url(cleanup_url(get_page(Context2)), Context2),
    ?WM_REPLY({true, Location}, Context2).


provide_content(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = z_context:ensure_all(Context1),
    Context3 = z_context:set_resp_header("X-Robots-Tag", "noindex", Context2),
    Secret = z_context:get_q("secret", Context3),
    Vars = [{page, get_page(Context3)}],
    Vars1 = case get_by_reminder_secret(Secret, Context3) of
                {ok, UserId} -> 
                    [ {user_id, UserId}, 
                      {secret, Secret},
                      {username, m_identity:get_username(UserId, Context3)} | Vars ];
                undefined -> 
                    Vars
            end,
    ErrorUId = z_context:get_q("error_uid", Context3),
    ContextVerify = case ErrorUId /= undefined andalso z_utils:only_digits(ErrorUId) of
                        false -> Context3;
                        true -> check_verified(list_to_integer(ErrorUId), Context3)
                    end,
    Template = z_context:get(template, ContextVerify, "logon.tpl"),
    Rendered = z_template:render(Template, Vars1, ContextVerify),
    {Output, OutputContext} = z_context:output(Rendered, ContextVerify),
    ?WM_REPLY(Output, OutputContext).


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
    Page = z_context:get_q("page", Context, []),
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

event(#submit{message=[], form="password_expired"}=S, Context) ->
    event(S#submit{form="password_reset"}, Context);

event(#submit{message=[], form="password_reset"}, Context) ->
    Secret = z_context:get_q("secret", Context),
    Password1 = z_string:trim(z_context:get_q("password_reset1", Context)),
    Password2 = z_string:trim(z_context:get_q("password_reset2", Context)),
    case {Password1,Password2} of
        {A,_} when length(A) < 6 ->
            logon_error("tooshort", Context);
        {P,P} ->
            {ok, UserId} = get_by_reminder_secret(Secret, Context),
            case m_identity:get_username(UserId, Context) of
                undefined ->
                    throw({error, "User does not have an username defined."});
                Username ->
                    ContextLoggedon = logon_user(UserId, Context),
                    delete_reminder_secret(UserId, ContextLoggedon),
                    m_identity:set_username_pw(UserId, Username, Password1, ContextLoggedon),
                    ContextLoggedon
            end;
        {_,_} ->
            logon_error("unequal", Context)
    end;

event(#submit{message=[], form="password_reminder"}, Context) ->
	case z_string:trim(z_context:get_q("reminder_address", Context, [])) of
		[] ->
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
	end;

event(#submit{message={logon_confirm, Args}, form="logon_confirm_form"}, Context) ->
    LogonArgs = [{"username", binary_to_list(m_identity:get_username(Context))}
                  | z_context:get_q_all(Context)],
    case z_notifier:first(#logon_submit{query_args=LogonArgs}, Context) of
        {error, _Reason} ->
            z_render:wire({show, [{target, "logon_confirm_error"}]}, Context);
        {ok, UserId} when is_integer(UserId) ->
            z_auth:confirm(UserId, Context),
            z_render:wire(proplists:get_all_values(on_success, Args), Context);
        Other -> ?DEBUG(Other)
    end;

event(#submit{message=[]}, Context) ->
    Args = z_context:get_q_all(Context),
    case z_notifier:first(#logon_submit{query_args=Args}, Context) of
        undefined -> logon_error("pwd", Context); % No handler for posted args
        {error, _Reason} -> logon_error("pw", Context);
        {expired, UserId} when is_integer(UserId) ->
            logon_stage("password_expired", [{user_id, UserId}, {secret, set_reminder_secret(UserId, Context)}], Context);
        {ok, UserId} when is_integer(UserId) -> logon_user(UserId, Context)
    end.

logon_error(Reason, Context) ->
    Context1 = z_render:set_value("password", "", Context),
    Context2 = z_render:wire({add_class, [{target, "logon_box"}, {class, "logon_error"}]}, Context1),
    z_render:update("logon_error", z_template:render("_logon_error.tpl", [{reason, Reason}], Context2), Context2).


remove_logon_error(Context) ->
    z_render:wire({remove_class, [{target, "logon_box"}, {class, "logon_error"}]}, Context).


logon_stage(Stage, Context) ->
    logon_stage(Stage, [], Context).

logon_stage(Stage, Args, Context) ->
    Context1 = remove_logon_error(Context),
    z_render:update("logon_form", z_template:render("_logon_stage.tpl", [{stage, Stage}|Args], Context1), Context1).
    

logon_user(UserId, Context) ->
    case z_auth:logon(UserId, Context) of
		{ok, ContextUser} ->
		    ContextRemember = case z_context:get_q("rememberme", ContextUser, []) of
		        [] -> ContextUser;
		        _ -> set_rememberme_cookie(UserId, ContextUser)
		    end,
		    z_render:wire({redirect, [{location, cleanup_url(get_ready_page(ContextRemember))}]}, ContextRemember);
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
                    {ok, UserId} when is_integer(UserId) ->
						case z_auth:is_enabled(UserId, Context) of
							true -> {ok, UserId};
							false -> undefined
						end
                end
            catch
                _:_ -> undefined
            end
    end.


%% @doc Set the 'rememberme' cookie.  Let it expire after some days.
set_rememberme_cookie(UserId, Context) ->
    Expire = add_days(?LOGON_REMEMBERME_DAYS, calendar:local_time()),
    Value = z_utils:url_encode(z_convert:to_list(z_utils:encode_value_expire(UserId, Expire, Context))),
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


% @doc Find all identities with the given handle.  The handle is either an e-mail address or an username.
lookup_identities(Handle, Context) ->
    Handle1 = z_string:trim(Handle),
	lookup_by_username(Handle1, Context) ++ lookup_by_email(Handle1, Context).

lookup_by_username(Handle, Context) ->
	case m_identity:lookup_by_username(Handle, Context) of
		undefined -> [];
		Row -> [ proplists:get_value(rsc_id, Row) ]
	end.


%% @doc Find all users with a certain e-mail address
lookup_by_email(Handle, Context) ->
    case lists:member($@, Handle) of
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
		[] -> send_reminder(Ids, Context, Acc);
		Email -> 
			Vars = [
			    {recipient_id, Id},
				{id, Id},
				{secret, set_reminder_secret(Id, Context)},
				{username, m_identity:get_username(Id, Context)},
				{email, Email}
			],
			send_email(Email, Vars, Context)
	end.


%% @doc Find all e-mail addresses of an user.
find_email(Id, Context) ->
	case m_rsc:p(Id, email, Context) of
		undefined -> [];
		Email -> [Email]
	end.

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



