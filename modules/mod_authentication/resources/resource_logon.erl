%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @date 2010-05-07
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
-define(LOGON_REMEMBERME_DAYS, 14).


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
    case get_rememberme_cookie(Context1) of
        {ok, UserId} ->
            ContextUser =  z_context:set(user_id, UserId, Context1),
            ?WM_REPLY(false, ContextUser);
        undefined -> 
       		?WM_REPLY(true, Context1)
    end.

previously_existed(ReqData, Context) ->
    {true, ReqData, Context}.

%% @doc Temporary redirect if we have an automatic log on due to a rememberme cookie
moved_temporarily(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = z_context:ensure_all(Context1),
    ContextUser = z_auth:logon(z_context:get(user_id, Context2), Context2),
    Location = z_context:get_q("p", ContextUser, "/"),
    ?WM_REPLY({true, Location}, ContextUser).


provide_content(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = z_context:ensure_all(Context1),
    Context3 = z_context:set_resp_header("X-Robots-Tag", "noindex", Context2),
	IsPasswordReset = z_context:get(is_password_reset, Context3),
    Vars = [
        {page, z_context:get_q("p", Context3)},
		{is_password_reset, IsPasswordReset}
    ],
	Vars1 = case IsPasswordReset of
		true -> 
			Secret = z_context:get_q("secret", Context3),
			case get_by_reminder_secret(Secret, Context3) of
				{ok, UserId} -> 
					[ {user_id, UserId}, 
					  {secret, Secret},
					  {username, m_identity:get_username(UserId, Context3)} | Vars ];
				undefined -> 
					Vars
			end;
		_ ->
			Vars
	end,
    Rendered = z_template:render("logon.tpl", Vars1, Context3),
    {Output, OutputContext} = z_context:output(Rendered, Context3),
    ?WM_REPLY(Output, OutputContext).


%% @doc Handle the submit of the logon form, this will be handed over to the
%% different authentication handlers.
event({submit, [], "logon_password_reset_form", _Target}, Context) ->
	Secret = z_context:get_q("secret", Context),
	Password1 = z_string:trim(z_context:get_q("password_reset1", Context)),
	Password2 = z_string:trim(z_context:get_q("password_reset2", Context)),
	case {Password1,Password2} of
		{A,_} when length(A) < 6 ->
			z_render:wire([
					{remove_class, [{target, "logon_outer"}, {class, "logon_error_password_unequal"}]},
					{add_class, [{target, "logon_outer"}, {class, "logon_error"}]},
					{add_class, [{target, "logon_outer"}, {class, "logon_error_password_tooshort"}]}
					], Context);
		{P,P} ->
			{ok, UserId} = get_by_reminder_secret(Secret, Context),
			case m_identity:get_username(UserId, Context) of
				undefined ->
					throw({error, "User does not have an username defined."});
				Username ->
					delete_reminder_secret(UserId, Context),
					m_identity:set_username_pw(UserId, Username, Password1, Context),
					logon_user(UserId, Context)
			end;
		{_,_} ->
			z_render:wire([
					{remove_class, [{target, "logon_outer"}, {class, "logon_error_password_tooshort"}]},
					{add_class, [{target, "logon_outer"}, {class, "logon_error"}]},
					{add_class, [{target, "logon_outer"}, {class, "logon_error_password_unequal"}]}
					], Context)
	end;
event({submit, [], "logon_reminder_form", _Target}, Context) ->
	case z_string:trim(z_context:get_q("reminder_address", Context, [])) of
		[] ->
			logon_error(Context);
		Reminder ->
			case lookup_identities(Reminder, Context) of
				[] -> 
					logon_error(Context);
				Identities ->
					% @todo TODO check if reminder could be sent (maybe there is no e-mail address)
					?DEBUG(Identities),
					send_reminder(Identities, Context),
					reminder_success(Context)
			end
	end;
event({submit, [], _Trigger, _Target}, Context) ->
    Args = z_context:get_q_all(Context),
    case z_notifier:first({logon_submit, Args}, Context) of
        undefined -> logon_error(Context);
        {error, _Reason} -> logon_error(Context);
        {ok, UserId} when is_integer(UserId) -> logon_user(UserId, Context)
    end.

	logon_error(Context) ->
		z_render:wire({add_class, [{target, "logon_outer"}, {class, "logon_error"}]}, Context).

	remove_logon_error(Context) ->
		z_render:wire({remove_class, [{target, "logon_outer"}, {class, "logon_error"}]}, Context).
	
	reminder_success(Context) ->
		Context1 = remove_logon_error(Context),
		z_render:wire({add_class, [{target, "logon_outer"}, {class, "logon_reminder_sent"}]}, Context1).

logon_user(UserId, Context) ->
    ContextUser = z_auth:logon(UserId, Context),
    ContextRemember = case z_context:get_q("rememberme", Context, []) of
        [] -> ContextUser;
        _ -> set_rememberme_cookie(UserId, ContextUser)
    end,
    case z_context:get_q("page", ContextRemember, []) of
        [] ->  z_render:wire({redirect, [{location, "/"}]}, ContextRemember);
        Url -> z_render:wire({redirect, [{location, Url}]}, ContextRemember)
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
                        {ok, UserId}
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
    Options = [{max_age, ?LOGON_REMEMBERME_DAYS*3600*24}, {path, "/"}, {http_only, true}],
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
    Options = [{path, "/"}, {http_only, true}],
    Hdr = mochiweb_cookies:cookie(?LOGON_REMEMBERME_COOKIE, "", Options),
    RD1 = wrq:merge_resp_headers([Hdr], RD),
    z_context:set_reqdata(RD1, Context).


% @doc Find all identities with the given handle.  The handle is either an e-mail address or an username.
lookup_identities(Handle, Context) ->
	lookup_by_username(Handle, Context) ++ lookup_by_email(Handle, Context).

lookup_by_username(Handle, Context) ->
	case m_identity:lookup_by_username(Handle, Context) of
		undefined -> [];
		Row -> [ proplists:get_value(rsc_id, Row) ]
	end.


%% @doc Find all users with a certain e-mail address
%% @todo TODO
lookup_by_email(Handle, Context) ->
	[].


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
	?DEBUG({Email, Vars}),
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



