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
    Vars = [
        {page, z_context:get_q("p", Context3)}
    ],
    Rendered = z_template:render("logon.tpl", Vars, Context3),
    {Output, OutputContext} = z_context:output(Rendered, Context3),
    ?WM_REPLY(Output, OutputContext).


%% @doc Handle the submit of the logon form, this will be handed over to the
%% different authentication handlers.
event({submit, [], _Trigger, _Target}, Context) ->
    Args = z_context:get_q_all(Context),
    case z_notifier:first({logon_submit, Args}, Context) of
        undefined -> z_render:wire({fade_in, [{target, "error"}]}, Context);
        {error, _Reason} -> z_render:wire({fade_in, [{target, "error"}]}, Context);
        {ok, UserId} when is_integer(UserId) -> logon_user(UserId, Context)
    end.


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
    Path = lists:flatten(z_dispatcher:url_for(logon, Context)),
    Options = [{path, Path}, {http_only, true}],
    Hdr = mochiweb_cookies:cookie(?LOGON_REMEMBERME_COOKIE, "", Options),
    RD1 = wrq:merge_resp_headers([Hdr], RD),
    z_context:set_reqdata(RD1, Context).

