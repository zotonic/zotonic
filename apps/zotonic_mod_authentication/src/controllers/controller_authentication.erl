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
handle_cmd(<<"logoff">>, Payload, Context) ->
    logoff(Payload, Context);
handle_cmd(<<"refresh">>, Payload, Context) ->
    refresh(Payload, Context);
handle_cmd(<<"setautologon">>, Payload, Context) ->
    setautologon(Payload, Context);
handle_cmd(<<"status">>, Payload, Context) ->
    status(Payload, Context);
handle_cmd(_Cmd, _Payload, Context) ->
    {
        #{
            status => error,
            error => <<"unknown_cmd">>,
            message => <<"Unknown cmd, use one of 'logon', 'logoff', 'refresh', 'setautologon' or 'status'">>
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
            Context2 = z_authentication_tokens:set_auth_cookie(UserId, Context1),
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
            end
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
    lager:warning("Auth module error: #logon_submit{} returned undefined."),
    { #{ status => error, error => pw }, Context }.



%% @doc Remove authentication cookie(s), signal user logoff
-spec logoff( map(), z:context() ) -> { map(), z:context() }.
logoff(Payload, Context) ->
    Context1 = z_auth:logoff(Context),
    Context2 = z_authentication_tokens:reset_cookies(Context1),
    status(Payload, Context2).

%% @doc Refresh the current authentication cookie
-spec refresh( map(), z:context() ) -> { map(), z:context() }.
refresh(Payload, Context) ->
    Context1 = z_authentication_tokens:refresh_auth_cookie(Context),
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
        }
    },
    Status1 = case z_auth:is_auth(Context1) of
        true ->
            Status#{ expires => z_context:get(auth_expires, Context1) };
        false ->
            Status
    end,
    { Status1, Context1 }.


