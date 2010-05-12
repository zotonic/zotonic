%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @date 2010-05-12
%% @doc Display a form to sign up.

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

-module(resource_signup).
-author("Marc Worrell <marc@worrell.nl>").

-export([init/1, service_available/2, charsets_provided/2, content_types_provided/2]).
-export([provide_content/2]).
-export([event/2]).

-include_lib("webmachine_resource.hrl").
-include_lib("include/zotonic.hrl").


init(DispatchArgs) -> {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:set(DispatchArgs, Context),
    ?WM_REPLY(true, Context1).

charsets_provided(ReqData, Context) ->
    {[{"utf-8", fun(X) -> X end}], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"text/html", provide_content}], ReqData, Context}.


provide_content(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = z_context:ensure_all(Context1),
    Vars = [
    ],
    Rendered = z_template:render("signup.tpl", Vars, Context2),
    {Output, OutputContext} = z_context:output(Rendered, Context2),
    ?WM_REPLY(Output, OutputContext).


%% @doc Handle the submit of the signup form.
event({submit, [], "signup_form", _Target}, Context) ->
    ?DEBUG(z_context:get_q_all(Context)),
    Agree = z_context:get_q_validated("signup_tos_agree", Context),
    case Agree of
        "1" ->
            Email = z_string:trim(z_context:get_q_validated("email", Context)),
            Props = [
                {name_first, z_string:trim(z_context:get_q_validated("name_first", Context))},
                {name_surname_prefix, z_string:trim(z_context:get_q("surprefix", Context, ""))},
                {name_surname, z_string:trim(z_context:get_q_validated("name_surname", Context))},
                {email, Email}
            ],
            IsVerified = not z_convert:to_bool(m_config:get_value(mod_signup, request_confirm, false, Context)),
            SignupProps = [
                {identity, {username, 
                            {z_context:get_q_validated("username", Context), z_context:get_q_validated("password1", Context)},
                            true,
                            IsVerified}}
            ],
            SignupProps1 = case Email of
                [] -> SignupProps;
                <<>> -> SignupProps;
                _ ->  [ {identity, {email, Email, false, false}} | SignupProps ]
            end,
            signup(Props, SignupProps1, Context);
        _ ->
            show_errors([error_tos_agree], Context)
    end.


%% @doc Sign up a new user. Check if the identity is available.
signup(Props, SignupProps, Context) ->
    case mod_signup:signup(Props, SignupProps, Context) of
        {ok, UserId} ->
            % @todo when user is not yet confirmed, do not log on as user.
            ContextUser = z_auth:logon(UserId, Context),
            Location = case z_convert:to_list(proplists:get_value(ready_page, SignupProps, [])) of
                [] -> m_rsc:p(UserId, page_url, ContextUser);
                Url -> Url
            end,
            z_render:wire({redirect, [{location, Location}]}, ContextUser);
        {error, {identity_in_use, username}} ->
            show_errors([error_duplicate_username], Context);
        {error, {identity_in_use, _}} ->
            show_errors([error_duplicate_identity], Context);
        {error, Reason} ->
            show_errors([error_signup], Context)
    end.
    

show_errors(Errors, Context) ->
    Errors1 = [ z_convert:to_list(E) || E <- Errors ],
    z_render:wire({set_class, [{target,"signup_form"}, {class,Errors1}]}, Context).

