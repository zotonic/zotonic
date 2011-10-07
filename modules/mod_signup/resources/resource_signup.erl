%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% Date: 2010-05-12
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
    Vars = case z_context:get_q("xs", Context2) of
                undefined ->
                    [];
                Check ->
                    case z_session:get(signup_xs, Context2) of
                        {Check, Props, SignupProps} -> [ {xs_props, {Props,SignupProps}} | Props ];
                        _ -> []
                    end
            end,
    z_session:set(signup_xs, undefined, Context),
    Rendered = z_template:render("signup.tpl", Vars, Context2),
    {Output, OutputContext} = z_context:output(Rendered, Context2),
    ?WM_REPLY(Output, OutputContext).


%% @doc Handle the submit of the signup form.
event({submit, {signup, [{xs_props,Xs}]}, "signup_form", _Target}, Context) ->
    {XsProps0,XsSignupProps} = case Xs of
        {A,B} -> {A,B};
        undefined -> {undefined, undefined}
    end,
    XsProps = case XsProps0 of undefined -> []; _ -> XsProps0 end,
    
    Agree = z_context:get_q_validated("signup_tos_agree", Context),
    case Agree of
        "1" ->
            Email = fetch_prop(email, true, XsProps, Context),
            Props = [
                {name_first, fetch_prop(name_first, true, XsProps, Context)},
                {name_surname_prefix, fetch_prop(name_surname_prefix, false, XsProps, Context)},
                {name_surname, fetch_prop(name_surname, true, XsProps, Context)},
                {email, Email}
            ],
            RequestConfirm = z_convert:to_bool(m_config:get_value(mod_signup, request_confirm, true, Context)),
            SignupProps = case is_set(XsSignupProps) of
                                true ->
                                    XsSignupProps;
                                false ->
                                    [ {identity, {username_pw, 
                                            {z_string:trim(z_context:get_q_validated("username", Context)), 
                                             z_context:get_q_validated("password1", Context)},
                                            true,
                                            true}}
                                    ]
                            end,
            SignupProps1 = case Email of
                [] -> SignupProps;
                <<>> -> SignupProps;
                _ ->  case has_email_identity(Email, SignupProps) of
                        false -> [ {identity, {email, Email, false, false}} | SignupProps ];
                        true -> SignupProps
                      end
            end,
            signup(Props, SignupProps1, RequestConfirm, Context);
        _ ->
            show_errors([error_tos_agree], Context)
    end.


    fetch_prop(Prop, Validated, SignupProps, Context) ->
        case proplists:get_value(Prop, SignupProps) of
            undefined ->
                V = case Validated of
                    true -> z_context:get_q_validated(Prop, Context);
                    false -> z_context:get_q(Prop, Context)
                end,
                V1 = case {V,Prop} of
                    {undefined, name_surname_prefix} -> z_context:get_q("surprefix", Context);
                    _ -> V
                end,
                z_string:trim(z_convert:to_list(V1));
            V -> V
        end.
    
    is_set(undefined) -> false;
    is_set([]) -> false;
    is_set(<<>>) -> false;
    is_set(_) -> true.

    has_email_identity(_Email, []) -> false;
    has_email_identity(Email, [{identity, {email, Email, _, _}}|_]) -> true;
    has_email_identity(Email, [_|Rest]) -> has_email_identity(Email, Rest).


%% @doc Sign up a new user. Check if the identity is available.
signup(Props, SignupProps, RequestConfirm, Context) ->
    case mod_signup:signup(Props, SignupProps, RequestConfirm, Context) of
        {ok, UserId} ->
            case not RequestConfirm orelse m_identity:is_verified(UserId, Context) of
                true ->
                    ensure_published(UserId, z_acl:sudo(Context)),
                    {ok, ContextUser} = z_auth:logon(UserId, Context),
                    Location = case z_convert:to_list(proplists:get_value(ready_page, SignupProps, [])) of
                        [] -> 
                            case z_notifier:first(#signup_confirm_redirect{id=UserId}, ContextUser) of
                                undefined -> m_rsc:p(UserId, page_url, ContextUser);
                                Loc -> Loc
                            end;
                        Url ->
                            Url
                    end,
                    z_render:wire({redirect, [{location, Location}]}, ContextUser);
                false ->
                    % User is not yet verified, send a verification message to the user's external identities
                    case mod_signup:request_verification(UserId, Context) of
                        {error, no_verifiable_identities} ->
                            % Problem, no email address or other identity that could be verified
                            show_errors([error_need_verification], Context);
                        ok ->
                            % Show feedback that we sent a confirmation message
                            Context1 = show_errors([], Context),
                            z_render:wire([ {hide, [{target, "signup_area"}]},
                                            {show, [{target, "signup_verify"}]},
                                            {redirect, [{location, "#signup_verify"}]}], Context1)
                    end
            end;
        {error, {identity_in_use, username}} ->
            show_errors([error_duplicate_username], Context);
        {error, {identity_in_use, _}} ->
            show_errors([error_duplicate_identity], Context);
        {error, #context{} = ContextError} ->
            show_errors([error_signup], ContextError);
        {error, _Reason} ->
            show_errors([error_signup], Context)
    end.


ensure_published(UserId, Context) ->
    case m_rsc:p(UserId, is_published, Context) of
        true -> {ok, UserId};
        false -> m_rsc:update(UserId, [{is_published, true}], Context)
    end.


show_errors(Errors, Context) ->
    Errors1 = [ z_convert:to_list(E) || E <- Errors ],
    z_render:wire({set_class, [{target,"signup_form"}, {class,Errors1}]}, Context).

