%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2026 Marc Worrell
%% @doc Display a form to sign up.
%% @end

%% Copyright 2010-2026 Marc Worrell
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

-module(controller_signup).
-moduledoc("
Controller which displays a form to sign up (rendered from `signup.tpl`).

It also implements the neccesary postbacks to perform the signup and log a user in.

Todo

Extend documentation
").
-author("Marc Worrell <marc@worrell.nl>").

-export([
    process/4
]).
-export([event/2]).

-include_lib("zotonic_core/include/zotonic.hrl").


process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
    Context2 = z_context:ensure_qs(Context),
    z_context:logger_md(Context2),
    Vars = case z_context:get_q(<<"xs">>, Context2) of
        undefined ->
            [];
        <<>> ->
            [];
        Check ->
            % Set in mod_signup when fetching signup_url
            case m_server_storage:secure_lookup(Check, Context) of
                {ok, {Check, Props, SignupProps}} ->
                    [
                        {xs_props, {Props, SignupProps}}
                        | Props
                    ];
                {error, _} ->
                    []
            end
    end,
    % z_session:set(signup_xs, undefined, Context),
    Rendered = z_template:render(<<"signup.tpl">>, Vars, Context2),
    z_context:output(Rendered, Context2).

precheck_email(EmailNorm, Context) ->
    lists:foldl(
        fun
            (_F, {error, _} = Error) -> Error;
            (F, ok) -> F(EmailNorm, Context)
        end,
        ok,
        [
            fun precheck_account/2,
            fun precheck_blocked/2,
            fun precheck_ratelimit/2
        ]).

precheck_account(Email, Context) ->
    case m_identity:lookup_by_username(Email, Context) of
        undefined ->
            case m_identity:lookup_users_by_verified_type_and_key(email, Email, Context) of
                [] -> ok;
                _ -> {error, exists}
            end;
        _Idn ->
            {error, exists}
    end.

precheck_blocked(Email, Context) ->
    case z_notifier:first(#email_is_blocked{ recipient = Email }, Context) of
        true -> {error, blocked};
        false -> ok;
        undefined -> ok
    end.

precheck_ratelimit(Email, Context) ->
    case z_notifier:first(#auth_precheck{ username = Email }, Context) of
        undefined -> ok;
        {error, _} = Error -> Error
    end.

precheck_managed(Email, Page, Context) ->
    Options = #{
        is_username_checked => false,
        is_user_local => false,
        is_user_external => false,
        username => Email,
        user_external => [],
        page => Page,
        is_password_entered => false
    },
    Payload = #{
        <<"username">> => Email,
        <<"password">> => undefined
    },
    Options1 = z_notifier:foldl(#logon_options{ payload = Payload }, Options, Context),
    case Options1 of
        #{
            is_user_local := false,
            is_user_external := true,
            user_external := Ps
        } when Ps =/= [] ->
            {ok, external, Ps};
        #{
            is_username_checked := true,
            is_user_local := true,
            user_external := Ps
        } ->
            {ok, local, Ps};
        _ ->
            {ok, false, []}
    end.

send_code(Email, Context) ->
    {ok, Code} = mod_signup:new_onetime_code({signup, Email}, Context),
    z_email:send(#email{
            to = Email,
            html_tpl = "email_signup_code.tpl",
            vars = [
                {code, Code}
            ]
        }, Context).

is_ratelimit_code(Email, Context) ->
    Key = <<"onetime-code-check <", Email/binary, ">">>,
    case z_notifier:first(#auth_precheck{ username = Key }, Context) of
        undefined -> false;
        {error, _} -> true
    end.

normalize_username(undefined) ->
    <<>>;
normalize_username(Username) ->
    z_string:to_lower(z_string:trim(Username)).

event(#submit{ message={signup_email_step1, Args} }, Context) ->
    Email = z_string:trim(z_context:get_q_validated(<<"email">>, Context)),
    EmailNorm = m_identity:normalize_key(email, Email),
    Page = proplists:get_value(page, Args),
    Vars = case precheck_email(EmailNorm, Context) of
        ok ->
            % Check if email domain is managed by a service provider
            case precheck_managed(Email, Page, Context) of
                {ok, false, []} ->
                    % Account unknown and no external providers
                    % - Mail a code to the email address
                    % - Show form to enter code
                    % - Show status of email sending
                    case send_code(Email, Context) of
                        {ok, MsgNr} ->
                            [
                                {email, Email},
                                {message_nr, MsgNr},
                                {is_code_sent, true},
                                {is_logon_link, false},
                                {user_external, []}
                            ];
                        {error, _Reason} ->
                            [
                                {error, email_send_failed},
                                {email, Email},
                                {message_nr, undefined},
                                {is_code_sent, false},
                                {is_logon_link, false},
                                {user_external, []}
                            ]
                    end;
                {ok, local, UserExternal} ->
                    % Account exists, and maybe has external providers
                    % Show buttons for managed and logon link
                    [
                        {email, Email},
                        {is_code_sent, false},
                        {is_logon_link, true},
                        {user_external, UserExternal}
                    ];
                {ok, external, UserExternal} ->
                    % This email domain is managed by external providers
                    % Show buttons for providers and hide logon link
                    [
                        {email, Email},
                        {is_code_sent, false},
                        {is_logon_link, false},
                        {user_external, UserExternal}
                    ]
            end;
        {error, exists} ->
            % Some account exists (might be disabled).
            % Do not send a verification code.
            % Check if email domain is managed by a service provider.
            case precheck_managed(Email, Page, Context) of
                {ok, false, []} ->
                    [
                        {email, Email},
                        {is_code_sent, false},
                        {is_logon_link, true},
                        {user_external, []}
                    ];
                {ok, local, UserExternal} ->
                    [
                        {email, Email},
                        {is_code_sent, false},
                        {is_logon_link, true},
                        {user_external, UserExternal}
                    ];
                {ok, external, UserExternal} ->
                    [
                        {email, Email},
                        {is_code_sent, false},
                        {is_logon_link, false},
                        {user_external, UserExternal}
                    ]
            end;
        {error, Reason} ->
            % Failed: show an error
            [
                {error, Reason},
                {email, Email},
                {is_code_sent, false},
                {is_logon_link, false},
                {user_external, []}
            ]
    end,
    Vars1 = z_utils:props_merge(Vars, Args),
    Context1 = z_render:update("signup-email-step2", #render{ template = "_signup_with_email_step2.tpl", vars = Vars1 }, Context),
    z_render:wire([
            {hide, [ {target, "signup-login"}]},
            {hide, [ {target, "signup-services"}]},
            {hide, [ {target, "signup-email-step1"} ]},
            {fade_in, [ {target, "signup-email-step2"} ]},
            {hide, [ {target, "signup-email-step3"} ]}
        ], Context1);
event(#submit{ message={signup_email_step2, Args} }, Context) ->
    % Check:the verification code and show the form to enter the rest of the details
    {email, Email} = proplists:lookup(email, Args),
    EmailNorm = m_identity:normalize_key(email, Email),
    Code = z_string:trim(z_convert:to_binary(z_context:get_q(<<"code">>, Context))),
    case is_ratelimit_code(EmailNorm, Context) of
        true ->
            z_render:wire([
                    {hide, [ {target, "signup-code-error"} ]},
                    {fade_in, [ {target, "signup-code-ratelimit"} ]}
                ], Context);
        false ->
            case mod_signup:check_onetime_code({signup, EmailNorm}, Code, Context) of
                true ->
                    % Code is correct, render step3
                    Vars = [
                        {is_code_checked, true}
                        | Args
                    ],
                    Context1 = z_render:update("signup-email-step3", #render{ template = "_signup_with_email_step3.tpl", vars = Vars }, Context),
                    z_render:wire([
                            {hide, [ {target, "signup-email-step1"} ]},
                            {hide, [ {target, "signup-email-step2"} ]},
                            {fade_in, [ {target, "signup-email-step3"} ]},
                            {update, [
                                {target, "signup-email-step2"},
                                {text, <<>>}
                            ]}
                        ], Context1);
                false ->
                    z_render:wire([
                            {hide, [ {target, "signup-code-ratelimit"} ]},
                            {fade_in, [ {target, "signup-code-error"} ]}
                        ], Context)
            end
    end;
event(#submit{ message={signup_email_step3, Args} }, Context) ->
    % Check:
    % - check the email address in the args with the email address in the post data
    {email, Email} = proplists:lookup(email, Args),
    case z_context:get_q(<<"email">>, Context) of
        QEmail when QEmail =:= Email ->
            Agree = z_convert:to_bool(z_context:get_q(<<"signup_tos_agree">>, Context)),
            case Agree of
                true ->
                    {FormProps, XsSignupProps} = form_props(Args, Context),
                    FormProps1 = FormProps#{
                        <<"email">> => Email
                    },
                    SignupProps = fetch_signup_props(Email, XsSignupProps, Context),
                    % Check uniqueness of the username and email again
                    UsernameCheck = username_idn_check(SignupProps, Context),
                    EmailCheck = email_idn_check(SignupProps, Context),
                    if
                        UsernameCheck =/= undefined ->
                            z_render:wire([
                                    {hide, [ {target, <<".signup-error">>} ]},
                                    {fade_in, [ {target, <<"signup_error_username">>} ]}
                                ],
                                Context);
                        EmailCheck =/= [] ->
                            z_render:wire([
                                    {hide, [ {target, <<".signup-error">>} ]},
                                    {fade_in, [ {target, <<"signup_error_email">>} ]}
                                ],
                                Context);
                        true ->
                            % All ok - do the signup
                            case signup(FormProps1, SignupProps, Context) of
                                {ok, ContextSignup} ->
                                    ContextSignup;
                                {error, #context{} = ContextError} ->
                                    z_render:wire([
                                            {hide, [ {target, <<".signup-error">>} ]},
                                            {fade_in, [ {target, <<"signup_error_generic">>} ]}
                                        ], ContextError);
                                {error, username} ->
                                    z_render:wire([
                                            {hide, [ {target, <<".signup-error">>} ]},
                                            {fade_in, [ {target, <<"signup_error_username">>} ]}
                                        ],
                                        Context);
                                {error, duplicate} ->
                                    z_render:wire([
                                            {hide, [ {target, <<".signup-error">>} ]},
                                            {fade_in, [ {target, <<"signup_error_duplicate_identity">>} ]}
                                        ],
                                        Context);
                                {error, _Reason} ->
                                    z_render:wire([
                                            {hide, [ {target, <<".signup-error">>} ]},
                                            {fade_in, [ {target, <<"signup_error_generic">>} ]}
                                        ],
                                        Context)
                            end
                    end;
                false ->
                    z_render:wire([
                            {hide, [ {target, <<".signup-error">>} ]},
                            {fade_in, [ {target, <<"signup_error_tos_agree">>} ]}
                        ],
                        Context)
            end;
        _TamperedEmail ->
            z_render:wire({alert, [
                    {title, ?__(<<"Email address mismatch">>, Context)},
                    {text, ?__(<<"The email address you entered does not match the email address you provided in the previous step. Please check your input and try again.">>, Context)}
                ]}, Context)
    end;
event(#postback{ message={signup_go_step1, _Args} }, Context) ->
    z_render:wire([
            {hide, [{target, "signup-email-step2"}]},
            {hide, [{target, "signup-email-step3"}]},
            {fade_in, [{target, "signup-email-step1"}]},
            {fade_in, [ {target, "signup-login"}]},
            {fade_in, [ {target, "signup-services"}]},
            {update, [{target, "signup-email-step2"}, {text, <<>>}]},
            {update, [{target, "signup-email-step3"}, {text, <<>>}]},
            {focus, [{target, "signup-email-input"}]}
        ], Context);
event(#postback{ message={signup_resend_code, Args} }, Context) ->
    {email, Email} = proplists:lookup(email, Args),
    case send_code(Email, Context) of
        {ok, MsgNr} ->
            Context1 = z_render:wire([
                    {hide, [ {target, "signup-code-error"} ]},
                    {set_value, [ {target, "signup-code-input"}, {value, <<>>} ]},
                    {focus, [ {target, "signup-code-input"} ]},
                    {fade_in, [ {target, "signup-code-resent"} ]},
                    {script, [ {script, <<"setTimeout(function() { $('#signup-code-resent').fadeOut(); }, 5000);">>} ]}
                ], Context),
            z_render:update(
                "signup-code-email-status",
                #render{
                    template = "_email_send_status.tpl",
                    vars = [
                        {message_nr, MsgNr},
                        {email, Email}
                    ]
                },
                Context1);
        {error, _Reason} ->
            z_render:update("signup-email-step2", #render{ template = "_signup_with_email_step2.tpl", vars = [
                {error, email_send_failed},
                {email, Email},
                {message_nr, undefined},
                {is_code_sent, false}
            ]}, Context)
    end.

username_idn_check(SignupProps, Context) ->
    case find_identity(username, SignupProps) of
        none -> none;
        {username_pw, {Username, _, _, _}} -> m_identity:lookup_by_username(Username, Context)
    end.

email_idn_check(SignupProps, Context) ->
    case find_identity(email, SignupProps) of
        none -> none;
        {email, Email, _, _} -> m_identity:lookup_users_by_verified_type_and_key(email, Email, Context)
    end.

form_props(Args, Context) ->
    % Optional XsProps from the signup url generation.
    {XsProps0, XsSignupProps} = case proplists:get_value(xs_props, Args) of
        {A,B} -> {A,B};
        undefined -> {undefined, undefined}
    end,
    XsProps = if
        is_list(XsProps0) -> XsProps0;
        is_map(XsProps0) -> XsProps0;
        true -> []
    end,
    % Call observers to fetch the required signup form fields and if
    % they should be validated.
    FormFields0 = [
        % {email, true}, --- Already known from step1, always exclude
        {name_first, true},
        {name_surname_prefix, false},
        {name_surname, true}
    ],
    FormFields = case proplists:get_value(form_fields, Args) of
        FF when is_list(FF) ->
            FF1 = [ {K, z_convert:to_bool(V)} || {K, V} <- FF ],
            lists:keymerge(1, lists:sort(FF1), lists:sort(FormFields0));
        {form_fields, FF} when is_list(FF) ->
            FF1 = [ {K, z_convert:to_bool(V)} || {K, V} <- FF ],
            lists:keymerge(1, lists:sort(FF1), lists:sort(FormFields0));
        undefined ->
            z_notifier:foldr(signup_form_fields, FormFields0, Context)
    end,
    FormProps = lists:foldl(
        fun({Prop, Validate}, Acc) ->
            {PropB, PropValue} = fetch_prop(Prop, Validate, XsProps, Context),
            Acc#{
                PropB => PropValue
            }
        end,
        #{},
        FormFields),
    {FormProps, XsSignupProps}.

%% Fetch the signup identities. Either from the XsSignupProps passed by the signup
%% caller, or from the query arguments.
fetch_signup_props(Email, XsSignupProps, Context) ->
    SignupProps = if
        is_list(XsSignupProps), length(XsSignupProps) > 0 ->
            XsSignupProps;
        true ->
            Username = normalize_username(z_context:get_q(<<"username">>, Context)),
            Password = z_string:trim(z_context:get_q_validated(<<"password">>, Context)),
            [
                {identity, {username_pw, {Username, Password}, true, true}}
            ]
    end,
    case z_utils:is_empty(Email) of
        true ->
            SignupProps;
        false ->
            case find_identity(email, SignupProps) of
                undefined ->
                    [
                        {identity, {email, Email, false, true}}
                        | SignupProps
                    ];
                {email, _, _, _} ->
                    SignupProps
            end
    end.

%% @doc Fetch the value of the property, either form the XsProps passed by the
%% signup caller, or from a query argument. Optionally the query argument is
%% validated.
fetch_prop(Prop, IsValidated, XsProps, Context) ->
    PropB = z_convert:to_binary(Prop),
    case fetch_signup_prop(Prop, PropB, XsProps) of
        undefined ->
            {PropV, V} = if
                IsValidated ->
                    {PropB, z_context:get_q_validated(PropB, Context)};
                true ->
                    case z_context:get_q(PropB, Context) of
                        undefined when PropB =:= <<"name_surname_prefix">> ->
                            {<<"surprefix">>, z_context:get_q(<<"surprefix">>, Context)};
                        QV ->
                            {PropB, QV}
                    end
            end,
            {PropV, maybe_trim(V)};
        V ->
            {PropB, V}
    end.

% Do not accept uploaded files; trim all string values.
maybe_trim(V) when is_binary(V); is_list(V) -> z_string:trim(z_convert:to_binary(V));
maybe_trim(V) when is_number(V); is_boolean(V) -> V;
maybe_trim(undefined) -> undefined;
maybe_trim(#upload{}) -> undefined.

fetch_signup_prop(Prop, PropB, XsProps) when is_list(XsProps) ->
    proplists:get_value(Prop, XsProps, proplists:get_value(PropB, XsProps, undefined));
fetch_signup_prop(Prop, PropB, XsProps) when is_map(XsProps) ->
    maps:get(Prop, XsProps, maps:get(PropB, XsProps, undefined));
fetch_signup_prop(_Prop, _PropB, undefined) ->
    undefined.

find_identity(_Type, []) ->
    undefined;
find_identity(Type, [{identity, {IdnType, _, _, _} = Idn} | _]) when Type =:= IdnType ->
    Idn;
find_identity(Type, [_ | SignupProps])  ->
    find_identity(Type, SignupProps).

%% @doc Sign up a new user. Check if the identity is available.
signup(Props, SignupProps, Context) ->
    UserId = proplists:get_value(user_id, SignupProps),
    SignupProps1 = proplists:delete(user_id, SignupProps),
    case mod_signup:signup_existing(UserId, Props, SignupProps1, false, Context) of
        {ok, NewUserId} ->
            ensure_published(NewUserId, z_acl:sudo(Context)),
            {ok, ContextUser} = z_auth:logon(NewUserId, Context),
            Location = case get_redirect_page(SignupProps) of
                <<>> -> m_signup:confirm_redirect(ContextUser);
                Url -> Url
            end,
            % Post a onetime-token to the auth worker on the page
            % The auth worker will exchange it for a valid cookie and then perform
            % the redirect to the url.
            case z_authentication_tokens:encode_onetime_token(NewUserId, ContextUser) of
                {ok, Token} ->
                    AuthMsg = #{
                        token => Token,
                        url => Location
                    },
                    z_mqtt:publish(<<"~client/model/auth/post/onetime-token">>, AuthMsg, Context),
                    z_render:wire({mask, []}, Context);
                {error, Reason} ->
                    ?LOG_ERROR(#{
                        text => <<"Error making onetime token">>,
                        in => zotonic_mod_signup,
                        result => error,
                        reason => Reason
                    }),
                    show_errors([internal], Context)
            end;
        {error, {identity_in_use, username}} ->
            {error, username};
        {error, {identity_in_use, _}} ->
            {error, duplicate};
        {error, #context{} = ContextError} ->
            {error, ContextError};
        {error, _Reason} = Error ->
            Error
    end.

get_redirect_page(SignupProps) ->
    z_convert:to_binary(proplists:get_value(ready_page, SignupProps, <<>>)).


ensure_published(UserId, Context) ->
    case m_rsc:p(UserId, <<"is_published">>, Context) of
        true -> {ok, UserId};
        false -> m_rsc:update(UserId, #{ <<"is_published">> => true }, Context)
    end.


show_errors(Errors, Context) ->
    Errors1 = [ z_convert:to_list(E) || E <- Errors ],
    z_render:wire({add_class, [{target, "signup_logon_box"}, {class, Errors1}]}, Context).

