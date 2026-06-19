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

It also implements the necessary postbacks to perform the signup and log a user in.

Data flow
---------

The signup page can be reached directly through the `signup` dispatch rule, or by
asking the notification system for a `#signup_url{}`. The `mod_signup`
`observe_signup_url/2` observer stores the caller supplied arguments in
`mod_server_storage`:

```erlang
#signup_url{
    props = Props,
    signup_props = SignupProps
}
```

`Props` are resource properties for the person that will be created or updated,
for example a prefilled `email`, `name_first`, or `depiction_url`. `SignupProps`
are signup control values and identities, for example `{user_id, Id}`,
`{ready_page, Url}`, or `{identity, {Type, Key, IsUnique, IsVerified}}`.

The observer generates a random check id, stores
`{CheckId, Props, SignupProps}`, and returns the signup URL with `xs=CheckId`.
When this controller renders the page it reads the `xs` query argument, performs
`m_server_storage:secure_lookup/2`, and only accepts the stored payload when the
returned check id matches the query value. Accepted values are exposed to
`signup.tpl` as:

* `props`: a map with prefilled resource properties. Empty email values are
  removed so the email step can still ask for an address.
* `signup_props`: the stored signup properties as a proplist.
* `email`: the prefilled email from `props`, when present.

Without a valid `xs`, the controller renders with empty signup properties and
default `props`. The `xs` value itself is not posted back. Instead, the stored
payload is carried forward as template variables in each wire postback
(`props=props signup_props=signup_props`). This keeps the browser-visible form
state limited to the values the template already needs to render, while the
original `xs` token is only used to bootstrap the first page render.

The controller accepts these postbacks:

* `signup_email_step1`: validates and prechecks the email address. If the address
  is new and local signup is possible, a short one-time code is mailed using
  `email_signup_code.tpl`. If the address is already known, or an external
  provider handles the domain, the second step shows a logon link and/or external
  provider buttons instead.
* `signup_email_step2`: checks the mailed one-time code. On success, the code is
  deleted and the account details form is rendered.
* `signup_email_step3`: verifies the email did not change, checks the terms
  checkbox, merges posted form properties with any prefilled `props`, combines
  identities from `signup_props` with the verified email identity, creates or
  updates the user through `mod_signup:signup_existing/5`, logs the user on, and
  sends a one-time authentication token to the client auth model for redirect.
* `signup_go_step1`: returns the browser UI to the first email step.
* `signup_resend_code`: replaces the stored one-time code for the email address
  and sends a new `email_signup_code.tpl` message.

Template structure
------------------

The public signup page is `signup.tpl`. It extends `base.tpl`, adds the logon
CSS in `html_head_extra`, and fills `content_area` by including
`_signup_box.tpl`. Other pages can include `_signup_box.tpl` directly when they
need the signup UI without the surrounding page inheritance. The signup box
handles already logged-on users and otherwise includes:

* `_signup_with_email.tpl` for the three-step email signup flow.
* all `_logon_extra.tpl` templates supplied by active authentication modules for
  SSO signup options.

`_signup_with_email.tpl` includes `_signup_with_email_step1.tpl` for the first
email form and renders empty containers for steps two and three. The controller
fills those containers with `_signup_with_email_step2.tpl` and
`_signup_with_email_step3.tpl` after the corresponding postbacks.

`_signup_with_email_step1.tpl` renders the email address form and posts
`signup_email_step1`. If an email address was supplied through `props`, it shows
that address with a Change link. The Change postback re-renders
`_signup_with_email_step1.tpl` without the prefilled email so the visitor can
enter another address.

`_signup_with_email_step2.tpl` shows the selected email address, the mailed code
form, send status, resend action, existing-account logon link, external-provider
options, and error states.

`_signup_with_email_step3.tpl` renders the final signup form. It defines blocks
around the form, field set, individual field groups, and error area so sites can
override the shape of the final step. It uses the configuration exposed through
`m.signup.config.username_equals_email` to decide whether the username is hidden
and equal to the email address, or entered separately by the user.

Email address confirmation
--------------------------

There are two email checks in the complete signup lifecycle.

The first check happens before account creation. `signup_email_step1` sends the
short code rendered by `email_signup_code.tpl`. Codes are stored in the
`mod_signup` gen_server under the tag `{signup, EmailNorm}`. They are replaced on
resend, expire by generational garbage collection, are rate-limit checked through
`#auth_precheck{}`, and are deleted after a successful `signup_email_step2`.
This confirms that the visitor can read the mailbox before the account is
created. The verified email is then added to the signup identities as
`{identity, {email, Email, false, true}}` unless an email identity was already
supplied in `signup_props`.

The second check is the account identity confirmation handled by `mod_signup`
and `controller_signup_confirm`. If signup is requested with confirmation
enabled and no non-password identity is already verified, `mod_signup` creates
the user unpublished and unverified, inserts unverified identities, and sends
`email_verify.tpl` through the `#identity_verification{}` observer. The email
contains a `signup_confirm` URL with the identity verification key.

`controller_signup_confirm` renders `signup_confirm.tpl`. That template extends
`base.tpl` and immediately posts the key back to the confirmation controller.
The confirmation controller looks up the identity by verification key, publishes
the user resource, marks the account and identity as verified, emits
`#signup_confirm{id=UserId}`, logs the user on, and redirects to the first
`#signup_confirm_redirect{}` result or to the user's page.

").
-author("Marc Worrell <marc@worrell.nl>").

-export([
    process/4
]).
-export([event/2]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(TIMEOUT, 3600).

process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
    Context2 = z_context:ensure_qs(Context),
    z_context:logger_md(Context2),
    DefaultVars = [
        {signup_props, []},
        {props, #{}}
    ],
    Vars = case z_context:get_q(<<"xs">>, Context2) of
        undefined -> DefaultVars;
        <<>> -> DefaultVars;
        QXs ->
            % Set in mod_signup when fetching signup_url
            case m_server_storage:secure_lookup(QXs, Context) of
                {ok, {Check, Timestamp, Props, SignupProps}} when QXs =:= Check ->
                    Now = z_datetime:timestamp(),
                    if
                        Timestamp + ?TIMEOUT > Now ->
                            Props1 = maybe_drop_email(as_props_map(Props)),
                            Email = maps:get(<<"email">>, Props1, undefined),
                            SignupProps1 = as_list(SignupProps),
                            [
                                {signup_props, SignupProps1},
                                {props, Props1},
                                {email, Email},
                                {page, z_context:get_q(<<"p">>, Context)},
                                {is_email_verified, is_email_idn_verified(Email, SignupProps1)}
                            ];
                        true ->
                            % Expired, delete from server storage
                            m_server_storage:secure_delete(QXs, Context),
                            DefaultVars
                    end;
                {ok, _} -> DefaultVars;
                {error, _} -> DefaultVars
            end
    end,
    % z_session:set(signup_xs, undefined, Context),
    Rendered = z_template:render(<<"signup.tpl">>, Vars, Context2),
    z_context:output(Rendered, Context2).

is_email_idn_verified(undefined, _SignupProps) ->
    false;
is_email_idn_verified(Email, SignupProps)->
    case m_identity:normalize_key(email, Email) of
        <<>> -> false;
        EmailNorm ->
            lists:any(
                fun
                    ({identity, {email, EmailIdn, _Unique, true}}) ->
                        EmailIdnNorm = m_identity:normalize_key(email, EmailIdn),
                        EmailIdnNorm =:= EmailNorm;
                    (_) ->
                        false
                end,
                SignupProps)
    end.

remove_email_idn(Email, SignupProps) ->
    case m_identity:normalize_key(email, Email) of
        <<>> -> SignupProps;
        EmailNorm ->
            lists:filter(
                fun
                    ({identity, {email, EmailIdn, _Unique, _Verified}}) ->
                        EmailIdnNorm = m_identity:normalize_key(email, EmailIdn),
                        EmailIdnNorm =/= EmailNorm;
                    (_) ->
                        true
                end,
                SignupProps)
    end.

as_list(undefined) -> [];
as_list(L) when is_list(L) -> L.

as_props_map(undefined) -> #{};
as_props_map(L) when is_list(L) -> z_props:from_props(L);
as_props_map(M) when is_map(M) -> z_props:from_map(M).

%% @doc Check if the email property is non-empty, if it is empty then delete it.
%% This is makes email property handling easier in the event routines.
maybe_drop_email(#{ <<"email">> := Email } = Props) when is_binary(Email); is_list(Email) ->
    case z_string:trim(z_convert:to_binary(Email)) of
        <<>> -> maps:remove(<<"email">>, Props);
        EmailTrimmed -> Props#{ <<"email">> => EmailTrimmed }
    end;
maybe_drop_email(Props) ->
    maps:remove(<<"email">>, Props).

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
    EmailNorm = m_identity:normalize_key(email, Email),
    {ok, Code} = mod_signup:new_onetime_code({signup, EmailNorm}, Context),
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
    Props = proplists:get_value(props, Args, #{}),
    SignupProps = proplists:get_value(signup_props, Args, []),
    Email = case Props of
        #{ <<"email">> := EmailProp } -> EmailProp;
        _ -> z_string:trim(z_context:get_q_validated(<<"email">>, Context))
    end,
    EmailNorm = m_identity:normalize_key(email, Email),
    IsEmailVerified = is_email_idn_verified(EmailNorm, SignupProps),
    Page = proplists:get_value(page, Args),
    Vars = case precheck_email(EmailNorm, Context) of
        ok ->
            % Check if email domain is managed by a service provider
            case precheck_managed(Email, Page, Context) of
                {ok, false, []} when not IsEmailVerified ->
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
                                {is_email_verified, false},
                                {user_external, []}
                            ];
                        {error, _Reason} ->
                            [
                                {error, email_send_failed},
                                {email, Email},
                                {message_nr, undefined},
                                {is_code_sent, false},
                                {is_logon_link, false},
                                {is_email_verified, false},
                                {user_external, []}
                            ]
                    end;
                {ok, false, []} when IsEmailVerified ->
                    % Account unknown and no external providers
                    % - Email is verified - no email with code needed
                    [
                        {email, Email},
                        {is_code_sent, false},
                        {is_logon_link, false},
                        {is_email_verified, true},
                        {user_external, []}
                    ];
                {ok, local, UserExternal} ->
                    % Account exists, and maybe has external providers
                    % Show buttons for managed and logon link
                    [
                        {email, Email},
                        {is_code_sent, false},
                        {is_logon_link, true},
                        {is_email_verified, false},
                        {user_external, UserExternal}
                    ];
                {ok, external, UserExternal} ->
                    % This email domain is managed by external providers
                    % Show buttons for providers and hide logon link
                    [
                        {email, Email},
                        {is_code_sent, false},
                        {is_logon_link, false},
                        {is_email_verified, false},
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
                        {is_email_verified, false},
                        {user_external, []}
                    ];
                {ok, local, UserExternal} ->
                    [
                        {email, Email},
                        {is_code_sent, false},
                        {is_logon_link, true},
                        {is_email_verified, false},
                        {user_external, UserExternal}
                    ];
                {ok, external, UserExternal} ->
                    [
                        {email, Email},
                        {is_code_sent, false},
                        {is_logon_link, false},
                        {is_email_verified, false},
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
                {is_email_verified, false},
                {user_external, []}
            ]
    end,
    HasError = proplists:get_value(error, Vars) =/= undefined,
    HasExternal = length(proplists:get_value(user_external, Vars)) > 0,
    HasIsEmailVerified = proplists:get_bool(is_email_verified, Vars),
    HasCodeSent = proplists:get_bool(is_code_sent, Vars),
    HasLogonLink = proplists:get_bool(is_logon_link, Vars),
    if
        not HasError
        andalso not HasExternal
        andalso HasIsEmailVerified
        andalso not HasCodeSent
        andalso not HasLogonLink ->
            % Email was verified, no external providers, no errors and no existing account
            % Skip step 2 and continue to the signup form of step3
            Vs = [
                {email, Email},
                {is_code_checked, true}
            ],
            Vars1 = z_utils:props_merge(Vs, Args),
            Context1 = z_render:update("signup-email-step3", #render{ template = "_signup_with_email_step3.tpl", vars = Vars1 }, Context),
            z_render:wire([
                    {hide, [ {target, "signup-login"}]},
                    {hide, [ {target, "signup-services"}]},
                    {hide, [ {target, "signup-email-step1"} ]},
                    {hide, [ {target, "signup-email-step2"} ]},
                    {fade_in, [ {target, "signup-email-step3"} ]},
                    {update, [
                        {target, "signup-email-step2"},
                        {text, <<>>}
                    ]}
                ], Context1);
        true ->
            Vars1 = z_utils:props_merge(Vars, Args),
            Context1 = z_render:update("signup-email-step2", #render{ template = "_signup_with_email_step2.tpl", vars = Vars1 }, Context),
            z_render:wire([
                    {hide, [ {target, "signup-login"}]},
                    {hide, [ {target, "signup-services"}]},
                    {hide, [ {target, "signup-email-step1"} ]},
                    {fade_in, [ {target, "signup-email-step2"} ]},
                    {hide, [ {target, "signup-email-step3"} ]}
                ], Context1)
    end;
event(#submit{ message={signup_email_step2, Args} }, Context) ->
    % Check:the verification code and show the form to enter the rest of the details
    {email, Email} = proplists:lookup(email, Args),
    {signup_props, SignupProps} = proplists:lookup(signup_props, Args),
    EmailNorm = m_identity:normalize_key(email, Email),
    case is_email_idn_verified(EmailNorm, SignupProps) of
        false ->
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
                            ok = mod_signup:delete_onetime_code({signup, EmailNorm}, Context),
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
        true ->
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
                ], Context1)
    end;
event(#submit{ message={signup_email_step3, Args} }, Context) ->
    % Check:
    % - check the email address in the args with the email address in the post data
    {email, Email} = proplists:lookup(email, Args),
    Page = proplists:get_value(page, Args, <<>>),
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
                            case signup(FormProps1, SignupProps, Page, Context) of
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
event(#postback{ message={signup_go_step1, Args} }, Context) ->
    % Allow user to edit their email address, even if it was passed
    % in the props/signup_props.
    % - Re-render step1 form with the email address form.
    Email = proplists:get_value(email, Args),
    Args1 = [
        {signup_props, remove_email_idn(Email, proplists:get_value(signup_props, Args))},
        {props, maps:remove(<<"email">>, proplists:get_value(props, Args))},
        {email, Email},
        {is_email_verified, false}
    ],
    Context1 = z_render:update("signup-email-step1", #render{ template = "_signup_with_email_step1.tpl", vars = Args1 }, Context),
    z_render:wire([
            {hide, [{target, "signup-email-step2"}]},
            {hide, [{target, "signup-email-step3"}]},
            {update, [{target, "signup-email-step2"}, {text, <<>>}]},
            {update, [{target, "signup-email-step3"}, {text, <<>>}]},
            {fade_in, [{target, "signup-email-step1"}]},
            {fade_in, [ {target, "signup-login"}]},
            {fade_in, [ {target, "signup-services"}]},
            {focus, [{target, "signup-email-input"}]}
        ], Context1);
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
    case find_identity(username_pw, SignupProps) of
        undefined ->
            undefined;
        {username_pw, {Username, _Password}, _IsUnique, _IsVerified} ->
            m_identity:lookup_by_username(Username, Context)
    end.

email_idn_check(SignupProps, Context) ->
    case find_identity(email, SignupProps) of
        undefined ->
            [];
        {email, Email, _IsUnique, _IsVerified} ->
            m_identity:lookup_users_by_verified_type_and_key(email, Email, Context)
    end.

form_props(Args, Context) ->
    % Optional XsProps from the signup url generation.
    XsProps = proplists:get_value(props, Args, #{}),
    XsSignupProps = proplists:get_value(signup_props, Args, []),
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
    Username = normalize_username(z_context:get_q(<<"username">>, Context)),
    Password = z_string:trim(z_context:get_q_validated(<<"password">>, Context)),
    SignupProps = [
        {identity, {username_pw, {Username, Password}, true, true}}
        | XsSignupProps
    ],
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
                    AltProp = maybe_alternate_prop(PropB),
                    case z_context:get_q(PropB, Context) of
                        undefined when is_binary(AltProp) ->
                            {PropB, z_context:get_q(AltProp, Context)};
                        QV ->
                            {PropB, QV}
                    end
            end,
            {PropV, maybe_trim(V)};
        V ->
            {PropB, V}
    end.

maybe_alternate_prop(<<"name_surname_prefix">>) -> <<"surprefix">>;
maybe_alternate_prop(_) -> false.


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
signup(Props, SignupProps, Page, Context) ->
    UserId = proplists:get_value(user_id, SignupProps),
    SignupProps1 = proplists:delete(user_id, SignupProps),
    case mod_signup:signup_existing(UserId, Props, SignupProps1, false, Context) of
        {ok, NewUserId} ->
            ensure_published(NewUserId, z_acl:sudo(Context)),
            {ok, ContextUser} = z_auth:logon(NewUserId, Context),
            Location = case get_redirect_page(SignupProps, Page) of
                <<>> -> m_signup:confirm_redirect(ContextUser);
                Url -> Url
            end,
            % Post a onetime-token to the auth worker on the page
            % The auth worker will exchange it for a valid cookie and then perform
            % the redirect to the url.
            AuthOptions = case find_identity(username_pw, SignupProps) of
                undefined ->
                    % TODO: determine which identity was used for the signup
                    #{
                    };
                {Username, _Password} ->
                    #{
                        auth_service => <<"username_pw">>,
                        auth_service_uid => Username
                    }
            end,
            case z_authentication_tokens:encode_onetime_token(NewUserId, AuthOptions, ContextUser) of
                {ok, Token} ->
                    AuthMsg = #{
                        token => Token,
                        url => Location
                    },
                    z_mqtt:publish(<<"~client/model/auth/post/onetime-token">>, AuthMsg, Context),
                    {ok, z_render:wire({mask, []}, Context)};
                {error, Reason} = Error ->
                    ?LOG_ERROR(#{
                        text => <<"Error making onetime token">>,
                        in => zotonic_mod_signup,
                        result => error,
                        reason => Reason
                    }),
                    Error
            end;
        {error, {identity_in_use, username_pw}} ->
            {error, username};
        {error, {identity_in_use, _}} ->
            {error, duplicate};
        {error, #context{} = ContextError} ->
            {error, ContextError};
        {error, _Reason} = Error ->
            Error
    end.

get_redirect_page(SignupProps, Page) ->
    case z_string:trim(z_convert:to_binary(proplists:get_value(ready_page, SignupProps))) of
        <<>> -> z_string:trim(z_convert:to_binary(Page));
        ReadyPage -> ReadyPage
    end.

ensure_published(UserId, Context) ->
    case m_rsc:p(UserId, <<"is_published">>, Context) of
        true -> {ok, UserId};
        false -> m_rsc:update(UserId, #{ <<"is_published">> => true }, Context)
    end.
