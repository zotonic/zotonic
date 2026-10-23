%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2026 Marc Worrell
%% @doc Handle the signup confirmation link
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

-module(controller_signup_confirm).
-moduledoc(#{
    zotonic_keywords => ["reference", "backend_developer", "controller", "identity_and_accounts", "authentication", "create"]
}).
-moduledoc("
Controller which displays the confirmation page where the user can confirm his signup.

The template used is `signup_confirm.tpl`. It extends `base.tpl` and posts the
verification key back to this controller using a `{confirm, ...}` postback.

The key is created for an email identity by `mod_signup` when an account needs
identity verification. On a valid key this controller publishes the user
resource, marks the account and identity as verified, emits
`#signup_confirm{id=UserId}`, logs the user on, and redirects to the first
`#signup_confirm_redirect{}` result, the destination saved at signup, or the
user's page (falling back to home). The destination and verification key are
consumed together; resending a verification email preserves the destination.

Handled events
--------------

* `{confirm, [{key, Key}]}` verifies the email identity associated with `Key`,
  publishes the account, logs the user on, and redirects after confirmation.
").
-author("Marc Worrell <marc@worrell.nl>").

-export([
    process/4
]).
-export([event/2]).

-include_lib("zotonic_core/include/zotonic.hrl").


process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
    z_context:logger_md(Context),
    Rendered = z_template:render("signup_confirm.tpl", [], Context),
    z_context:output(Rendered, Context).


%% @doc Handle the submit of the signup form.
event(#postback{ message={confirm, [ {key, Key} ]}}, Context) ->
    case m_signup:confirm(Key, Context) of
        {ok, {UserId, IdnType, IdnKey, RequestPage}} ->
            {ok, ContextUser} = z_auth:logon(UserId, Context),
            Url = m_signup:confirm_redirect(RequestPage, ContextUser),
            AuthOptions = #{
                auth_service => z_convert:to_binary(IdnType),
                auth_service_uid => IdnKey
            },
            ok = z_auth:logon_redirect(UserId, Url, AuthOptions, ContextUser),
            z_render:growl(?__("Redirecting...", Context), Context);
        {error, _Reason} ->
            z_render:wire([
                    {hide, [{target,"confirm_wait"}]},
                    {show, [{target,"confirm_error"}]}
                ], Context)
    end.
