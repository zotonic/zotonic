%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021-2026 Marc Worrell
%% @doc Model to support signup views.
%% @end

%% Copyright 2021-2026 Marc Worrell
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

-module(m_signup).
-moduledoc(#{
    zotonic_keywords => ["reference", "backend_developer", "model", "identity_and_accounts", "create"]
}).
-moduledoc("
Exported APIs:

*   `model/signup/get/confirm_redirect` return the URL for redirecting the current user after a signup.
    
    Calls the `#signup_confirm_redirect{ id = UserId, request_page = RequestPage }`
    notification. The first observer result overrides the requested destination;
    otherwise signup uses that destination, the user's page, or the home page.
    All destinations are constrained to the current site. This model GET is public
    and only resolves a destination for the current user; it consumes no state.

*   `confirm_redirect/2` also accepts a requested destination; `/1` passes `undefined`.
*   `store_confirm_redirect/3` is an internal, server-side signup API. It stores
    `signup_ready_page` in the unverified email identities' `propb` maps or
    proplists, preserving other properties. Callers must already be authorized
    to create or update the signup. Resending verification mail preserves it.
*   `confirm/2` authorizes confirmation with the verification key, publishes and
    verifies the account, and consumes the pending destination transactionally.
    It emits `#signup_confirm{}` after commit. Neither mutation is exposed through
    the model's MQTT/template API.

Available Model API Paths
-------------------------

| Method | Path pattern | Description |
| --- | --- | --- |
| `get` | `/confirm_redirect/...` | Return the same-site post-signup URL: notifier result, user `page_url`, or `home`. Public; uses the current user and consumes no pending destination. |
| `get` | `/config/email_unique/...` | Return `mod_signup.email_unique` boolean setting (default `true`). If enabled, signup e-mail addresses must be unique among user accounts and must not match another resource's e-mail identity when that identity is marked `is_unique`. Non-account resources with non-unique e-mail identities do not block signup. |
| `get` | `/config/username_equals_email/...` | Return `mod_signup.username_equals_email` boolean setting (default `true`) indicating whether signup usernames must equal email addresses. |

`/+name` marks a variable path segment. A trailing `/...` means extra path segments are accepted for further lookups.
").

-behaviour(zotonic_model).

-export([
    m_get/3,
    confirm_redirect/1,
    confirm_redirect/2,
    normalize_redirect/2,
    store_confirm_redirect/3,
    confirm/2
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ <<"confirm_redirect">> | Rest ], _Msg, Context) ->
    Url = confirm_redirect(Context),
    {ok, {Url, Rest}};
m_get([ <<"config">>, <<"username_equals_email">> | Rest ], _Msg, Context) ->
    V = m_config:get_boolean(mod_signup, username_equals_email, true, Context),
    {ok, {V, Rest}};
m_get([ <<"config">>, <<"email_unique">> | Rest ], _Msg, Context) ->
    V = m_config:get_boolean(mod_signup, email_unique, true, Context),
    {ok, {V, Rest}};
m_get(_Path, _Msg, _Context) ->
    {error, unknown_path}.

%% @doc Resolve the post-signup destination for the authenticated user.
-spec confirm_redirect(z:context()) -> binary().
confirm_redirect(Context) ->
    confirm_redirect(undefined, Context).

-spec confirm_redirect(RequestPage, Context) -> Url when
    RequestPage :: term(),
    Context :: z:context(),
    Url :: binary().
confirm_redirect(RequestPage, Context) ->
    Page = normalize_redirect(RequestPage, Context),
    Url = case z_acl:user(Context) of
        undefined ->
            undefined;
        UserId ->
            ObserverPage = z_notifier:first(#signup_confirm_redirect{
                id = UserId,
                request_page = Page
            }, Context),
            case normalize_redirect(ObserverPage, Context) of
                undefined when Page =/= undefined -> Page;
                undefined -> m_rsc:p(UserId, <<"page_url">>, Context);
                Location -> Location
            end
    end,
    case normalize_redirect(Url, Context) of
        undefined -> z_context:site_url(z_dispatcher:url_for(home, Context), Context);
        Location1 -> Location1
    end.

%% @doc Normalize external destinations. A reload marker has no meaning when
%% confirmation happens in another browser; the signup form resolves it first.
-spec normalize_redirect(Page, Context) -> Url when
    Page :: term(),
    Context :: z:context(),
    Url :: binary() | undefined.
normalize_redirect(Page, Context) when is_binary(Page); is_list(Page) ->
    try z_string:trim(unicode:characters_to_binary(Page)) of
        <<>> -> undefined;
        <<"#reload">> -> undefined;
        Url when is_binary(Url) -> z_context:site_url(Url, Context)
    catch
        error:_ -> undefined
    end;
normalize_redirect(_Page, _Context) ->
    undefined.

%% @doc Server-side signup API. Persist a destination on unverified email
%% identities before signup_done observers can send verification mail. Metadata
%% belongs to the identity, so resending a verification key preserves it.
%% The caller must already be authorized to create/update this user's signup.
-spec store_confirm_redirect(UserId, RequestPage, Context) -> Result when
    UserId :: m_rsc:resource_id(),
    RequestPage :: term(),
    Context :: z:context(),
    Result :: ok | {error, term()}.
store_confirm_redirect(UserId, RequestPage, Context) ->
    Page = normalize_redirect(RequestPage, Context),
    transaction_result(z_db:transaction(fun(Ctx) ->
        Identities = z_db:assoc("select id, propb from identity
            where rsc_id = $1 and type = 'email' and is_verified = false
            order by id for update", [UserId], Ctx),
        lists:foreach(fun(Identity) ->
            Id = proplists:get_value(id, Identity),
            Props = proplists:get_value(propb, Identity),
            Props1 = put_ready_page(Page, Props),
            case Props1 =:= Props of
                true -> ok;
                false -> ok = m_identity:set_propb(Id, Props1, Ctx)
            end
        end, Identities)
    end, Context)).

%% @doc Confirm using the verification key as authorization. Lock the identity
%% so verification and consumption of its pending destination happen only once.
%% This is deliberately not exposed as a model GET or POST operation.
-spec confirm(Key, Context) -> Result when
    Key :: binary(),
    Context :: z:context(),
    Result :: {ok, {m_rsc:resource_id(), binary(), binary(), binary() | undefined}}
            | {error, term()}.
confirm(Key, Context) when is_binary(Key), Key =/= <<>> ->
    Result = transaction_result(z_db:transaction(fun(Ctx) ->
        case z_db:assoc_row("select * from identity where verify_key = $1 for update", [Key], Ctx) of
            undefined -> {error, unknown_key};
            Identity -> confirm_identity(Identity, Ctx)
        end
    end, Context)),
    case Result of
        {ok, {UserId, _, _, _}} ->
            z_notifier:map(#signup_confirm{id = UserId}, Context),
            Result;
        {error, _} -> Result
    end;
confirm(_Key, _Context) ->
    {error, unknown_key}.

confirm_identity(Identity, Context) ->
    Id = proplists:get_value(id, Identity),
    UserId = proplists:get_value(rsc_id, Identity),
    Type = proplists:get_value(type, Identity),
    Key = proplists:get_value(key, Identity),
    Props = proplists:get_value(propb, Identity),
    Page = normalize_redirect(ready_page(Props), Context),
    {ok, UserId} = m_rsc:update(UserId, #{
        <<"is_published">> => true,
        <<"is_verified_account">> => true
    }, z_acl:sudo(Context)),
    ok = m_identity:set_verified(Id, Context),
    Props1 = put_ready_page(undefined, Props),
    case Props1 =:= Props of
        true -> ok;
        false -> ok = m_identity:set_propb(Id, Props1, Context)
    end,
    {ok, {UserId, Type, Key, Page}}.

ready_page(Props) when is_map(Props) -> maps:get(signup_ready_page, Props, undefined);
ready_page(Props) when is_list(Props) -> proplists:get_value(signup_ready_page, Props);
ready_page(_) -> undefined.

put_ready_page(undefined, Props) when is_map(Props) -> maps:remove(signup_ready_page, Props);
put_ready_page(undefined, Props) when is_list(Props) -> proplists:delete(signup_ready_page, Props);
put_ready_page(undefined, Props) -> Props;
put_ready_page(Page, undefined) -> #{signup_ready_page => Page};
put_ready_page(Page, Props) when is_map(Props) -> Props#{signup_ready_page => Page};
put_ready_page(Page, Props) when is_list(Props) ->
    [{signup_ready_page, Page} | proplists:delete(signup_ready_page, Props)].

transaction_result({rollback, Reason}) -> {error, Reason};
transaction_result(Result) -> Result.
