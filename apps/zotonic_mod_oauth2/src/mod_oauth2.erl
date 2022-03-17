%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019-2021 Marc Worrell
%% @doc OAuth2 (https://tools.ietf.org/html/draft-ietf-oauth-v2-26)

%% Copyright 2019-2021 Marc Worrell
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

-module(mod_oauth2).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("OAuth2").
-mod_description("Provides authentication over OAuth2.").
-mod_prio(900).
-mod_schema(6).
-mod_depends([ authentication ]).

-export([
    event/2,
    observe_request_context/3,
    observe_url_fetch_options/2,
    observe_admin_menu/3,
    manage_schema/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").


event(#submit{ message={oauth2_authorize, Args}}, Context) ->
    {client_id, ClientId} = proplists:lookup(client_id, Args),
    {redirect_uri, RedirectUri} = proplists:lookup(redirect_uri, Args),
    {state, State} = proplists:lookup(state, Args),
    {response_type, _ResponseType} = proplists:lookup(response_type, Args),
    {scope, Scope} = proplists:lookup(scope, Args),
    Redirect = case z_context:get_q(<<"accept">>, Context) of
        <<>> ->
            oauth_authorize_accept(ClientId, RedirectUri, Scope, State, Context);
        undefined ->
            oauth_authorize_cancel(RedirectUri, State)
    end,
    z_render:wire({redirect, [ {location, Redirect} ]}, Context);

event(#submit{ message={oauth2_app_insert, []} }, Context) ->
    App = #{
        <<"user_id">> => z_acl:user(Context),
        <<"description">> => z_string:trim(z_context:get_q_validated(<<"description">>, Context)),
        <<"is_enabled">> => z_convert:to_bool(z_context:get_q(<<"is_enabled">>, Context)),
        <<"redirect_urls">> => z_string:trim(z_context:get_q(<<"redirect_urls">>, Context))
    },
    case m_oauth2:insert_app(App, Context) of
        {ok, _AppId} ->
            z_render:wire({redirect, [ {dispatch, admin_oauth2_apps} ]}, Context);
        {error, _} ->
            z_render:growl_error(?__("Could not insert the App.", Context), Context)
    end;
event(#submit{ message={oauth2_app_update, [ {app_id, AppId} ]} }, Context) ->
    App = #{
        <<"description">> => z_string:trim(z_context:get_q_validated(<<"description">>, Context)),
        <<"is_enabled">> => z_convert:to_bool(z_context:get_q(<<"is_enabled">>, Context)),
        <<"redirect_urls">> => z_string:trim(z_context:get_q(<<"redirect_urls">>, Context))
    },
    case m_oauth2:update_app(AppId, App, Context) of
        ok ->
            z_render:wire({redirect, [ {dispatch, admin_oauth2_apps} ]}, Context);
        {error, _} ->
            z_render:growl_error(?__("Could not insert the App.", Context), Context)
    end;
event(#postback{ message={oauth2_app_delete, [ {app_id, AppId} ]} }, Context) ->
    case m_oauth2:delete_app(AppId, Context) of
        ok ->
            z_render:wire({redirect, [ {dispatch, admin_oauth2_apps} ]}, Context);
        {error, _} ->
            z_render:growl_error(?__("Could not insert the App.", Context), Context)
    end;
event(#postback{ message={oauth2_app_token_generate, [ {app_id, AppId} ]} }, Context) ->
    TPs = #{
        <<"is_read_only">> => false,
        <<"is_full_access">> => true,
        <<"note">> => ?__("Generated using the admin interface", Context)
    },
    case m_oauth2:insert_token(AppId, z_acl:user(Context), TPs, Context) of
        {ok, TId} ->
            {ok, Token} = m_oauth2:encode_bearer_token(TId, undefined, Context),
            z_render:dialog(
                ?__("New access token", Context),
                "_dialog_oauth2_app_token.tpl",
                [
                    {app_id, AppId},
                    {token, Token}
                ],
                Context);
        {error, _} ->
            z_render:growl_error(?__("Could not generate the access token.", Context), Context)
    end;
event(#submit{ message={oauth2_consumer_insert, []} }, Context) ->
    Consumer = #{
        <<"name">> => z_string:trim(z_context:get_q_validated(<<"name">>, Context)),
        <<"user_id">> => z_acl:user(Context),
        <<"description">> => z_string:trim(z_context:get_q_validated(<<"description">>, Context)),
        <<"domain">> => z_string:to_lower(z_string:trim(z_context:get_q_validated(<<"domain">>, Context))),
        <<"app_code">> => z_string:trim(z_context:get_q_validated(<<"app_code">>, Context)),
        <<"app_secret">> => z_string:trim(z_context:get_q_validated(<<"app_secret">>, Context)),
        <<"is_use_auth">> => z_convert:to_bool(z_context:get_q(<<"is_use_auth">>, Context)),
        <<"is_use_import">> => z_convert:to_bool(z_context:get_q(<<"is_use_import">>, Context)),
        <<"authorize_url">> => z_string:trim(z_context:get_q(<<"authorize_url">>, Context)),
        <<"access_token_url">> => z_string:trim(z_context:get_q(<<"access_token_url">>, Context))
    },
    case m_oauth2_consumer:insert_consumer(Consumer, Context) of
        {ok, _AppId} ->
            z_render:wire({redirect, [ {dispatch, admin_oauth2_consumers} ]}, Context);
        {error, duplicate_name} ->
            z_render:growl_error(?__("An OAuth2 consumer with this name already exsists, please use another name.", Context), Context);
        {error, _} ->
            z_render:growl_error(?__("Could not insert the Consumer.", Context), Context)
    end;
event(#submit{ message={oauth2_consumer_update, [ {app_id, AppId} ]} }, Context) ->
    Consumer = #{
        <<"description">> => z_string:trim(z_context:get_q_validated(<<"description">>, Context)),
        <<"domain">> => z_string:to_lower(z_string:trim(z_context:get_q_validated(<<"domain">>, Context))),
        <<"app_code">> => z_string:trim(z_context:get_q_validated(<<"app_code">>, Context)),
        <<"app_secret">> => z_string:trim(z_context:get_q_validated(<<"app_secret">>, Context)),
        <<"is_use_auth">> => z_convert:to_bool(z_context:get_q(<<"is_use_auth">>, Context)),
        <<"is_use_import">> => z_convert:to_bool(z_context:get_q(<<"is_use_import">>, Context)),
        <<"authorize_url">> => z_string:trim(z_context:get_q(<<"authorize_url">>, Context)),
        <<"access_token_url">> => z_string:trim(z_context:get_q(<<"access_token_url">>, Context))
    },
    case m_oauth2_consumer:update_consumer(AppId, Consumer, Context) of
        ok ->
            z_render:wire({redirect, [ {dispatch, admin_oauth2_consumers} ]}, Context);
        {error, _} ->
            z_render:growl_error(?__("Could not update the Consumer.", Context), Context)
    end;
event(#postback{ message={oauth2_consumer_delete, [ {app_id, AppId} ]} }, Context) ->
    case m_oauth2_consumer:delete_consumer(AppId, Context) of
        ok ->
            z_render:wire({redirect, [ {dispatch, admin_oauth2_consumers} ]}, Context);
        {error, _} ->
            z_render:growl_error(?__("Could not insert the Consumer.", Context), Context)
    end.


oauth_authorize_accept(ClientId, RedirectUri, Scope, State, Context) ->
    {ok, Code} = m_oauth2:encode_accept_code(ClientId, RedirectUri, Scope, Context),
    Parsed = uri_string:parse(RedirectUri),
    Qs = case maps:find(query, Parsed) of
        {ok, Q} -> <<"?", Q/binary, $&>>;
        error -> <<"?">>
    end,
    Qs1 = iolist_to_binary([
                Qs, <<"&state=">>, cow_qs:urlencode(State),
                <<"&code=">>, cow_qs:urlencode(Code)
                ]),
    combine_url(Parsed#{ query => Qs1 }).

oauth_authorize_cancel(RedirectUri, State) ->
    Parsed = uri_string:parse(RedirectUri),
    Qs = case maps:find(query, Parsed) of
        {ok, Q} -> <<"?", Q/binary, $&>>;
        error -> <<"?">>
    end,
    Qs1 = iolist_to_binary([
                Qs, <<"state=">>, cow_qs:urlencode(State),
                <<"&error=access_denied">>,
                <<"&error_reason=user_denied">>,
                <<"&error_description=The+user+denied+your+request">>
                ]),
    combine_url(Parsed#{ query => Qs1 }).

combine_url(#{
        scheme := Scheme,
        host := Host,
        path := Path,
        query := Qs
    } = Parsed) ->
    iolist_to_binary([
        Scheme, "://", Host,
        case maps:find(port, Parsed) of
            {ok, Port} -> [ ":", integer_to_binary(Port) ];
            error -> <<>>
        end,
        Path,
        Qs,
        case maps:find(fragment, Parsed) of
            {ok, Frag} -> [ "#", Frag ];
            error -> <<>>
        end
        ]).

%% @doc Check if there is a valid Authorization header or 'access_token' argument.
-spec observe_request_context( #request_context{}, z:context(), z:context() ) -> z:context().
observe_request_context(#request_context{ phase = init }, Context, _Context) ->
    case z_context:get(anonymous, Context, false) of
        true ->
            Context;
        false ->
            case z_auth:is_auth(Context) of
                true ->
                    Context;
                false ->
                    try_auth(Context)
            end
    end;
observe_request_context(#request_context{ phase = _Phase }, Context, _Context) ->
    Context.

%% @doc Check if the current user has a token for the given host. If so then
%% add it to the headers for the fetch request.
observe_url_fetch_options(#url_fetch_options{
                    url = <<"https:", _/binary>>,
                    host = Host,
                    options = Options
                }, Context) ->
    case z_acl:user(Context) of
        UserId when is_integer(UserId) ->
            case m_oauth2_consumer:find_token(UserId, Host, Context) of
                {ok, AccessToken} ->
                    [
                        {authorization, <<"Bearer ", AccessToken/binary>>}
                        | Options
                    ];
                {error, _} ->
                    undefined
            end;
        _ ->
            undefined
    end;
observe_url_fetch_options(_, _Context) ->
    undefined.



observe_admin_menu(#admin_menu{}, Acc, Context) ->
     [
     #menu_item{id=admin_oauth2_apps,
                parent=admin_auth,
                label=?__("OAuth2 Applications", Context),
                url={admin_oauth2_apps, []},
                visiblecheck={acl, use, mod_admin_config}},
     #menu_item{id=admin_oauth2_consumers,
                parent=admin_auth,
                label=?__("OAuth2 Consumer Tokens", Context),
                url={admin_oauth2_consumers, []}}
    | Acc ].

try_auth(Context) ->
    case cowmachine_req:get_req_header(<<"authorization">>, Context) of
        <<"Bearer ", Token/binary>> ->
            try_bearer(Token, Context);
        <<"bearer ", Token/binary>> ->
            try_bearer(Token, Context);
        _ ->
            case z_context:get_q(<<"access_token">>, Context) of
                undefined ->
                    Context;
                Token when is_binary(Token) ->
                    try_bearer(Token, Context)
            end
    end.

try_bearer(<<>>, Context) ->
    Context;
try_bearer(<<" ", Token/binary>>, Context) ->
    try_bearer(Token, Context);
try_bearer(Token, Context) ->
    case m_oauth2:decode_bearer_token(Token, Context) of
        {ok, TokenMap} ->
            try_token(TokenMap, Context);
        {error, unknown_token} ->
            % Somebody else's token - ignore
            Context;
        {error, Reason} ->
            % Illegal token, maybe throw a 400 here?
            ?LOG_NOTICE("Could not decode OAuth2 token, error ~p for ~p", [ Reason, Token ]),
            Context
    end.

try_token(#{
        <<"id">> := TokenId,
        <<"user_id">> := UserId,
        <<"user_groups">> := UserGroups,
        <<"is_read_only">> := IsReadOnly,
        <<"is_full_access">> := IsFullAccess
    }, Context) ->
    Options = case IsFullAccess of
        true ->
            % No restriction on user groups
            #{
                is_read_only => IsReadOnly
            };
        false ->
            % Limited access, user groups will be filtered
            #{
                user_groups => UserGroups,
                is_read_only => IsReadOnly
            }
    end,
    case z_auth:is_enabled(UserId, Context) of
        true ->
            z_acl:logon(UserId, Options, Context);
        false ->
            % User is disabled, maybe throw a 403 here?
            ?LOG_NOTICE("Authenticated OAuth2 request for disabled user ~p with token ~p", [ UserId, TokenId ]),
            Context
    end.


-spec manage_schema( z_module_manager:manage_schema(), z:context() ) -> ok.
manage_schema(Version, Context) ->
    m_oauth2:manage_schema(Version, Context),
    m_oauth2_consumer:manage_schema(Version, Context).

