%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019-2025 Marc Worrell
%% @doc OAuth2 (https://tools.ietf.org/html/draft-ietf-oauth-v2-26)
%% @end

%% Copyright 2019-2025 Marc Worrell
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
-mod_schema(11).
-mod_depends([ authentication ]).

-export([
    event/2,
    observe_request_context/3,
    observe_url_fetch_options/2,
    observe_admin_menu/3,
    observe_tick_3h/2,
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
event(#submit{ message={oauth2_app_token_new, [ {app_id, AppId} ]} }, Context) ->
    TPs = #{
        <<"is_read_only">> => z_convert:to_bool(z_context:get_q(<<"is_read_only">>, Context) ),
        <<"is_full_access">> => true,
        <<"note">> => z_convert:to_binary(z_context:get_q(<<"note">>, Context))
    },
    case m_rsc:rid(z_context:get_q(<<"user_id">>, Context), Context) of
        undefined ->
            z_render:growl(?__("Please select a user.", Context), Context);
        UserId ->
            Label = z_string:trim(z_convert:to_binary(z_context:get_q(<<"label">>, Context))),
            case m_oauth2:insert_token(AppId, UserId, Label, TPs, Context) of
                {ok, TId} ->
                    {ok, Token} = m_oauth2:encode_bearer_token(TId, undefined, Context),
                    z_render:dialog(
                        ?__("New access token", Context),
                        "_dialog_oauth2_app_token_view.tpl",
                        [
                            {app_id, AppId},
                            {token, Token},
                            {backdrop, static},
                            {action, {reload, []}}
                        ],
                        Context);
                {error, _} ->
                    z_render:growl_error(?__("Could not generate the access token.", Context), Context)
            end
    end;
event(#postback{ message={oauth2_app_token_delete, [ {token_id, TokenId} ]} }, Context) ->
    case m_oauth2:delete_token(TokenId, Context) of
        ok ->
            z_render:wire({reload, []}, Context);
        {error, _} ->
            z_render:growl_error(?__("Could not delete the token.", Context), Context)
    end;
event(#postback{ message={oauth2_app_token_generate, [ {app_id, AppId} ]} }, Context) ->
    TPs = #{
        <<"is_read_only">> => false,
        <<"is_full_access">> => true,
        <<"note">> => ?__("Generated using the admin interface", Context)
    },
    case m_oauth2:insert_token(AppId, z_acl:user(Context), undefined, TPs, Context) of
        {ok, TId} ->
            {ok, Token} = m_oauth2:encode_bearer_token(TId, undefined, Context),
            z_render:dialog(
                ?__("New access token", Context),
                "_dialog_oauth2_app_token_view.tpl",
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
        <<"app_code">> => z_string:trim(z_context:get_q(<<"app_code">>, Context)),
        <<"app_secret">> => z_string:trim(z_context:get_q(<<"app_secret">>, Context)),
        <<"is_use_auth">> => z_convert:to_bool(z_context:get_q(<<"is_use_auth">>, Context)),
        <<"is_use_import">> => z_convert:to_bool(z_context:get_q(<<"is_use_import">>, Context)),
        <<"authorize_url">> => z_string:trim(z_context:get_q(<<"authorize_url">>, Context)),
        <<"access_token_url">> => z_string:trim(z_context:get_q(<<"access_token_url">>, Context)),
        <<"grant_type">> => z_string:trim(z_context:get_q(<<"grant_type">>, Context)),
        <<"is_extend_automatic">> => z_convert:to_bool(z_context:get_q(<<"is_extend_automatic">>, Context))
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
        <<"app_code">> => z_string:trim(z_context:get_q(<<"app_code">>, Context)),
        <<"app_secret">> => z_string:trim(z_context:get_q(<<"app_secret">>, Context)),
        <<"is_use_auth">> => z_convert:to_bool(z_context:get_q(<<"is_use_auth">>, Context)),
        <<"is_use_import">> => z_convert:to_bool(z_context:get_q(<<"is_use_import">>, Context)),
        <<"authorize_url">> => z_string:trim(z_context:get_q(<<"authorize_url">>, Context)),
        <<"access_token_url">> => z_string:trim(z_context:get_q(<<"access_token_url">>, Context)),
        <<"grant_type">> => z_string:trim(z_context:get_q(<<"grant_type">>, Context)),
        <<"is_extend_automatic">> => z_convert:to_bool(z_context:get_q(<<"is_extend_automatic">>, Context))
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
    end;
event(#submit{ message={oauth2_consumer_token_new, [ {app_id, AppId} ]} }, Context) ->
    case m_rsc:rid(z_context:get_q(<<"user_id">>, Context), Context) of
        undefined ->
            z_render:growl(?__("Please select a user.", Context), Context);
        UserId ->
            case z_context:get_q(<<"z_submitter">>, Context) of
                <<"fetch">> ->
                    z_render:wire({confirm, [
                        {text, iolist_to_binary([
                            ?__("Fetch an access token to this site?", Context),
                            "<br>",
                            ?__("The token will be registered with the selected user account.", Context),
                            "<br><br>",
                            ?__("Fetching a token can take some time.", Context)
                        ])},
                        {ok, ?__("Fetch Token", Context)},
                        {postback, {oauth2_fetch_consumer_token, [ {app_id, AppId}, {user_id, UserId} ]}},
                        {delegate, mod_oauth2}
                    ]}, Context);
                _ ->
                    case z_string:trim(z_convert:to_binary(z_context:get_q(<<"token">>, Context))) of
                        <<>> ->
                            Context;
                        Token ->
                            case m_oauth2_consumer:insert_token(AppId, UserId, Token, Context) of
                                ok ->
                                    z_render:wire({reload, []}, Context);
                                {error, _} ->
                                    z_render:growl_error(?__("Could not save the access token.", Context), Context)
                            end
                    end
            end
    end;
event(#postback{ message={oauth2_consumer_token_delete, Args} }, Context) ->
    {id, TokenId} = proplists:lookup(id, Args),
    {app_id, AppId} = proplists:lookup(app_id, Args),
    case m_oauth2_consumer:delete_token(AppId, TokenId, Context) of
        ok ->
            z_render:wire({reload, []}, Context);
        {error, _} ->
            z_render:growl_error(?__("Could not delete the token.", Context), Context)
    end;
event(#postback{ message={oauth2_fetch_consumer_token, Args} }, Context) ->
    case z_acl:is_admin(Context) of
        true ->
            {app_id, AppId} = proplists:lookup(app_id, Args),
            {user_id, UserId} = proplists:lookup(user_id, Args),
            case m_oauth2_consumer:fetch_token(AppId, UserId, Context) of
                {ok, _AccessToken} ->
                    ?LOG_INFO(#{
                        text => <<"Fetched new consumer token">>,
                        in => mod_oauth2,
                        result => ok,
                        app_id => AppId,
                        user_id => UserId
                    }),
                    z_render:wire([
                            {alert, [
                                {title, ?__("Success", Context)},
                                {text, ?__("Fetched a new access token.", Context)},
                                {action, {reload, []}}
                            ]}
                        ], Context);
                {error, Reason} ->
                    ?LOG_ERROR(#{
                        text => <<"Could not fetch a new consumer token">>,
                        in => mod_oauth2,
                        result => error,
                        reason => Reason,
                        app_id => AppId,
                        user_id => UserId
                    }),
                    ReasonText = iolist_to_binary(io_lib:format("~p", [ Reason ])),
                    z_render:wire([
                            {alert, [
                                {text, [
                                    ?__("Could not fetch a new access token.", Context),
                                    " (", z_html:escape(ReasonText), ")"
                                ]}
                            ]}
                        ], Context)
            end;
        false ->
            z_render:growl_error(?__("You are not allowed to fetch a consumer token.", Context), Context)
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
    case proplists:is_defined(authorization, Options) of
        false ->
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
        true ->
            undefined
    end;
observe_url_fetch_options(_, _Context) ->
    undefined.


%% @doc Periodically try to extend tokens that are expiring in the next 8 hours.
observe_tick_3h(tick_3h, Context) ->
    Next8H = z_datetime:next_hour(calendar:universal_time(), 8),
    Expiring = z_db:q("
        select id, rsc_id, key
        from identity
        where expires > now()
          and expires < $1
          and type = 'mod_oauth2'
        ",
        [ Next8H ],
        Context),
    lists:foreach(
        fun({_IdnId, RscId, Key}) ->
            case binary:split(Key, <<":">>) of
                [Name, RId] ->
                    AppId = m_oauth2_consumer:name_to_id(Name, Context),
                    case m_rsc:rid(RId, Context) of
                        RscId ->
                            case m_oauth2_consumer:fetch_token(Name, RscId, z_acl:sudo(Context)) of
                                {ok, _} ->
                                    ?LOG_INFO(#{
                                        text => <<"Fetched new consumer token">>,
                                        in => mod_oauth2,
                                        result => ok,
                                        app_id => AppId,
                                        user_id => RscId
                                    });
                                {error, Reason} ->
                                    ?LOG_ERROR(#{
                                        text => <<"Could not fetch a new consumer token">>,
                                        in => mod_oauth2,
                                        result => error,
                                        reason => Reason,
                                        app_id => AppId,
                                        user_id => RscId
                                    })
                            end;
                        _ ->
                            ok
                    end;
                _ ->
                    ok
            end
        end,
        Expiring).

observe_admin_menu(#admin_menu{}, Acc, Context) ->
     [
     #menu_item{id=admin_oauth2_apps,
                parent=admin_auth,
                label=?__("OAuth2 Applications", Context),
                url={admin_oauth2_apps, []},
                visiblecheck={acl, use, mod_admin_config}},
     #menu_item{id=admin_oauth2_consumers,
                parent=admin_auth,
                label=?__("OAuth2 Clients", Context),
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
            ?LOG_NOTICE(#{
                text => <<"Could not decode OAuth2 token">>,
                in => zotonic_mod_oauth2,
                result => error,
                reason => Reason,
                token => Token
            }),
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
            ?LOG_NOTICE(#{
                text => <<"Authenticated OAuth2 request for disabled user">>,
                in => zotonic_mod_oauth2,
                user_id => UserId,
                result => error,
                reason => disabled,
                token_id => TokenId
            }),
            Context
    end.


-spec manage_schema( z_module_manager:manage_schema(), z:context() ) -> ok.
manage_schema(Version, Context) ->
    m_oauth2:manage_schema(Version, Context),
    m_oauth2_consumer:manage_schema(Version, Context).

