% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021-2025 Marc Worrell
%% @doc OAuth2 model managing consumers for access to remote sites. The user tokens
%% are inserted into the identity table. The type of the token is 'mod_oauth2' and
%% the key is a combination of the name of the consumer app and the remote user-id.
%% For fetched tokens using 'Client Credentials' the remote user-id is replaced with
%% the fixed CLIENT_CREDENTIALS_UID as there can only be a single token fetched in
%% this way (per consumer application).
%% @end

%% Copyright 2021-2025 Marc Worrell
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

-module(m_oauth2_consumer).

-export([
    m_get/3,

    list_consumers_auth/1,
    list_consumers_import/1,
    list_consumers_all/1,

    list_consumers/1,
    get_consumer/2,
    insert_consumer/2,
    delete_consumer/2,
    update_consumer/3,
    delete_consumer_tokens/2,

    get_consumer_oauth_service/2,

    fetch_token/3,
    find_token/3,
    insert_token/4,
    delete_token/3,
    delete_user_tokens/3,
    is_connected/2,
    is_connected/3,

    name_to_id/2,

    manage_schema/2,
    manage_data/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-type consumer_id() :: binary() | pos_integer() | undefined.
-export_type([consumer_id/0]).

-define(CLIENT_CREDENTIALS_UID, ":client credentials").

m_get([ <<"consumers">> ], _Msg, Context) ->
    case list_consumers(Context) of
        {ok, Apps} ->
            {ok, {Apps, []}};
        {error, _} = Error ->
            Error
    end;
m_get([ <<"consumers">>, <<"list">>, <<"auth">> | Rest ], _Msg, Context) ->
    case list_consumers_auth(Context) of
        {ok, Apps} ->
            {ok, {Apps, Rest}};
        {error, _} = Error ->
            Error
    end;
m_get([ <<"consumers">>, <<"list">>, <<"import">> | Rest ], _Msg, Context) ->
    case list_consumers_import(Context) of
        {ok, Apps} ->
            {ok, {Apps, Rest}};
        {error, _} = Error ->
            Error
    end;
m_get([ <<"consumers">>, <<"list">> | Rest ], _Msg, Context) ->
    case list_consumers_all(Context) of
        {ok, Apps} ->
            {ok, {Apps, Rest}};
        {error, _} = Error ->
            Error
    end;
m_get([ <<"consumers">>, ConsumerId, <<"tokens">> | Rest ], _Msg, Context) ->
    case list_consumer_tokens(z_convert:to_integer(ConsumerId), Context) of
        {ok, Tokens} ->
            {ok, {Tokens, Rest}};
        {error, _} = Error ->
            Error
    end;
m_get([ <<"consumers">>, ConsumerId | Rest ], _Msg, Context) ->
    case get_consumer(z_convert:to_integer(ConsumerId), Context) of
        {ok, App} ->
            {ok, {App, Rest}};
        {error, _} = Error ->
            Error
    end;
m_get([ <<"is_connected">>, Name | Rest ], _Msg, Context) ->
    {ok, {is_connected(Name, Context), Rest}}.


%% @doc List all consumers that can be used for authentication.
-spec list_consumers_auth( z:context() ) -> {ok, list( map() )} | {error, eacces | term()}.
list_consumers_auth(Context) ->
    z_db:qmap("
        select a.id, a.name, a.description, a.is_use_import, a.is_use_auth, a.domain
        from oauth2_consumer_app a
        where a.is_use_auth = true
          and a.grant_type <> 'client_credentials'
          and a.app_secret <> ''
          and a.app_code <> ''
        order by description",
        Context).

%% @doc List all consumers that can be used for import of data.
-spec list_consumers_import( z:context() ) -> {ok, list( map() )} | {error, eacces | term()}.
list_consumers_import(Context) ->
    z_db:qmap("
        select a.id, a.name, a.description, a.is_use_import, a.is_use_auth, a.domain,
               a.app_secret <> '' and a.app_code <> '' as has_credentials
        from oauth2_consumer_app a
        where a.is_use_import = true
          and a.grant_type <> 'client_credentials'
        order by description",
        Context).

%% @doc List all consumers that can be used for authentication or import.
-spec list_consumers_all( z:context() ) -> {ok, list( map() )} | {error, eacces | term()}.
list_consumers_all(Context) ->
    z_db:qmap("
        select a.id, a.name, a.description, a.is_use_import, a.is_use_auth, a.domain, a.grant_type,
               a.app_secret <> '' and a.app_code <> '' as has_credentials
        from oauth2_consumer_app a
        where (   a.is_use_auth = true
               or a.is_use_import = true)
          and a.grant_type <> 'client_credentials'
        order by description",
        Context).

%% @doc List all consumers, the secrets of these consumers are copied from the remote site.
%% Tokens are coupled to a consumer. Consumers are coupled to a user, if the user
%% is deleted then all their consumers and tokens are deleted. Consumers should be registered by
%% a user with config rights, as such all admin users can see all apps.
-spec list_consumers( z:context() ) -> {ok, list( map() )} | {error, eacces | term()}.
list_consumers(Context) ->
    case z_acl:is_admin(Context) of
        true ->
            z_db:qmap("
                select a.id, a.name, a.is_use_import, a.is_use_auth, a.user_id, a.domain,
                       a.grant_type, a.description, a.created, a.modified,
                       (select count(*) from identity t where t.type = 'mod_oauth2' and t.key like a.name || ':%') as token_count,
                       a.app_secret <> '' and a.app_code <> '' as has_credentials
                from oauth2_consumer_app a
                order by created desc",
                Context);
        false ->
            {error, eacces}
    end.

%% @doc Get a specific app, return also the token count for the app.
-spec get_consumer( ConsumerId :: consumer_id(), z:context() ) -> {ok, map()} | {error, eacces | term()}.
get_consumer(ConsumerId, Context) ->
    case z_acl:is_admin(Context) of
        true ->
            z_db:qmap_row("
                select a.*,
                       a.app_secret <> '' and a.app_code <> '' as has_credentials,
                       (select count(*) from identity t where t.type = 'mod_oauth2' and t.key like a.name || ':%') as token_count
                from oauth2_consumer_app a
                where a.id = $1",
                [ name_to_id(ConsumerId, Context) ],
                Context);
        false ->
            z_db:qmap_row("
                select a.id, a.name, a.description, a.domain
                from oauth2_consumer_app a
                where a.id = $1",
                [ name_to_id(ConsumerId, Context) ],
                Context)
    end.


%% @doc Get basic consumer info for the oauth flow.
-spec get_consumer_oauth_service( ConsumerId :: consumer_id(), z:context() ) -> {ok, map()} | {error, eacces | term()}.
get_consumer_oauth_service(ConsumerId, Context) ->
    z_db:qmap_row("
        select name, access_token_url, domain, app_code, app_secret
        from oauth2_consumer_app a
        where a.id = $1",
        [ name_to_id(ConsumerId, Context) ],
        Context).


%% @doc Insert a new Consumer.
-spec insert_consumer( ConsumerDetails :: map(), z:context() ) -> {ok, ConsumerId :: integer()} | {error, term()}.
insert_consumer(Map, Context) ->
    case z_acl:is_admin_editable(Context) of
        true ->
            Name = maps:get(<<"name">>, Map, <<>>),
            Consumer = #{
                <<"name">> => Name,
                <<"user_id">> => maps:get(<<"user_id">>, Map, z_acl:user(Context)),
                <<"is_use_auth">> => maps:get(<<"is_use_auth">>, Map, true),
                <<"is_use_import">> => maps:get(<<"is_use_import">>, Map, true),
                <<"description">> => maps:get(<<"description">>, Map, <<"Untitled">>),
                <<"domain">> => maps:get(<<"domain">>, Map, <<>>),
                <<"app_code">> => maps:get(<<"app_code">>, Map, <<>>),
                <<"app_secret">> => maps:get(<<"app_secret">>, Map, <<>>),
                <<"authorize_url">> => maps:get(<<"authorize_url">>, Map, <<>>),
                <<"access_token_url">> => maps:get(<<"access_token_url">>, Map, <<>>),
                <<"grant_type">> => maps:get(<<"grant_type">>, Map, <<"authorization_code">>)
            },
            case z_db:q1("select count(*) from oauth2_consumer_app where name = $1", [ Name ], Context) of
                0 ->
                    z_db:insert(oauth2_consumer_app, Consumer, Context);
                1 ->
                    {error, duplicate_name}
            end;
        false ->
            {error, eacces}
    end.

%% @doc Delete an App. All associated tokens are deleted as well.
-spec delete_consumer( ConsumerId :: consumer_id(), z:context() ) -> ok | {error, term()}.
delete_consumer(ConsumerId, Context) ->
    case z_acl:is_admin_editable(Context) of
        true ->
            AppId = name_to_id(ConsumerId, Context),
            AppName = id_to_name(AppId, Context),
            case z_db:delete(oauth2_consumer_app, AppId, Context) of
                {ok, 1} ->
                    delete_consumer_tokens_1(undefined, AppName, Context);
                {ok, 0} ->
                    {error, enoent};
                {error, _} = Error ->
                    Error
            end;
        false ->
            {error, eacces}
    end.

%% @doc Update an App's is_enabled flag and description.
-spec update_consumer( ConsumerId :: consumer_id(), map(), z:context() ) -> ok | {error, term()}.
update_consumer(ConsumerId, Map, Context) ->
    case z_acl:is_admin_editable(Context) of
        true ->
            Consumer = maps:without([<<"modified">>, <<"created">>, <<"user_id">>], Map),
            Consumer1 = Consumer#{
                <<"modified">> => calendar:universal_time()
            },
            case z_db:update(oauth2_consumer_app, name_to_id(ConsumerId, Context), Consumer1, Context) of
                {ok, 1} -> ok;
                {ok, 0} -> {error, enoent};
                {error, _} = Error -> Error
            end;
        false ->
            {error, eacces}
    end.

%% @doc Insert a Client Credentials or manual token to access the remote server by the user.
%% Any existing consumer token for the user is replaced. The current user must be authenticated
%% as an admin.
-spec insert_token(ConsumerId, UserId, AccessToken, Context) -> ok | {error, term()} when
    ConsumerId :: consumer_id(),
    UserId :: m_rsc:resource_id(),
    AccessToken :: binary(),
    Context :: z:context().
insert_token(ConsumerId, UserId, AccessToken, Context) ->
    insert_token(ConsumerId, UserId, AccessToken, undefined, Context).


%% @doc Insert a manual token for access to the remote server by the user. Any existing
%% token for the consumer is replaced. The current user MUST be authenticated as an admin.
-spec insert_token(ConsumerId, UserId, AccessToken, Expires, Context) -> ok | {error, term()} when
    ConsumerId :: consumer_id(),
    UserId :: m_rsc:resource_id(),
    AccessToken :: binary(),
    Expires :: undefined | calendar:datetime(),
    Context :: z:context().
insert_token(ConsumerId, UserId, AccessToken, Expires, Context) ->
    case z_acl:is_admin(Context) of
        true ->
            case z_db:qmap_row("
                select name, domain, access_token_url, app_code, app_secret, grant_type
                from oauth2_consumer_app
                where id = $1
                ",
                [ name_to_id(ConsumerId, Context) ],
                Context)
            of
                {ok, #{
                    <<"name">> := AppName,
                    <<"domain">> := Domain
                }} ->
                    Type = <<"mod_oauth2">>,
                    Props = [
                        {is_verified, true},
                        {propb, {term, #{ <<"access_token">> => AccessToken }}},
                        {expires, Expires}
                    ],
                    delete_user_tokens(ConsumerId, UserId, Context),
                    {ok, IdnId} = m_identity:insert(UserId, Type, AppName, Props, Context),
                    ?LOG_INFO(#{
                        text => <<"OAuth2 added new client_credentials token">>,
                        in => zotonic_mod_oauth2,
                        user_id => z_acl:user(Context),
                        result => ok,
                        name => AppName,
                        consumer_id => ConsumerId,
                        domain => Domain,
                        idn_user_id => UserId,
                        idn_id => IdnId,
                        idn_type => Type,
                        expires => Expires
                    }),
                    ok;
                {error, _} = Error ->
                    Error
            end;
        false ->
            {error, eacces}
    end.

%% @doc List all tokens registered with a consumer.
-spec list_consumer_tokens(ConsumerId, Context) -> {ok, list( map() )} | {error, term()} when
    ConsumerId :: consumer_id(),
    Context :: z:context().
list_consumer_tokens(ConsumerId, Context) ->
    case z_acl:is_admin(Context) of
        true ->
            ConsumerId1 = name_to_id(ConsumerId, Context),
            case id_to_name(ConsumerId1, Context) of
                undefined ->
                    {error, enoent};
                ConsumerName ->
                    z_db:qmap("
                        select id, rsc_id as user_id, expires, created, modified
                        from identity
                        where type = 'mod_oauth2'
                          and (key like $1 || ':%' or key = $1)
                        order by modified desc",
                        [ ConsumerName ],
                        Context)
            end;
        false ->
            {error, eacces}
    end.

%% @doc Delete a token connected with a consumer.
-spec delete_consumer_tokens(ConsumerId, Context) -> ok | {error, term()} when
    ConsumerId :: consumer_id(),
    Context :: z:context().
delete_consumer_tokens(ConsumerId, Context) ->
    case z_acl:is_admin(Context) of
        true ->
            ConsumerId1 = name_to_id(ConsumerId, Context),
            ConsumerName = id_to_name(ConsumerId1, Context),
            delete_consumer_tokens_1(undefined, ConsumerName, Context);
        false ->
            {error, eacces}
    end.

delete_consumer_tokens_1(undefined, ConsumerName, Context) ->
    Idns = z_db:q("
        select id
        from identity
        where type = 'mod_oauth2'
          and (key like $1 || ':%' or key = $1)",
        [ ConsumerName ],
        Context),
    lists:foreach(
        fun({IdnId}) ->
            m_identity:delete(IdnId, Context)
        end,
        Idns);
delete_consumer_tokens_1(UserId, ConsumerName, Context) ->
    Idns = z_db:q("
        select id
        from identity
        where type = 'mod_oauth2'
          and (key like $1 || ':%' or key = $1)
          and rsc_id = $2",
        [ ConsumerName, UserId ],
        Context),
    lists:foreach(
        fun({IdnId}) ->
            m_identity:delete(IdnId, Context)
        end,
        Idns).


%% @doc Delete a token connected with a consumer.
-spec delete_token(ConsumerId, TokenId, Context) -> ok | {error, term()} when
    ConsumerId :: consumer_id(),
    TokenId :: integer(),
    Context :: z:context().
delete_token(ConsumerId, TokenId, Context) ->
    case z_acl:is_admin(Context) of
        true ->
            ConsumerId1 = name_to_id(ConsumerId, Context),
            case id_to_name(ConsumerId1, Context) of
                undefined ->
                    {error, enoent};
                ConsumerName ->
                    Idns = z_db:q("
                        select id
                        from identity
                        where type = 'mod_oauth2'
                          and (key like $1 || ':%' or key = $1)
                          and id = $2",
                        [ ConsumerName, TokenId ],
                        Context),
                    lists:foreach(
                        fun({IdnId}) ->
                            m_identity:delete(IdnId, Context)
                        end,
                        Idns)
            end;
        false ->
            {error, eacces}
    end.

%% @doc Delete a token connected with a consumer.
-spec delete_user_tokens(ConsumerId, UserId, Context) -> ok | {error, term()} when
    ConsumerId :: consumer_id(),
    UserId :: m_rsc:resource_id(),
    Context :: z:context().
delete_user_tokens(ConsumerId, UserId, Context) ->
    case z_acl:is_admin(Context) of
        true ->
            ConsumerId1 = name_to_id(ConsumerId, Context),
            case id_to_name(ConsumerId1, Context) of
                undefined ->
                    {error, enoent};
                ConsumerName ->
                    Idns = z_db:q("
                        select id
                        from identity
                        where type = 'mod_oauth2'
                          and (key like $1 || ':%' or key = $1)
                          and rsc_id = $2",
                        [ ConsumerName, UserId ],
                        Context),
                    lists:foreach(
                        fun({IdnId}) ->
                            m_identity:delete(IdnId, Context)
                        end,
                        Idns)
            end;
        false ->
            {error, eacces}
    end.

%% @doc Fetch a token from the remote server for the user. The grant_flow MUST be of the
%% type 'Client Credentials'.  Any existing user token is replaced. The current user MUST be
%% authenticated as an admin.
-spec fetch_token(ConsumerId :: consumer_id(), UserId :: m_rsc:resource_id(), z:context() ) ->
    {ok, AccessToken :: binary()} | {error, term()}.
fetch_token(ConsumerId, UserId, Context) ->
    case z_acl:is_admin(Context) of
        true ->
            case z_db:qmap_row("
                select name, domain, access_token_url, app_code, app_secret, grant_type
                from oauth2_consumer_app
                where id = $1
                ",
                [ name_to_id(ConsumerId, Context) ],
                Context)
            of
                {ok, #{
                    <<"name">> := AppName,
                    <<"grant_type">> := <<"client_credentials">>,
                    <<"domain">> := Domain,
                    <<"access_token_url">> := TokenUrl0,
                    <<"app_code">> := AppCode,
                    <<"app_secret">> := AppSecret
                }} ->
                    TokenUrl = case z_string:trim(TokenUrl0) of
                        <<>> ->
                            ContextNoLang = z_context:set_language('x-default', Context),
                            iolist_to_binary([
                                "https://", Domain,
                                z_dispatcher:url_for(oauth2_server_access_token, ContextNoLang)
                            ]);
                        TU ->
                            TU
                    end,
                    Payload = #{
                        <<"client_id">> => AppCode,
                        <<"client_secret">> => AppSecret,
                        <<"grant_type">> => <<"client_credentials">>
                    },
                    Options = [
                        {authorization, none}
                    ],
                    Type = <<"mod_oauth2">>,
                    Key = <<AppName/binary, $:, ?CLIENT_CREDENTIALS_UID>>,
                    case z_fetch:fetch_json(post, TokenUrl, Payload, Options, Context) of
                        {ok, #{
                            <<"access_token">> := AccessToken
                            % <<"expires_in">> := ExpiresInSecs
                            % <<"token_type">> := <<"Bearer">>
                            % <<"refresh_token">> := _
                        } = Response} ->
                            Expires = expires(Response),
                            Props = [
                                {is_verified, true},
                                {propb, {term, #{ <<"access_token">> => AccessToken }}},
                                {expires, Expires}
                            ],
                            delete_consumer_tokens_1(UserId, AppName, Context),
                            {ok, IdnId} = m_identity:insert(UserId, Type, Key, Props, Context),
                            ?LOG_INFO(#{
                                text => <<"OAuth2 fetched new client_credentials token">>,
                                in => zotonic_mod_oauth2,
                                result => ok,
                                user_id => UserId,
                                name => AppName,
                                consumer_id => ConsumerId,
                                domain => Domain,
                                idn_id => IdnId,
                                idn_type => Type,
                                idn_key => Key,
                                expires => Expires
                            }),
                            {ok, AccessToken};
                        {ok, Ret} ->
                            delete_consumer_tokens_1(UserId, AppName, Context),
                            ?LOG_ERROR(#{
                                text => <<"OAuth2 could not fetch client_credentials token">>,
                                in => zotonic_mod_oauth2,
                                result => error,
                                reason => no_access_token,
                                user_id => UserId,
                                name => AppName,
                                consumer_id => ConsumerId,
                                client_id => AppCode,
                                domain => Domain,
                                payload => Ret
                            }),
                            {error, no_access_token};
                        {error, Reason} = Error ->
                            case is_permanent_error(Reason) of
                                true ->
                                    delete_consumer_tokens_1(UserId, AppName, Context);
                                false ->
                                    ok
                            end,
                            ?LOG_ERROR(#{
                                text => <<"OAuth2 could not fetch client_credentials token">>,
                                in => zotonic_mod_oauth2,
                                result => error,
                                reason => Reason,
                                user_id => UserId,
                                name => AppName,
                                consumer_id => ConsumerId,
                                client_id => AppCode,
                                domain => Domain
                            }),
                            Error
                    end;
                {ok, _} ->
                    {error, grant_type};
                {error, _} = Error ->
                    Error
            end;
        false ->
            {error, eacces}
    end.

expires(#{ <<"expires_in">> := ExpiresInSecs }) when is_integer(ExpiresInSecs) ->
    z_datetime:timestamp_to_datetime( z_datetime:timestamp() + ExpiresInSecs );
expires(#{}) ->
    undefined.

is_permanent_error({Code, _FinalUrl, _Hs, _Size, _Body})
    when Code =:= 400;
         Code =:= 401;
         Code =:= 403 ->
    true;
is_permanent_error({_Code, _FinalUrl, _Hs, _Size, _Body}) ->
    false;
is_permanent_error(timeout) ->
    false;
is_permanent_error(_) ->
    true.

%% @doc Fetch id of consumer with the given name.
name_to_id(undefined, _Context) ->
    undefined;
name_to_id(Name, Context) when is_binary(Name) ->
    case z_db:q1("select id from oauth2_consumer_app where name = $1", [ Name ], Context) of
        undefined ->
            case z_utils:only_digits(Name) of
                true -> binary_to_integer(Name);
                false -> undefined
            end;
        AppId ->
            AppId
    end;
name_to_id(Id, _Context) when is_integer(Id) ->
    Id.

%% @doc Fetch name of consumer with the given id.
id_to_name(Id, Context) ->
    z_db:q1("select name from oauth2_consumer_app where id = $1", [ Id ], Context).

%% @doc Find an access token for the given user / host combination. The corresponding consumer
%% app must be marked for import usage. The identity property must be map with an access_token
%% key.
-spec find_token( UserId :: m_rsc:resource_id(), Host :: binary(), z:context() ) ->
    {ok, binary()} | {error, term()}.
find_token(UserId, Host, Context) ->
    Now = z_datetime:prev_second(calendar:universal_time(), 10),
    case find_token1(UserId, Host, Context) of
        {_AppId, #{ <<"access_token">> := AccesToken }, Expires, _, _} when
            Expires =:= undefined;
            Expires >= Now
        ->
            {ok, AccesToken};
        {AppId, #{ <<"access_token">> := _ }, Expires, <<"client_credentials">>, true} when
            Expires < Now
        ->
            % Try to fetch a new token.
            fetch_token(AppId, UserId, z_acl:sudo(Context));
        _ ->
            {error, enoent}
    end.

find_token1(UserId, Host, Context) ->
    z_db:q_row("
        select app.id, idn.propb, idn.expires,
               app.grant_type, app.is_extend_automatic
        from identity idn,
             oauth2_consumer_app app
        where idn.rsc_id = $1
          and idn.type = 'mod_oauth2'
          and app.domain = $2
          and app.is_use_import
          and app.name = split_part(idn.key, ':', 1)
        limit 1
        ",
        [ UserId, Host ],
        Context).

%% @doc Check if the current user is connected to the OAuth2 service with the given name.
%% This only checks the presence of the correct identity key, it does not check if the
%% key contains a valid access_token.
-spec is_connected( Name :: binary(), z:context() ) -> boolean().
is_connected(Name, Context) ->
    is_connected(z_acl:user(Context), Name, Context).

%% @doc Check if the user is connected to the OAuth2 service with the given name.
%% This only checks the presence of the correct identity key, it does not check if the
%% key contains a valid access_token.
-spec is_connected( UserId :: m_rsc:resource_id() | undefined, Name :: binary(), z:context() ) -> boolean().
is_connected(undefined, _Name, _Context) ->
    false;
is_connected(UserId, Name, Context) ->
    case z_db:q1("
        select count(*)
        from identity
        where rsc_id = $1
          and type = 'mod_oauth2'
          and key like $2 || ':%'",
        [ UserId, Name ],
        Context)
    of
        0 -> false;
        _ -> true
    end.


%% @doc Install the datamodel - keep track of OAuth2 consumer apps.
-spec manage_schema( z_module_manager:manage_schema(), z:context() ) -> ok.
manage_schema(_Version,  Context) ->
    case z_db:table_exists(oauth2_consumer_app, Context) of
        false ->
            [] = z_db:q("
                create table oauth2_consumer_app (
                    id serial not null,
                    name varchar(255) not null,
                    is_use_import boolean not null default false,
                    is_use_auth boolean not null default false,
                    app_code varchar(128),
                    app_secret varchar(128),
                    domain varchar(255) not null,
                    authorize_url varchar(255) not null,
                    access_token_url varchar(255) not null,
                    description varchar(255),
                    grant_type character varying (32) not null default 'authorization_code',
                    is_extend_automatic boolean not null default false,
                    user_id int,
                    created timestamp with time zone not null default now(),
                    modified timestamp with time zone not null default now(),

                    primary key (id),
                    constraint oauth2_consumer_app_name_key unique (name),
                    constraint fk_oauth2_consumer_app_user_id foreign key (user_id)
                        references rsc(id)
                        on update cascade
                        on delete cascade
                )
                ",
                Context),
            [] = z_db:q(
                "CREATE INDEX fki_oauth2_consumer_app_user_id ON oauth2_consumer_app (user_id)",
                Context),
            [] = z_db:q(
                "CREATE INDEX oauth2_consumer_app_domain_key ON oauth2_consumer_app (domain)",
                Context),
            [] = z_db:q(
                "CREATE INDEX oauth2_consumer_app_grant_type_key ON oauth2_consumer_app (grant_type, is_extend_automatic)",
                Context),
            z_db:flush(Context);
        true ->
            case z_db:column(oauth2_consumer_app, app_code, Context) of
                {ok, #column_def{ length = 32 }} ->
                    [] = z_db:q("
                        alter table oauth2_consumer_app
                        alter column app_code type character varying(128),
                        alter column app_secret type character varying(128)",
                        Context),
                    z_db:flush(Context);
                {ok, _} ->
                    ok
            end,
            case z_db:column(oauth2_consumer_app, grant_type, Context) of
                {ok, #column_def{ default = <<"'code'::", _/binary>> }} ->
                    [] = z_db:q("
                        alter table oauth2_consumer_app
                        alter column grant_type set default 'authorization_code'",
                        Context),
                    ok;
                {ok, #column_def{}} ->
                    ok;
                {error, enoent} ->
                    [] = z_db:q("
                        alter table oauth2_consumer_app
                        add column grant_type character varying (32) not null default 'authorization_code'",
                        Context),
                    z_db:flush(Context)
            end,
            case z_db:column_exists(oauth2_consumer_app, is_extend_automatic, Context) of
                true ->
                    ok;
                false ->
                    [] = z_db:q("
                        alter table oauth2_consumer_app
                        add column is_extend_automatic boolean not null default false",
                        Context),
                    [] = z_db:q(
                        "CREATE INDEX oauth2_consumer_app_grant_type_key ON oauth2_consumer_app (grant_type, is_extend_automatic)",
                        Context),
                    z_db:flush(Context)
            end
    end.

manage_data({version, 12}, Context) ->
    % Update the idn key for the client-credentials fetched keys.
    Apps = z_db:q("
        select name, app_code
        from oauth2_consumer_app
        where grant_type = 'client_credentials'",
        Context),
    lists:foreach(
        fun({AppName, AppCode}) ->
            OldKey = <<AppName/binary, $:, AppCode/binary>>,
            NewKey = <<AppName/binary, $:, ?CLIENT_CREDENTIALS_UID>>,
            AllKeysPrefix = <<AppName/binary, ":%">>,
            IdnId = z_db:q1("
                select id
                from identity
                where type = 'mod_oauth2'
                  and key = any($1)",
                [ [ OldKey, NewKey ] ],
                Context),
            if
                is_integer(IdnId) ->
                    z_db:q("
                        delete from identity
                        where type = 'mod_oauth2'
                          and key like $1
                          and id <> $2",
                        [ AllKeysPrefix, IdnId ],
                        Context),
                    z_db:q("
                        update identity
                        set key = $1
                        where id = $2",
                        [ NewKey, IdnId ],
                        Context);
                true ->
                    z_db:q("
                        delete from identity
                        where type = 'mod_oauth2'
                          and key like $1",
                        [ AllKeysPrefix ],
                        Context),
                    ok
            end
        end,
        Apps);
manage_data(_, _Context) ->
    ok.

