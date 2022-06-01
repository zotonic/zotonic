%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021 Marc Worrell
%% @doc OAuth2 model managing consumers for access to remote sites.

%% Copyright 2021 Marc Worrell
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

    get_consumer_oauth_service/2,

    find_token/3,
    is_connected/2,
    is_connected/3,

    manage_schema/2
]).

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
        order by description",
        Context).

%% @doc List all consumers that can be used for import of data.
-spec list_consumers_import( z:context() ) -> {ok, list( map() )} | {error, eacces | term()}.
list_consumers_import(Context) ->
    z_db:qmap("
        select a.id, a.name, a.description, a.is_use_import, a.is_use_auth, a.domain
        from oauth2_consumer_app a
        where a.is_use_import = true
        order by description",
        Context).

%% @doc List all consumers that can be used for authentication or import.
-spec list_consumers_all( z:context() ) -> {ok, list( map() )} | {error, eacces | term()}.
list_consumers_all(Context) ->
    z_db:qmap("
        select a.id, a.name, a.description, a.is_use_import, a.is_use_auth, a.domain
        from oauth2_consumer_app a
        where a.is_use_auth = true
           or a.is_use_import = true
        order by description",
        Context).

%% @doc List all consumers, the secrets of these consumers are copied from the remote site.
%% Tokens are coupled to a consumer. Consumers are coupled to an user, if the user
%% is deleted then all their consumers and tokens are deleted. Consumers should be registered by
%% an user with config rights, as such all admin users can see all apps.
-spec list_consumers( z:context() ) -> {ok, list( map() )} | {error, eacces | term()}.
list_consumers(Context) ->
    case z_acl:is_admin(Context) of
        true ->
            z_db:qmap("
                select a.id, a.name, a.is_use_import, a.is_use_auth, a.user_id, a.domain,
                       a.description, a.created, a.modified,
                       (select count(*) from identity t where t.type = 'mod_oauth2' and t.key like a.name || ':%') as token_count
                from oauth2_consumer_app a
                order by created desc",
                Context);
        false ->
            {error, eacces}
    end.

%% @doc Get a specific app, return also the token count for the app.
-spec get_consumer( ConsumerId :: integer(), z:context() ) -> {ok, map()} | {error, eacces | term()}.
get_consumer(ConsumerId, Context) ->
    case z_acl:is_admin(Context) of
        true ->
            z_db:qmap_row("
                select a.*,
                       (select count(*) from identity t where t.type = 'mod_oauth2' and t.key like a.name || ':%') as token_count
                from oauth2_consumer_app a
                where a.id = $1",
                [ ConsumerId ],
                Context);
        false ->
            z_db:qmap_row("
                select a.id, a.name, a.description, a.domain
                from oauth2_consumer_app a
                where a.id = $1",
                [ ConsumerId ],
                Context)
    end.


%% @doc Get basic consumer info for the oauth flow.
-spec get_consumer_oauth_service( ConsumerId :: integer(), z:context() ) -> {ok, map()} | {error, eacces | term()}.
get_consumer_oauth_service(ConsumerId, Context) ->
    z_db:qmap_row("
        select name, access_token_url, domain, app_code, app_secret
        from oauth2_consumer_app a
        where a.id = $1",
        [ ConsumerId ],
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
                <<"access_token_url">> => maps:get(<<"access_token_url">>, Map, <<>>)
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
-spec delete_consumer( ConsumerId :: integer(), z:context() ) -> ok | {error, term()}.
delete_consumer(ConsumerId, Context) ->
    case z_acl:is_admin_editable(Context) of
        true ->
            case z_db:delete(oauth2_consumer_app, ConsumerId, Context) of
                {ok, 1} -> ok;
                {ok, 0} -> {error, enoent};
                {error, _} = Error -> Error
            end;
        false ->
            {error, eacces}
    end.

%% @doc Update an App's is_enabled flag and description.
-spec update_consumer( ConsumerId :: integer(), map(), z:context() ) -> ok | {error, term()}.
update_consumer(ConsumerId, Map, Context) ->
    case z_acl:is_admin_editable(Context) of
        true ->
            Consumer = maps:without([<<"modified">>, <<"created">>, <<"use_id">>], Map),
            Consumer1 = Consumer#{
                <<"modified">> => calendar:universal_time()
            },
            case z_db:update(oauth2_consumer_app, ConsumerId, Consumer1, Context) of
                {ok, 1} -> ok;
                {ok, 0} -> {error, enoent};
                {error, _} = Error -> Error
            end;
        false ->
            {error, eacces}
    end.

%% @doc Find an access token for the given user / host combination. The corresponding consumer
%% app must be marked for import usage. The identity property must be map with an access_token
%% key.
-spec find_token( UserId :: m_rsc:resource_id(), Host :: binary(), z:context() ) ->
    {ok, binary()} | {error, term()}.
find_token(UserId, Host, Context) ->
    case z_db:q1("
        select idn.propb
        from identity idn,
             oauth2_consumer_app app
        where idn.rsc_id = $1
          and app.domain = $2
          and app.is_use_import
          and app.name = split_part(idn.key, ':', 1)
        limit 1
        ",
        [ UserId, Host ],
        Context)
    of
        #{
            <<"access_token">> := AccesToken
        } ->
            {ok, AccesToken};
        _ ->
            {error, enoent}
    end.

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
                    app_code varchar(32),
                    app_secret varchar(32),
                    domain varchar(255) not null,
                    authorize_url varchar(255) not null,
                    access_token_url varchar(255) not null,
                    description varchar(255),
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

            z_db:flush(Context);
        true ->
            ok
    end.
