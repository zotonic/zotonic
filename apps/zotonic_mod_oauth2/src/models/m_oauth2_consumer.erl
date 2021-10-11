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

    list_consumers/1,
    get_consumer/2,
    insert_consumer/2,
    delete_consumer/2,
    update_consumer/3,

    manage_schema/2,

    reinstall_tables/0
]).

m_get([ <<"consumers">> ], _Msg, Context) ->
    case list_consumers(Context) of
        {ok, Apps} ->
            {ok, {Apps, []}};
        {error, _} = Error ->
            Error
    end;
m_get([ <<"consumers">>, ConsumerId | Rest ], _Msg, Context) ->
    case get_consumer(z_convert:to_integer(ConsumerId), Context) of
        {ok, App} ->
            {ok, {App, Rest}};
        {error, _} = Error ->
            Error
    end.


%% @doc List all consumers, the secrets of these consumers are copied from the remote site.
%% Tokens are coupled to a consumer. Consumers are coupled to an user, if the user
%% is deleted then all their consumers and tokens are deleted. Consumers should be registered by
%% an user with config rights, as such all admin users can see all apps.
-spec list_consumers( z:context() ) -> {ok, list( map() )} | {error, eacces | term()}.
list_consumers(Context) ->
    case z_acl:is_admin(Context) of
        true ->
            z_db:qmap("
                select a.id, a.is_use_import, a.is_use_auth, a.user_id, a.domain,
                       a.description, a.created, a.modified,
                       (select count(*) from oauth2_consumer_token t where a.id = t.consumer_app_id) as token_count
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
                       (select count(*) from oauth2_consumer_token t where a.id = t.consumer_app_id) as token_count
                from oauth2_consumer_app a
                where a.id = $1",
                [ ConsumerId ],
                Context);
        false ->
            {error, eacces}
    end.



%% @doc Insert a new Consumer.
-spec insert_consumer( ConsumerDetails :: map(), z:context() ) -> {ok, ConsumerId :: integer()} | {error, term()}.
insert_consumer(Map, Context) ->
    case z_acl:is_admin(Context) of
        true ->
            Consumer = #{
                <<"user_id">> => maps:get(<<"user_id">>, Map, z_acl:user(Context)),
                <<"is_use_auth">> => maps:get(<<"is_use_auth">>, Map, true),
                <<"is_use_import">> => maps:get(<<"is_use_import">>, Map, true),
                <<"description">> => maps:get(<<"description">>, Map, <<"Untitled">>),
                <<"domain">> => maps:get(<<"domain">>, Map, <<>>),
                <<"app_code">> => maps:get(<<"app_code">>, Map, <<>>),
                <<"app_secret">> => maps:get(<<"app_secret">>, Map, <<>>),
                <<"authorize_url">> => maps:get(<<"authorize_url">>, Map, <<>>),
                <<"redirect_url">> => maps:get(<<"redirect_url">>, Map, <<>>)
            },
            z_db:insert(oauth2_consumer_app, Consumer, Context);
        false ->
            {error, eacces}
    end.

%% @doc Delete an App. All associated tokens are deleted as well.
-spec delete_consumer( ConsumerId :: integer(), z:context() ) -> ok | {error, term()}.
delete_consumer(ConsumerId, Context) ->
    case z_acl:is_admin(Context) of
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
    case z_acl:is_admin(Context) of
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


%% @doc Install the datamodel - keep track of OAuth2 consumer apps.
-spec manage_schema( z_module_manager:manage_schema(), z:context() ) -> ok.
manage_schema(_Version,  Context) ->
    case z_db:table_exists(oauth2_consumer_app, Context) of
        false ->
            [] = z_db:q("
                create table oauth2_consumer_app (
                    id serial not null,
                    is_use_import boolean not null default false,
                    is_use_auth boolean not null default false,
                    app_code varchar(32),
                    app_secret varchar(32),
                    domain varchar(255) not null,
                    authorize_url varchar(255) not null,
                    redirect_url varchar(255) not null,
                    description varchar(255),
                    user_id int,
                    created timestamp with time zone not null default now(),
                    modified timestamp with time zone not null default now(),

                    primary key (id),

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


            [] = z_db:q("
                create table oauth2_consumer_token (
                    id serial not null,
                    consumer_app_id int not null,
                    user_id int not null,
                    token_type varchar(32) not null default 'Bearer',
                    token text not null,
                    created timestamp with time zone not null default now(),
                    modified timestamp with time zone not null default now(),

                    primary key (id),

                    constraint fk_oauth2_consumer_token_consumer_app_id foreign key (consumer_app_id)
                        references oauth2_consumer_app(id)
                        on update cascade
                        on delete cascade,

                    constraint fk_oauth2_consumer_token_user_id foreign key (user_id)
                        references rsc(id)
                        on update cascade
                        on delete cascade
                )
                ",
                Context),
            [] = z_db:q(
                "CREATE INDEX fki_oauth2_consumer_token_consumer_app_id ON oauth2_consumer_token (consumer_app_id)",
                Context),
            [] = z_db:q(
                "CREATE INDEX fki_oauth2_consumer_token_user_id ON oauth2_consumer_token (user_id)",
                Context),

            z_db:flush(Context);
        true ->
            ok
    end.


reinstall_tables() ->
    z_sites_manager:foreach(
        fun(Context) ->
            z_db:q("drop table if exists oauth2_consumer_app cascade", Context),
            z_db:q("drop table if exists oauth2_consumer_token cascade", Context),
            z_db:flush(Context),
            manage_schema(install, Context)
        end).

