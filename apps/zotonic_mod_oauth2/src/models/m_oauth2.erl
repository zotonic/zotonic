%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019 Marc Worrell
%% @doc OAuth2 model

%% Copyright 2019 Marc Worrell
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

-module(m_oauth2).

-behaviour(zotonic_model).

-export([
    m_get/3,

    user_groups/1,
    list_tokens/2,
    get_token/2,

    insert/3,
    delete/2,

    encode_bearer_token/3,
    decode_bearer_token/2,

    get_token_access/2,
    oauth_key/1,

    manage_schema/2
]).

-define(TOKEN_PREFIX, "oauth2-").

m_get([ user_groups | Rest ], _Msg, Context) ->
    {ok, {user_groups(Context), Rest}};
m_get([ tokens, list ], Msg, Context) ->
    m_get([ tokens, list, me ], Msg, Context);
m_get([ tokens, list, me | Rest ], _Msg, Context) ->
    case list_tokens(z_acl:user(Context), Context) of
        {ok, List} -> {ok, {List, Rest}};
        {error, _} = Error -> Error
    end;
m_get([ tokens, list, UserId | Rest ], _Msg, Context) ->
    case list_tokens(m_rsc:rid(UserId, Context), Context) of
        {ok, List} -> {ok, {List, Rest}};
        {error, _} = Error -> Error
    end;
m_get([ tokens, TokenId | Rest ], _Msg, Context) ->
    {ok, {get_token(TokenId, Context), Rest}}.


-spec user_groups( z:context() ) -> {ok, [ m_rsc:resource_id() ]}.
user_groups(Context) ->
    {ok, z_acl:user_groups(Context)}.


-spec list_tokens( m_rsc:resource_id() | undefined, z:context() ) -> {ok, map()}.
list_tokens(UserId, Context) when is_integer(UserId) ->
    case is_allowed(UserId, Context) of
        true ->
            List = z_db:assoc_map("
                select id, user_id, is_read_only, ip_allowed, note, created, modified
                from oauth2_token
                where user_id = $1
                order by created desc",
                [ UserId ],
                Context),
            {ok, List};
        false ->
            {error, eacces}
    end;
list_tokens(undefined, _Context) ->
    {ok, []}.


-spec insert( m_rsc:resource_id(), map() | proplists:list(), z:context() ) -> {ok, integer()} | {error, eacces}.
insert( UserId, Props, Context ) when is_list(Props) ->
    case is_allowed(UserId, Context) of
        true ->
            z_db:transaction(
                fun(Ctx) ->
                    insert_trans(UserId, Props, Ctx)
                end,
                Context);
        false ->
            {error, eacces}
    end;
insert( UserId, Props, Context) when is_map(Props) ->
    insert(UserId, maps:to_list(Props), Context).

insert_trans(UserId, Props, Context) ->
    Secret = z_ids:id(32),
    Props1 = [
        {user_id, UserId},
        {is_read_only, z_convert:to_bool( proplists:get_value(is_read_only, Props, true) )},
        {is_full_access, z_convert:to_bool( proplists:get_value(is_full_access, Props, false) )},
        {ip_allowed, z_string:trim( z_convert:to_binary( proplists:get_value(ip_allowed, Props, <<>>) ) )},
        {note, z_html:escape( z_string:trim( proplists:get_value(note, Props, <<>>) ) )},
        {valid_till, proplists:get_value(valid_till, Props)},
        {secret, Secret}
    ],
    case z_db:insert(oauth2_token, Props1, Context) of
        {ok, TokenId} ->
            Groups = proplists:get_value(user_groups, Props, []),
            Groups1 = [ m_rsc:rid(GId, Context) || GId <- Groups ],
            Groups2 = filter_groups(Groups1, Context),
            lists:foreach(
                fun(GId) ->
                    z_db:q("
                        insert into oauth2_token_group (token_id, group_id)
                        values ($1, $2)",
                        [ TokenId, GId ],
                        Context)
                end,
                Groups2),
            {ok, TokenId};
        {error, _} = Error ->
            Error
    end.


-spec delete( integer(), z:context() ) -> ok | {error, notfound | eacces}.
delete( TokenId, Context ) when is_integer(TokenId) ->
    case z_db:q1("select user_id from oauth2_token where id = $1", [ TokenId ], Context) of
        undefined ->
            {error, notfound};
        UserId ->
            case is_allowed(UserId, Context) of
                true ->
                    z_db:q("delete from oauth2_token where id = $1", [ TokenId ], Context),
                    ok;
                false ->
                    {error, eacces}
            end
    end.

%% @doc Return the Bearer OAuth2 token for the given user
-spec encode_bearer_token( integer(), undefined | integer(), z:context() ) -> {ok, binary()} | {error, notfound | eacces}.
encode_bearer_token( TokenId, TTL, Context ) ->
    case get_token_access(TokenId, Context) of
        {ok, #{ user_id := UserId, secret := TokenSecret }} ->
            case is_allowed(UserId, Context) of
                true ->
                    Key = oauth_key(Context),
                    Term = {v1, TokenId, TokenSecret},
                    Enc = issue_token(Term, Key, TTL),
                    {ok, <<?TOKEN_PREFIX, Enc/binary>>};
                false ->
                    {error, eacces}
            end;
        {error, _} = Error ->
            Error
    end.

%5 @doc Issue token with optional expiration.
issue_token(Term, Key, undefined) ->
    termit:issue_token(Term, Key);
issue_token(Term, Key, TTL) when is_integer(TTL) ->
    termit:issue_token(Term, Key, TTL).

-spec decode_bearer_token(binary(), z:context()) -> {ok, {integer(), binary()}} | {error, unknown_token | expired | forged | badarg}.
decode_bearer_token(<<?TOKEN_PREFIX, Token/binary>>, Context) ->
    case termit:verify_token(Token, m_oauth2:oauth_key(Context)) of
        {ok, {v1, TokenId, TokenSecret}} ->
            {ok, {TokenId, TokenSecret}};
        {error, _Reason} = Error ->
            Error
    end;
decode_bearer_token(_Token, _Context) ->
    {error, unknown_token}.

-spec get_token( integer() | undefined, z:context() ) -> {ok, map()} | {error, notfound | eacces}.
get_token( TokenId, Context ) when is_integer(TokenId) ->
    case z_db:assoc_map("
        select id, user_id, is_read_only, is_full_access, ip_allowed, note, created, modified
        from oauth2_token
        where id = $1",
        [ z_convert:to_integer(TokenId) ],
        Context)
    of
        [] ->
            {error, notfound};
        [#{ user_id := UserId } = Token] ->
            case is_allowed(UserId, Context) of
                true ->
                    Log = z_db:assoc_map("
                            select *
                            from oauth2_token_log
                            where token_id = $1
                            order by id desc",
                            [ TokenId ],
                            Context),
                    Groups = z_db:q("
                            select group_id
                            from oauth2_token_group
                            where token_id = $1",
                            [ TokenId ],
                            Context),
                    Groups1 = [ Id || {Id} <- Groups ],
                    {ok, Token#{
                        log => Log,
                        user_groups => Groups1
                    }};
                false ->
                    {error, eacces}
            end
    end;
get_token( undefined, _Context ) ->
    {error, notfound}.


%% @doc Get the token details for authentication checks.
%% @todo Cache this for speeding up api requests
-spec get_token_access( integer(), z:context() ) -> {ok, map()} | {error, notfound}.
get_token_access(TokenId, Context) when is_integer(TokenId) ->
    case z_db:assoc_map("
        select user_id, is_read_only, is_full_access, ip_allowed, secret
        from oauth2_token
        where id = $1
          and (valid_till is null or valid_till > now())",
        [ z_convert:to_integer(TokenId) ],
        Context)
    of
        [] ->
            {error, notfound};
        [ Token ] ->
            Groups = z_db:q("
                    select group_id
                    from oauth2_token_group
                    where token_id = $1",
                    [ TokenId ],
                    Context),
            Groups1 = [ Id || {Id} <- Groups ],
            {ok, Token#{
                user_groups => Groups1
            }}
    end.


%% @doc Check if the current user is allowed to see the tokens of the given user.
-spec is_allowed( m_rsc:resource_id(), z:context() ) -> boolean().
is_allowed(UserId, Context) when is_integer(UserId) ->
    z_acl:user(Context) =:= UserId orelse z_acl:is_admin(Context).

%% @doc Filter the user groups for access
-spec filter_groups( [ m_rsc:resource_id() ], z:context() ) -> [ m_rsc:resource_id() ].
filter_groups(GIds, Context) when is_list(GIds) ->
    case z_acl:is_admin(Context) of
        true ->
            GIds;
        false ->
            {ok, UserGroups} = user_groups(Context),
            lists:filter(
                fun(GId) -> lists:member(GId, UserGroups) end,
                GIds)
    end.

%% @spec Return the secret site key used for symmetrically encrypting OAuth2 tokens.
-spec oauth_key( z:context() ) -> binary().
oauth_key(Context) ->
    case m_config:get_value(mod_oauth2, oauth_key, Context) of
        undefined ->
            Key = z_ids:id(64),
            m_config:set_value(mod_oauth2, oauth_key, Key, Context),
            Key;
        SignKey ->
            SignKey
    end.

-spec manage_schema( z_module_manager:manage_schema(), z:context() ) -> ok.
manage_schema(_Version,  Context) ->
   case z_db:table_exists(oauth2_tokens, Context) of
        false ->
            install_tables(Context);
        true ->
            ok
    end.

install_tables(Context) ->
    % All tokens and their signature secret
    [] = z_db:q("
        CREATE TABLE oauth2_token (
            id serial not null,
            user_id int not null,
            is_read_only boolean not null default true,
            is_full_access boolean not null default false,
            secret character varying(64) not null,
            ip_allowed character varying(500) not null default '*',
            valid_till timestamp with time zone,
            note text,
            created timestamp with time zone NOT NULL DEFAULT now(),
            modified timestamp with time zone NOT NULL DEFAULT now(),

            primary key (id),

            constraint fk_oauth2_token_user_id foreign key (user_id)
                references rsc(id)
                on update cascade
                on delete cascade
        )",
        Context),
    [] = z_db:q(
        "CREATE INDEX fki_oauth2_token_user_id ON oauth2_token (user_id)",
        Context),

    % ACL group resources the token gives access to
    [] = z_db:q("
        CREATE TABLE oauth2_token_group (
            token_id int not null,
            group_id int not null,

            primary key (token_id, group_id),

            constraint fk_oauth2_token_group_token_id foreign key (token_id)
                references oauth2_token(id)
                on update cascade
                on delete cascade,
            constraint fk_oauth2_token_group_group_id foreign key (group_id)
                references rsc(id)
                on update cascade
                on delete cascade
        )",
        Context),
    [] = z_db:q(
        "CREATE INDEX fki_oauth2_token_group_token_id ON oauth2_token_group (token_id)",
        Context),
    [] = z_db:q(
        "CREATE INDEX fki_oauth2_token_group_group_id ON oauth2_token_group (group_id)",
        Context),

    % A log of requests for this token (should be periodically pruned to keep only the last period)
    [] = z_db:q("
        CREATE TABLE oauth2_token_log (
            id bigserial not null,
            token_id int not null,
            remote_ip character varying(32) not null,
            request_path character varying(500) not null,
            created timestamp with time zone not null default current_timestamp,

            primary key(id),

            constraint fk_oauth2_token_group_token_id foreign key (token_id)
                references oauth2_token(id)
                on update cascade
                on delete cascade
        )",
        Context),
    [] = z_db:q(
        "CREATE INDEX fki_oauth2_token_log_token_id ON oauth2_token_log (token_id)",
        Context),
    [] = z_db:q(
        "CREATE INDEX oauth2_token_log_token_key ON oauth2_token_log (token_id, id)",
        Context),

    z_db:flush(Context).

