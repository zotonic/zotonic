%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012 Marc Worrell
%% @doc Manage a resource's revisions.

%% Copyright 2012 Marc Worrell
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

-module(m_backup_revision).

-behaviour(gen_model).

-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,

    save_revision/3,
    get_revision/2,
    list_revisions/2,
    list_revisions_assoc/2,
    install/1
]).

-include("zotonic.hrl").

-define(BACKUP_TYPE_PROPS, $P).

m_find_value(list, #m{value=undefined} = M, _Context) ->
    M#m{value=list};
m_find_value(Id, #m{value=list}, Context) when is_integer(Id) ->
    list_revisions_assoc(Id, Context);
m_find_value(_X, #m{}, _Context) ->
    undefined.

m_to_list(_m, _Context) ->
    [].

m_value(_m, _Context) ->
    undefined.



save_revision(Id, Props, Context) ->
    Version = proplists:get_value(version, Props),
    LastVersion = z_db:q1("select version from backup_revision where rsc_id = $1 order by created desc limit 1", [Id], Context),
    case Version of
        LastVersion when LastVersion =/= undefined ->
            ok;
        _ ->
            1 = z_db:q("
                insert into backup_revision 
                    (rsc_id, type, version, user_id, user_name, data_type, data)
                values ($1, $2, $3, $4, $5, $6, $7)
                ", [
                    Id, 
                    ?BACKUP_TYPE_PROPS,
                    proplists:get_value(version, Props),
                    z_acl:user(Context),
                    z_string:truncate(m_rsc:p_no_acl(z_acl:user(Context), title, Context), 60),
                    "erlang",
                    erlang:term_to_binary(Props, [compressed])
                ],
                Context),
            ok = prune_revisions(Id, Context),
            ok
    end.

get_revision(RevId, Context) ->
    case z_db:assoc_row("select * from backup_revision where id = $1", [RevId], Context) of
        undefined -> 
            {error, notfound};
        Row ->
            R1 = proplists:delete(data, Row),
            {ok, [ {data, erlang:binary_to_term(proplists:get_value(data, Row)) } | R1 ]}
    end.

list_revisions(Id, Context) ->
    z_db:q("
        select id, type, created, version, user_id, user_name
        from backup_revision
        where rsc_id = $1
        order by created desc", [Id], Context).

list_revisions_assoc(Id, Context) ->
    z_db:assoc("
        select id, type, created, version, user_id, user_name
        from backup_revision
        where rsc_id = $1
        order by created desc", [Id], Context).


%% @doc Prune the old revisions in the database. Drops revisions close to each other.
prune_revisions(Id, Context) ->
    % TODO
    ok.



%% @doc Install the revisions table.
install(Context) ->
    case z_db:table_exists(backup_revision, Context) of
        false ->
            [] = z_db:q("
                    create table backup_revision (
                        id bigserial not null,
                        type character(1) not null,
                        rsc_id integer not null,
                        created timestamp not null default current_timestamp,
                        version integer,
                        user_id integer,
                        user_name character varying(80),
                        filename character varying(400),
                        note character varying(200),
                        data_type character varying(10) not null,
                        data bytea not null,

                        primary key (id)
                    )
                ", Context),
            [] = z_db:q("
                    create index backup_revision_id_created on backup_revision (rsc_id, created)
                ", Context),
            ok;
        true ->
            ok
    end.
