%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012-2023 Marc Worrell
%% @doc Model for administration of deleted resources and their possible new location.
%% @end

%% Copyright 2012-2023 Marc Worrell
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

-module(m_rsc_gone).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(zotonic_model).

%% interface functions
-export([
    m_get/3,

    get/2,
    get_uri/2,
    get_new_location/2,
    is_gone/2,
    is_gone_uri/2,
    gone/2,
    gone/3,

    delete/2,

    install/1
]).

-include_lib("zotonic.hrl").
-include_lib("epgsql/include/epgsql.hrl").


%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ Id, <<"new_location">> | Rest ], _Msg, Context) ->
    {ok, {get_new_location(Id, Context), Rest}};
m_get([ Id, <<"is_gone">> | Rest ], _Msg, Context) ->
    {ok, {is_gone(Id, Context), Rest}};
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.

%% @doc Get the possible 'rsc_gone' resource for the id.
get(Id, Context) when is_integer(Id) ->
    F = fun() ->
        z_db:assoc_row("select * from rsc_gone where id = $1", [Id], Context)
    end,
    z_depcache:memo(F, {rsc_gone, Id}, Context);
get(Id, Context) ->
    get(m_rsc:rid(Id, Context), Context).


%% @doc Get the possible 'rsc_gone' resource for the uri.
-spec get_uri( binary() | string(), z:context() ) -> proplists:proplist() | undefined.
get_uri(Uri, Context) when is_binary(Uri); is_list(Uri) ->
    z_db:assoc_row("select * from rsc_gone where uri = $1", [Uri], Context);
get_uri(_, _Context) ->
    undefined.


%% @doc Get the redirect location for the id, uses the current dispatch rule and otherwise
%%      the 'id' dispatch rule.
get_new_location(undefined, _Context) ->
    undefined;
get_new_location(Id, Context) when is_integer(Id) ->
    F = fun() ->
            z_db:q_row("select new_id, new_uri from rsc_gone where id = $1 limit 1", [Id], Context)
        end,
    case z_depcache:memo(F, {rsc_gone_new_location, Id}, Context) of
        undefined ->
            undefined;
        {undefined, undefined} ->
            undefined;
        {NewId, _} when is_integer(NewId) ->
            NewUri = case z_context:get(zotonic_dispatch, Context) of
                         undefined ->
                             z_dispatcher:url_for(id, [{id, NewId}], Context);
                         Dispatch ->
                             case z_dispatcher:url_for(Dispatch, [{id, NewId}], Context) of
                                 undefined -> z_dispatcher:url_for(id, [{id, NewId}], Context);
                                 DispUri -> DispUri
                             end
                     end,
            z_context:abs_url(NewUri, Context);
        {undefined, NewUri} ->
            z_context:abs_url(NewUri, Context)
    end.


%% @doc Check if the resource used to exist.
-spec is_gone(m_rsc:resource_id()|undefined, z:context()) -> boolean().
is_gone(undefined, _Context) ->
    false;
is_gone(Id, Context) when is_integer(Id) ->
    F = fun() ->
            z_db:q1("select count(*) from rsc_gone where id = $1", [Id], Context) =:= 1
        end,
    z_depcache:memo(F, {rsc_is_gone, Id}, Context).

%% @doc Check if the resource uri used to exist.
-spec is_gone_uri(string()|binary()|undefined, z:context()) -> boolean().
is_gone_uri(undefined, _Context) ->
    false;
is_gone_uri(Uri, Context) ->
    UriB = unicode:characters_to_binary(Uri, utf8),
    F = fun() ->
            z_db:q1("select count(*) from rsc_gone where uri = $1", [UriB], Context) >= 1
        end,
    z_depcache:memo(F, {rsc_is_gone, UriB}, Context).


%% @doc Copy a resource to the 'gone' table, use the current user as the modifier (deleter).
-spec gone(m_rsc:resource_id(), z:context()) -> {ok, integer()} | {error, term()}.
gone(Id, Context) when is_integer(Id) ->
    gone(Id, undefined, Context).

%% @doc Copy a resource to the 'gone' table, use the current user as the modifier (deleter).
%%      Also sets the 'new id', which is the id that replaces the deleted id.
gone(Id, NewId, Context) when is_integer(Id), is_integer(NewId) orelse NewId =:= undefined ->
    case z_db:assoc_row("
            select id, name, version, page_path, uri, is_authoritative, creator_id, created
            from rsc
            where id = $1
            ", [Id], Context)
    of
        undefined ->
            {error, notfound};
        Props when is_list(Props) ->
            Result = z_db:transaction(
                    fun(Ctx) ->
                        Props1 = [
                            {new_id, NewId},
                            {new_uri, undefined},
                            {modifier_id, z_acl:user(Ctx)},
                            {is_personal_data, m_identity:is_user(Id, Context)}
                            | Props
                        ],
                        case z_db:q1("select count(*) from rsc_gone where id = $1", [Id], Ctx) of
                            1 ->
                                {ok, _} = z_db:update(rsc_gone, Id, Props1, Ctx),
                                {ok, Id};
                            0 ->
                                % Force previous versions to refer to the new resource
                                z_db:q("
                                    update rsc_gone
                                    set new_id = $1
                                    where new_id = $2
                                    ",
                                    [ NewId, Id ],
                                    Ctx),
                                z_db:insert(rsc_gone, Props1, Ctx)
                        end
                    end,
                    Context),
            case Result of
                {error, #error{ codename = unique_violation }} ->
                    % Duplicate key - ignore (race condition)
                    {ok, Id};
                Other -> Other
            end
    end.

%% @doc Delete a gone entry for a resource, used after recovery of a resource.
-spec delete(Id, Context) -> ok | {error, enoent} when
    Id :: m_rsc:resource_id(),
    Context :: z:context().
delete(Id, Context) ->
    case z_db:q("delete from rsc_gone where id = $1", [ Id ], Context) of
        1 -> ok;
        0 -> {error, enoent}
    end.

%% @doc Install or upgrade the rsc_gone table.
-spec install( z:context() ) -> ok.
install(Context) ->
    % Table: rsc_gone
    % Tracks deleted or moved resources, adding "410 gone" support
    % Also contains new id or new url for 301 moved permanently replies.
    % mod_backup is needed to recover a deleted resource's content.
    case z_db:table_exists(rsc_gone, Context) of
        false ->
            [] = z_db:q("
                CREATE TABLE IF NOT EXISTS rsc_gone
                (
                    id bigint not null,
                    new_id bigint,
                    new_uri character varying(2048),
                    version int not null,
                    uri character varying(2048),
                    name character varying(80),
                    page_path character varying(80),
                    is_authoritative boolean NOT NULL DEFAULT true,
                    creator_id bigint,
                    modifier_id bigint,
                    created timestamp with time zone NOT NULL DEFAULT now(),
                    modified timestamp with time zone NOT NULL DEFAULT now(),
                    is_personal_data boolean NOT NULL DEFAULT false,
                    CONSTRAINT rsc_gone_pkey PRIMARY KEY (id)
                )",
                Context),

            [] = z_db:q("CREATE INDEX IF NOT EXISTS rsc_gone_name_key ON rsc_gone(name)", Context),
            [] = z_db:q("CREATE INDEX IF NOT EXISTS rsc_gone_uri_key ON rsc_gone(uri)", Context),
            [] = z_db:q("CREATE INDEX IF NOT EXISTS rsc_gone_page_path_key ON rsc_gone(page_path)", Context),
            [] = z_db:q("CREATE INDEX IF NOT EXISTS rsc_gone_modified_key ON rsc_gone(modified)", Context),
            z_db:flush(Context),
            ok;
        true ->
            % Check for rsc_gone_uri_key
            case z_db:key_exists(rsc_gone, rsc_gone_uri_key, Context) of
                false ->
                    [] = z_db:q("CREATE INDEX IF NOT EXISTS rsc_gone_uri_key ON rsc_gone(uri)", Context),
                    ok;
                true ->
                    ok
            end,
            % Check for is_personal_data column
            case z_db:column_exists(rsc_gone, is_personal_data, Context) of
                false ->
                    [] = z_db:q("ALTER TABLE rsc_gone ADD COLUMN is_personal_data boolean NOT NULL DEFAULT false", Context),
                    z_db:flush(Context);
                true ->
                    ok
            end
    end.
