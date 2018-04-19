%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013 Marc Worrell
%% @doc Model for configurable host/path redirects

%% Copyright 2013 Marc Worrell
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

-module(m_custom_redirect).
-author("Marc Worrell <marc@worrell.nl>").

-behaviour(zotonic_model).

-export([
    m_get/2,

    get/2,
    get/3,
    insert/2,
    update/3,
    delete/2,
    list/1,
    list_ids/1,
    list_dispatch_host/3,
    get_dispatch/2,

    manage_schema/2
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").


%% @doc Fetch the value for the key from a model source
-spec m_get( list(), z:context() ) -> {term(), list()}.
m_get([ list | Rest ], Context) ->
    case z_acl:is_admin(Context) of
        true -> {list(Context), Rest};
        false -> {[], Rest}
    end;
m_get([ Id | Rest ], Context) ->
    case z_acl:is_admin(Context) of
        true -> {get(Id, Context), Rest};
        false -> {[], Rest}
    end;
m_get(Vs, _Context) ->
    lager:error("Unknown ~p lookup: ~p", [?MODULE, Vs]),
    {undefined, []}.


get(Id, Context) ->
    z_db:assoc_row("select * from custom_redirect where id=$1", [Id], Context).

get(Host, Path, Context) ->
    z_db:assoc_row("select * from custom_redirect where host=lower($1) and path=lower($2)", [Host, remove_slash(Path)], Context).

insert(Props, Context) ->
    z_db:insert(custom_redirect, normalize_props(Props), Context).

update(Id, Props, Context) ->
    z_db:update(custom_redirect, Id, normalize_props(Props), Context).

delete(Id, Context) ->
    z_db:delete(custom_redirect, Id, Context).

list(Context) ->
    z_db:assoc("select *
                from custom_redirect
                order by host, path", Context).

list_ids(Context) ->
    Ids = z_db:q("select id from custom_redirect", Context),
    [ Id || {Id} <- Ids ].

list_dispatch_host(Host, Path, Context) ->
    z_db:q("select path, redirect, is_permanent
            from custom_redirect
            where host = lower($1)
              and (path = $2 or path = '')",
           [Host, remove_slash(Path)],
           Context).

get_dispatch(Path, Context) ->
    z_db:q_row("select redirect, is_permanent
                from custom_redirect
                where host = ''
                  and path = $1",
               [remove_slash(Path)],
               Context).

normalize_props(Props) ->
    [
        {host, z_string:to_lower(proplists:get_value(host, Props, ""))},
        {path, remove_slash(proplists:get_value(path, Props, ""))},
        {redirect, add_slash(proplists:get_value(redirect, Props, ""))},
        {is_permanent, z_convert:to_bool(proplists:get_value(is_permanent, Props, false))}
    ].

add_slash([]) -> [$/];
add_slash(<<>>) -> <<$/>>;
add_slash([$/|_] = Path) -> Path;
add_slash(<<$/,_/binary>> = Path) -> Path;
add_slash(Path) ->
    case string:chr(z_convert:to_list(Path), $:) of
        0 -> [$/|Path];
        _ -> Path
    end.

remove_slash(<<$/,Path/binary>>) -> Path;
remove_slash([$/|Path]) -> Path;
remove_slash(Path) -> Path.

manage_schema(install, Context) ->
    case z_db:table_exists(custom_redirect, Context) of
        false -> install(Context);
        true -> ok
    end.

install(Context) ->
    z_db:create_table(custom_redirect, [
                        #column_def{
                            name=id,
                            type="serial",
                            is_nullable=false,
                            primary_key=true
                        },
                        #column_def{
                            name=host,
                            type="character varying",
                            length=80,
                            is_nullable=false
                        },
                        #column_def{
                            name=path,
                            type="character varying",
                            length=80,
                            is_nullable=false
                        },
                        #column_def{
                            name=redirect,
                            type="character varying",
                            length=255,
                            is_nullable=false
                        },
                        #column_def{
                            name=is_permanent,
                            type="boolean",
                            is_nullable=false,
                            default="false"
                        }
                    ], Context),
    z_db:q("alter table custom_redirect add constraint custom_redirect_host_path unique(host, path)", Context),
    ok.
