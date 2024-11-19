%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013-2024 Marc Worrell
%% @doc Model for configurable host/path redirects
%% @end

%% Copyright 2013-2024 Marc Worrell
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
    m_get/3,

    is_allowed/1,
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
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ <<"list">> | Rest ], _Msg, Context) ->
    case is_allowed(Context) of
        true -> {ok, {list(Context), Rest}};
        false -> {error, eacces}
    end;
m_get([ Id | Rest ], _Msg, Context) ->
    case is_allowed(Context) of
        true -> {ok, {get(z_convert:to_integer(Id), Context), Rest}};
        false -> {error, eacces}
    end;
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.


%% @doc Check if the user is allowed to make changes to the custom redirects.
-spec is_allowed(Context) -> boolean() when
    Context :: z:context().
is_allowed(Context) ->
    z_acl:is_allowed(use, mod_custom_redirect, Context).

-spec get(Id, Context) -> Redirect | undefined when
    Id :: pos_integer(),
    Context :: z:context(),
    Redirect :: proplists:proplist().
get(Id, Context) ->
    z_db:assoc_row("select * from custom_redirect where id=$1", [Id], Context).

-spec get(Host, Path, Context) -> Redirect | undefined when
    Host :: binary() | string(),
    Path :: iodata(),
    Context :: z:context(),
    Redirect :: proplists:proplist().
get(Host, Path, Context) ->
    case z_string:is_string(Path) of
        true ->
            z_db:assoc_row("
                select *
                from custom_redirect
                where host = lower($1)
                  and path = lower($2)",
                [Host, remove_slash(Path)],
                Context);
        false ->
            undefined
    end.

insert(Props, Context) ->
    z_db:insert(custom_redirect, normalize_props(Props), Context).

update(Id, Props, Context) ->
    z_db:update(custom_redirect, Id, normalize_props(Props), Context).

delete(Id, Context) ->
    z_db:delete(custom_redirect, Id, Context).

-spec list(Context) -> Redirects when
    Context :: z:context(),
    Redirects :: [ proplists:proplist() ].
list(Context) ->
    z_db:assoc("select *
                from custom_redirect
                order by host, path", Context).

-spec list_ids(Context) -> Ids when
    Context :: z:context(),
    Ids :: [ pos_integer() ].
list_ids(Context) ->
    Ids = z_db:q("select id from custom_redirect", Context),
    [ Id || {Id} <- Ids ].

-spec list_dispatch_host(Host, Path, Context) -> Redirects when
    Host :: binary() | string(),
    Path :: iodata(),
    Context :: z:context(),
    Redirects :: [ {Path, Redirect, IsPermanent} ],
    Path :: binary(),
    Redirect :: binary(),
    IsPermanent :: boolean().
list_dispatch_host(Host, Path, Context) ->
    case z_string:is_string(Path) of
        true ->
            z_db:q("select path, redirect, is_permanent
                    from custom_redirect
                    where host = lower($1)
                      and (path = lower($2) or path = '')",
                   [Host, remove_slash(Path)],
                   Context);
        false ->
            []
    end.

-spec get_dispatch(Path, Context) -> {Redirect, IsPermanent} | undefined when
    Path :: binary(),
    Context :: z:context(),
    Redirect :: binary(),
    IsPermanent :: boolean().
get_dispatch(Path, Context) ->
    case z_string:is_string(Path) of
        true ->
            z_db:q_row("select redirect, is_permanent
                        from custom_redirect
                        where host = ''
                          and path = $1",
                       [remove_slash(Path)],
                       Context);
        false ->
            undefined
    end.

normalize_props(Props) ->
    [
        {host, z_string:to_lower(proplists:get_value(host, Props, <<>>))},
        {path, z_string:to_lower(remove_slash(proplists:get_value(path, Props, <<>>)))},
        {redirect, add_slash(proplists:get_value(redirect, Props, <<>>))},
        {is_permanent, z_convert:to_bool(proplists:get_value(is_permanent, Props, false))}
    ].

add_slash(Path) when is_list(Path) ->
    add_slash(unicode:characters_to_binary(Path, utf8));
add_slash(<<>>) -> <<$/>>;
add_slash(<<$/,_/binary>> = Path) -> Path;
add_slash(Path) ->
    case binary:match(Path, <<":">>) of
        nomatch -> <<$/, Path/binary>>;
        _ -> Path
    end.

remove_slash(Path) when is_list(Path) ->
    remove_slash(unicode:characters_to_binary(Path, utf8));
remove_slash(<<$/,Path/binary>>) -> Path;
remove_slash(Path) -> Path.

manage_schema(install, Context) ->
    case z_db:table_exists(custom_redirect, Context) of
        false -> install(Context);
        true -> ok
    end;
manage_schema({upgrade, 2}, Context) ->
    [] = z_db:q("
        alter table custom_redirect
        alter column host type character varying(250),
        alter column path type character varying(1000),
        alter column redirect type character varying(1000)",
        Context),
    ok.

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
                            length=250,
                            is_nullable=false
                        },
                        #column_def{
                            name=path,
                            type="character varying",
                            length=1000,
                            is_nullable=false
                        },
                        #column_def{
                            name=redirect,
                            type="character varying",
                            length=1000,
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
