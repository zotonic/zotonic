%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2022-2026 Marc Worrell
%% @doc Note about a resource, edited in the admin, not part of the resource.
%% @end

%% Copyright 2022-2026 Marc Worrell
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

-module(m_admin_note).
-moduledoc("
Add an editorial note to any resource.

The note is entered on the admin edit page. Only people with edit permission on the resource *and* access to the admin
are allowed to see and edit notes.

Available Model API Paths
-------------------------

| Method | Path pattern | Description |
| --- | --- | --- |
| `get` | `/rsc/+id/...` | Return admin note text/metadata attached to resource `+id`. |

`/+name` marks a variable path segment. A trailing `/...` means extra path segments are accepted for further lookups.
").

-export([
    m_get/3,

    get_rsc/2,
    update_rsc/3,
    delete_rsc/2,

    install/1
]).

m_get([ <<"rsc">>, Id | Rest ], _Msg, Context) ->
    case get_rsc(Id, Context) of
        {ok, Note} ->
            {ok, {Note, Rest}};
        {error, _} = Error ->
            Error
    end.


%% @doc Fetch the admin note belonging the resource. Returns a binary
%% map with rsc_id, note, modified and modifier_id keys.
-spec get_rsc(Id, Context) -> Result when
    Id :: m_rsc:resource() | undefined,
    Context :: z:context(),
    Result :: {ok, map()} | {error, Reason},
    Reason :: enoent | eacces | term().
get_rsc(Id, Context) ->
    case m_rsc:rid(Id, Context) of
        undefined ->
            {error, enoent};
        RId ->
            case is_allowed(RId, Context) of
                true ->
                    z_db:qmap_row("
                        select *
                        from admin_note_rsc
                        where rsc_id = $1
                        ",
                        [ m_rsc:rid(RId, Context) ],
                        Context);
                false ->
                    {error, eacces}
            end
    end.

%% @doc Update or insert the admin note belonging the resource.
-spec update_rsc(Id, Note, Context) -> Result when
    Id :: m_rsc:resource() | undefined,
    Note :: binary() | undefined,
    Context :: z:context(),
    Result :: ok | {error, term()}.
update_rsc(Id, Note, Context) ->
    case m_rsc:rid(Id, Context) of
        undefined ->
            {error, enoent};
        RId ->
            case is_allowed(RId, Context) of
                true ->
                    1 = z_db:q("
                        insert into admin_note_rsc
                            (rsc_id, note, modifier_id, modified)
                        values
                            ($1, $2, $3, now())
                        on conflict(rsc_id)
                        do update
                        set note = excluded.note,
                            modifier_id = excluded.modifier_id,
                            modified = now()
                        ",
                        [ RId, Note, z_acl:user(Context) ],
                        Context),
                    publish_update(Id, <<"update">>, Context),
                    ok;
                false ->
                    {error, eacces}
            end
    end.

%% @doc Delete the admin note belonging the resource.
-spec delete_rsc(Id, Context) -> Result when
    Id :: m_rsc:resource() | undefined,
    Context :: z:context(),
    Result :: ok | {error, term()}.
delete_rsc(Id, Context) ->
    case m_rsc:rid(Id, Context) of
        undefined ->
            {error, enoent};
        RId ->
            case is_allowed(RId, Context) of
                true ->
                    z_db:q(
                        "delete from admin_note_rsc where rsc_id = $1",
                        [ RId ],
                        Context),
                    publish_update(Id, <<"delete">>, Context),
                    ok;
                false ->
                    {error, eacces}
            end
    end.


publish_update(Id, Event, Context) ->
    Topic = [ <<"model">>, <<"admin_note">>, <<"event">>, <<"rsc">>, Id ],
    z_mqtt:publish(
        Topic,
        #{
            id => Id,
            event => Event
        },
        Context).


is_allowed(Id, Context) ->
    z_acl:rsc_editable(Id, Context)
    andalso z_acl:is_allowed(use, mod_admin, Context).

install(Context) ->
    case z_db:table_exists(admin_note_rsc, Context) of
        true ->
            ok;
        false ->
            [] = z_db:q("
                create table admin_note_rsc (
                    rsc_id int not null,
                    note text not null default '',
                    modifier_id int,
                    modified timestamp with time zone NOT NULL DEFAULT now(),

                    PRIMARY KEY (rsc_id),
                    CONSTRAINT fk_admin_note_rsc_rsc_id FOREIGN KEY (rsc_id)
                        REFERENCES rsc (id)
                        ON UPDATE CASCADE ON DELETE CASCADE,
                    CONSTRAINT fk_admin_note_rsc_modifier_id FOREIGN KEY (modifier_id)
                        REFERENCES rsc (id)
                        ON UPDATE CASCADE ON DELETE SET NULL
                )
                ", Context),
            [] = z_db:q("CREATE INDEX fki_admin_note_rsc_modifier_id ON admin_note_rsc (modifier_id)", Context),
            z_db:flush(Context)
    end.
