%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-10
%%
%% @doc DB model group interface

%% Copyright 2009 Marc Worrell
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

-module(m_group).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,

    name_to_id/2,
    name_to_id_check/2,
    
    get/2,
    insert/2,
    update_noflush/3,
    add_member/3,
    add_observer/3,
    add_leader/3,
    get_members/2,
    get_leaders/2,
    get_observers/2,
    delete_member/2,
    delete_member/3,
    groups_member/1,
    groups_member/2,
    groups_observer/1,
    groups_observer/2,
    groups_visible/1,
    groups_visible/2,
    groups_leader/1,
    groups_leader/2,
    roles/1,
    roles/2
]).

-include_lib("zotonic.hrl").


%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(members, #m{value=undefined} = M, _Context) ->
    M#m{value=members};
m_find_value(leaders, #m{value=undefined} = M, _Context) ->
    M#m{value=leaders};
m_find_value(observers, #m{value=undefined} = M, _Context) ->
    M#m{value=observers};
m_find_value(roles, #m{value=undefined} = M, _Context) ->
    M#m{value=roles};
m_find_value(Id, #m{value=members}, Context) when is_integer(Id) ->
    get_members(Id, Context);
m_find_value(Id, #m{value=leaders}, Context) when is_integer(Id) ->
    get_leaders(Id, Context);
m_find_value(Id, #m{value=observers}, Context) when is_integer(Id) ->
    get_observers(Id, Context);
m_find_value(Id, #m{value=roles}, Context) when is_integer(Id) ->
    roles(Id, Context);
m_find_value(Id, #m{value=undefined}, Context) when is_integer(Id) ->
    get(Id, Context).


%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context)
m_to_list(_, _Context) ->
    [].

%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{value=_}, _Context) ->
    undefined.


%% @doc Get the group
%% @spec get(Id, Context) -> PropList
get(Id, Context) ->
    F = fun() ->
        z_db:assoc_row("select * from \"group\" where id = $1", [Id], Context)
    end,
    z_depcache:memo(F, {group, Id}, ?WEEK, [Id], Context).


name_to_id(Name, Context) ->
    m_rsc:name_to_id_cat(Name, group, Context).

name_to_id_check(Name, Context) ->
    m_rsc:name_to_id_cat_check(Name, group, Context).
    

%% @doc Insert a new group.
%% @spec insert(Title, Context) -> {ok, int()} | {error, Reason}
insert(Title, Context) ->
    case z_acl:has_role(admin, Context) of
        true ->
            Props = [
                {title, Title},
                {category, group},
                {group, admins},
                {is_published, true},
                {visible_for, 1}
            ],
            m_rsc:insert(Props, Context);
        false ->
            {error, eacces}
    end.


update_noflush(Id, Props, Context) ->
    case z_acl:has_role(admin, Context) of
        true ->
            GroupProps = [
                {is_admin, proplists:get_value(group_is_admin, Props, false)},
                {is_supervisor, proplists:get_value(group_is_supervisor, Props, false)},
                {is_community_publisher, proplists:get_value(group_is_community_publisher, Props, false)},
                {is_public_publisher, proplists:get_value(group_is_public_publisher, Props, false)}
            ],
            case z_db:update(group, Id, GroupProps, Context) of
                {ok, 1} ->
                    {ok, Id};
                {ok, 0} ->
                    z_db:insert(group, [ {id, Id} | GroupProps ], Context)
            end;
        false ->
            {error, eacces}
    end.
    

add_member(Id, RscId, Context) ->
    case z_acl:has_group_role(leader, Id, Context) of
        true ->
            F = fun(Ctx) ->
                case z_db:q1("select id from rsc_group where group_id = $1 and rsc_id = $2", [Id, RscId], Ctx) of
                    undefined ->
                        z_db:insert(rsc_group, [{group_id, Id},{rsc_id, RscId}], Ctx);
                    RscGroupId ->
                        z_db:q("update rsc_group set is_observer = false, is_leader = false where id = $1", [RscGroupId], Ctx),
                        {ok, RscGroupId}
                end
            end,
            Result = z_db:transaction(F, Context),
            z_depcache:flush({groups, RscId}, Context),
            Result;
        false ->
            {error, eacces}
    end.

add_observer(Id, RscId, Context) ->
    case z_acl:has_group_role(leader, Id, Context) of
        true ->
            F = fun(Ctx) ->
                case z_db:q1("select id from rsc_group where group_id = $1 and rsc_id = $2", [Id, RscId], Ctx) of
                    undefined ->
                        z_db:insert(rsc_group, [{group_id, Id},{rsc_id, RscId},{is_observer, true}], Ctx);
                    RscGroupId ->
                        z_db:q("update rsc_group set is_observer = true, is_leader = false where id = $1", [RscGroupId], Ctx),
                        {ok, RscGroupId}
                end
            end,
            Result = z_db:transaction(F, Context),
            z_depcache:flush({groups, RscId}, Context),
            Result;
        false ->
            {error, eacces}
    end.

add_leader(Id, RscId, Context) ->
    case z_acl:has_group_role(leader, Id, Context) of
        true ->
            F = fun(Ctx) ->
                case z_db:q1("select id from rsc_group where group_id = $1 and rsc_id = $2", [Id, RscId], Ctx) of
                    undefined ->
                        z_db:insert(rsc_group, [{group_id, Id},{rsc_id, RscId},{is_leader, true}], Ctx);
                    RscGroupId ->
                        z_db:q("update rsc_group set is_observer = false, is_leader = true where id = $1", [RscGroupId], Ctx),
                        {ok, RscGroupId}
                end
            end,
            Result = z_db:transaction(F, Context),
            z_depcache:flush({groups, RscId}, Context),
            Result;
        false ->
            {error, eacces}
    end.

get_members(Id, Context) ->
    [ RscId || {RscId} <- z_db:q("select rsc_id from rsc_group where group_id = $1 and is_observer = false and is_leader = false", [Id], Context) ].

get_leaders(Id, Context) ->
    [ RscId || {RscId} <- z_db:q("select rsc_id from rsc_group where group_id = $1 and is_leader = true", [Id], Context) ].

get_observers(Id, Context) ->
    [ RscId || {RscId} <- z_db:q("select rsc_id from rsc_group where group_id = $1 and is_observer = true", [Id], Context) ].


delete_member(RscGroupId, Context) ->
    case z_db:q_row("select group_id, rsc_id from rsc_group where id = $1", [RscGroupId], Context) of
        undefined ->
            {error, enoent};
        {GroupId, RscId} ->
            case z_acl:has_group_role(leader, GroupId, Context) of
                true ->
                    z_db:delete(rsc_group, RscGroupId, Context),
                    z_depcache:flush({groups, RscId}, Context),
                    ok;
                false ->
                    {error, eacces}
            end
    end.

delete_member(Id, RscId, Context) ->
    case z_acl:has_group_role(leader, Id, Context) of
        true ->
            z_db:q("delete from rsc_group where group_id = $1 and rsc_id = $2",[Id, RscId], Context),
            z_depcache:flush({groups, RscId}, Context),
            ok;
        false ->
            {error, eacces}
    end.

    
%% @doc Return the group ids the current person is member of
%% @spec person_groups(Context) -> List
groups_member(Context) ->
    groups_member(z_acl:user(Context), Context).


%% @doc Return the group ids the person is member or leader of
%% @spec person_groups(PersonId, Context) -> List
groups_member(PersonId, Context) ->
    F = fun() ->
        Groups = z_db:q("select group_id from rsc_group where rsc_id = $1 and is_observer = false", [PersonId], Context),
        [ G || {G} <- Groups ]
    end,
    z_depcache:memo(F, {groups_member, PersonId}, ?DAY, [{groups, PersonId}], Context).


%% @doc Return the group ids the current person can only view
%% @spec person_groups(Context) -> List
groups_observer(Context) ->
    groups_observer(z_acl:user(Context), Context).


%% @doc Return the group ids the person can only view
%% @spec person_groups(PersonId, Context) -> List
groups_observer(PersonId, Context) ->
    F = fun() ->
        Groups = z_db:q("select group_id from rsc_group where rsc_id = $1 and is_observer = true", [PersonId], Context),
        [ G || {G} <- Groups ]
    end,
    z_depcache:memo(F, {groups_observer, PersonId}, ?DAY, [{groups, PersonId}], Context).


%% @doc Return the group ids the current person can view
%% @spec person_groups(Context) -> List
groups_visible(Context) ->
    groups_visible(z_acl:user(Context), Context).

%% @doc Return the group ids the person can view
%% @spec person_groups(PersonId, Context) -> List
groups_visible(PersonId, Context) ->
    F = fun() ->
        Groups = z_db:q("select group_id from rsc_group where rsc_id = $1", [PersonId], Context),
        [ G || {G} <- Groups ]
    end,
    z_depcache:memo(F, {groups_visible, PersonId}, ?DAY, [{groups, PersonId}], Context).


%% @doc Return the group ids the current person is leader of
%% @spec person_groups(Context) -> List
groups_leader(Context) ->
    groups_leader(z_acl:user(Context), Context).

%% @doc Return the group ids the person leads
%% @spec person_groups(PersonId, Context) -> List
groups_leader(PersonId, Context) ->
    F = fun() ->
        Groups = z_db:q("select group_id from rsc_group where rsc_id = $1 and is_leader = true", [PersonId], Context),
        [ G || {G} <- Groups ]
    end,
    z_depcache:memo(F, {groups_leader, PersonId}, ?DAY, [{groups, PersonId}], Context).


%% @doc Return the roles the current person has
%% @spec roles(Context) -> [atom(), ..]
roles(Context) ->
    roles(z_acl:user(Context), Context).

%% @doc Return the roles the person has
%% @spec roles(Context) -> [atom(), ..]
roles(PersonId, Context) ->
    Roles = case z_db:q("
        select  max(g.is_admin::int), max(g.is_supervisor::int), 
                max(g.is_community_publisher::int), max(g.is_public_publisher::int)
        from \"group\" g join rsc_group rg on rg.group_id = g.id
        where rg.rsc_id = $1
          and rg.is_observer = false", [PersonId], Context) of
          [Row] -> Row;
          [] -> {0, 0, 0, 0, 0}
    end,
    P = lists:zip([admin, supervisor, community_publisher, public_publisher], tuple_to_list(Roles)),
    [ R || {R,M} <- P, is_integer(M), M > 0 ].
