%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2015-2026 Marc Worrell
%%
%% @doc Model for user group memberships.
%% @end

%% Copyright 2015-2026 Marc Worrell
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

-module(m_acl_user_group).
-moduledoc("
Model for ACL user-group helper values in mod_acl_user_groups, including collaboration-group settings and category usage checks.

Available Model API Paths
-------------------------

| Method | Path pattern | Description |
| --- | --- | --- |
| `get` | `/has_collaboration_groups/...` | Return whether collaboration groups are configured and available in the current ACL setup (`acl_user_groups_checks:has_collab_groups/1`). |
| `get` | `/is_used/+cat/...` | Return whether ACL user group `+cat` (or any of its child groups) is referenced by a `hasusergroup` edge. |
| `get` | `/collab_group_update/...` | Return module config `mod_acl_user_groups.collab_group_update` (collaboration-group update policy). |
| `get` | `/collab_group_link/...` | Return module config `mod_acl_user_groups.collab_group_link` (collaboration-group link behavior). |

`/+name` marks a variable path segment. A trailing `/...` means extra path segments are accepted for further lookups.
").
-author("Marc Worrell <marc@worrell.nl").

-behaviour(zotonic_model).

%% interface functions
-export([
    m_get/3,

    is_used/2,
    user_groups/1
]).


%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ <<"has_collaboration_groups">> | Rest ], _Msg, Context) ->
    {ok, {acl_user_groups_checks:has_collab_groups(Context), Rest}};
m_get([ <<"is_used">>, Cat | Rest ], _Msg, Context) ->
    {ok, {is_used(Cat, Context), Rest}};
m_get([ <<"collab_group_update">> | Rest ], _Msg, Context) ->
    {ok, {m_config:get_value(mod_acl_user_groups, collab_group_update, Context), Rest}};
m_get([ <<"collab_group_link">> | Rest ], _Msg, Context) ->
    {ok, {m_config:get_value(mod_acl_user_groups, collab_group_link, Context), Rest}};
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.

%% @doc Check if a user group is actually in use.
is_used(UserGroup, Context) ->
    case m_predicate:name_to_id(hasusergroup, Context) of
        {ok, PredId} ->
            UGId = m_rsc:rid(UserGroup, Context),
            Ids = [ UGId | m_hierarchy:children('acl_user_group', UGId, Context) ],
            lists:any(fun(Id) ->
                        z_db:q1("select id
                                 from edge
                                 where object_id = $1
                                   and predicate_id = $2
                                ",
                                [Id, PredId],
                                Context) =/= undefined
                     end,
                     Ids);
        {error, _} ->
            false
    end.

%% @doc Return the user groups for the current user
user_groups(Context) ->
    acl_user_groups_checks:user_groups(Context).
