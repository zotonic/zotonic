%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2015 Marc Worrell
%%
%% @doc Model for user group memberships.

%% Copyright 2015 Marc Worrell
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
-author("Marc Worrell <marc@worrell.nl").

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,

    is_used/2
]).


-include_lib("zotonic_core/include/zotonic.hrl").

m_find_value(has_collaboration_groups, #m{value=undefined}, Context) ->
    acl_user_groups_checks:has_collab_groups(Context);
m_find_value(is_used, #m{value=undefined} = M, _Context) ->
    M#m{value=is_used};
m_find_value(Cat, #m{value=is_used}, Context) ->
    is_used(Cat, Context);
m_find_value(_Key, _Value, _Context) ->
    undefined.

m_to_list(#m{value=undefined}, _Context) ->
    [].

m_value(#m{value=undefined}, _Context) ->
    undefined.


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
