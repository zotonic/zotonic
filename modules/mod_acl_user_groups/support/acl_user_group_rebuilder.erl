%% @copyright 2015 Marc Worrell
%% @doc Rebuilder process. Expands all rules and updated the in-memory ets tables
%%
%% This function requests an expansion of all ACL rules. These are then processed and
%% stored in a new ets table. After all processing is done the new table is transfered
%% to the ACL module gen_server process.
%%
%% There are two different ets tables: one with the <i>edit</i> version of the ACL rules
%% and one with the <i>publish</i> version. A key in the user session defines which table
%% is used.
%%
%% The entries in the ets table are:
%%
%% ```
%% {{content_group_id, {category_id, action, only_if_owner}, user_group_id}, true}
%% {{content_group_id, action, user_group_id}, true}
%% {{module, module_action, user_group_id}, true}
%% {{action, user_group_id}, [content_group_id]}
%% {user_group_id, [user_group_id]}
%% '''
%%
%% The action is one of the atoms: view, insert, delete, update, and link.
%%
%% @end

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

-module(acl_user_group_rebuilder).

-export([
	rebuild/3
	]).

%% @doc Fetch a the new rules and store them in a fresh ets table. 
%%      The completed table is transfered to the acl module gen_server.
rebuild(ModulePid, State, Context) ->
	{ContentGroupIds, UserGroupIds, UserGroupPaths, Rules} = acl_user_groups_rules:expand(State, Context),
	Name = list_to_atom("rules$"++atom_to_list(z_context:site(Context))++"$"++atom_to_list(State)),
	Table = ets:new(Name, [
					set,
					protected,
					{read_concurrency, true},
					{heir, ModulePid, State}
				]),
	lists:foreach(
		fun(K0) ->
			{IsAllow, K} = extract_is_allow(K0), 
			case K of
				{CId, {CatId, insert, _IfOwner}, GId} ->
					ets:insert(Table, {{CId, {CatId, insert, false}, GId}, IsAllow}),
					ets:insert(Table, {{CId, {CatId, insert, true}, GId}, IsAllow}),
					ets:insert(Table, {{CatId, insert, GId}, IsAllow}),
					ets:insert(Table, {{CId, insert, GId}, IsAllow});
				{CId, {_CatId, Action, _IfOwner}, GId} ->
					ets:insert(Table, {K, IsAllow}),
					ets:insert(Table, {{CId, Action, GId}, IsAllow});
				_ ->
					ets:insert(Table, {K, IsAllow})
			end
		end,
		Rules),
	lists:foreach(
		fun({Id,Path}) ->
			ets:insert(Table, {Id, Path})
		end,
		UserGroupPaths),
	can_action(Table, view, UserGroupIds, ContentGroupIds),
	can_action(Table, insert, UserGroupIds, ContentGroupIds).


extract_is_allow({CId, {CatId, Action, IfOwner, IsAllow}, GId}) ->
	K1 = {CId, {CatId, Action, IfOwner}, GId},
	{IsAllow, K1};
extract_is_allow({Module, Action, GId, IsAllow}) ->
	K1 = {Module, Action, GId},
	{IsAllow, K1}.

%% @doc Add lookups to see per user-group on which content-groups an action can be done.
can_action(_Table, _Action, [], _CIds) ->
	ok;
can_action(Table, Action, [UId|UIds], CIds) ->
	CIds1 = lists:filter(fun(CId) ->
					case ets:lookup(Table, {CId, Action, UId}) of
						[{_,true}] -> true;
						_ -> false
					end
				  end,
				  CIds),
	ets:insert(Table, {{Action, UId}, CIds1}),
	can_action(Table, Action, UIds, CIds).

