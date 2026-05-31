%% @copyright 2015-2023 Marc Worrell
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
%% {{content_group_id, {category_id, visibility, action, only_if_owner}, user_group_id}, true}
%% {{'collab', {category_id, visibility, action, only_if_owner}, 'collab'}, true}
%% {{content_group_id, action, user_group_id}, true}
%% {{module, module_action, user_group_id}, true}
%% {{action, user_group_id}, [content_group_id]}
%% {user_group_id, [user_group_id]}
%% '''
%%
%% The action is one of the atoms: view, insert, delete, update, and link.
%%
%% @end

%% Copyright 2015-2023 Marc Worrell
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
	rebuild/3,

	digest/2,
	rsc_digest_table/1,
	collab_digest_table/1
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
				{CId, {CatId, Visibility, insert, _IfOwner}, GId} when IsAllow =:= true ->
					% Also store generic allow rules, to allow for rough filtering
					% on content group for allowed actions.
					ets:insert(Table, {{CId, {CatId, Visibility, insert, false}, GId}, true}),
					ets:insert(Table, {{CId, {CatId, Visibility, insert, true}, GId}, true}),
					ets:insert(Table, {{CatId, insert, GId}, true}),
					ets:insert(Table, {{CId, insert, GId}, true});
				{CId, {CatId, Visibility, insert, _IfOwner}, GId} when IsAllow =:= false ->
					% Only store specific deny rules
					ets:insert(Table, {{CId, {CatId, Visibility, insert, false}, GId}, false}),
					ets:insert(Table, {{CId, {CatId, Visibility, insert, true}, GId}, false});
				{CId, {CatId, Visibility, Action, IfOwner}, GId} when IsAllow =:= true ->
					% Also store generic allow rules, to allow for rough filtering
					% on content group for allowed actions.
					ets:insert(Table, {{CId, {CatId, Visibility, Action, IfOwner}, GId}, true}),
					ets:insert(Table, {{CId, Action, GId}, true});
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


extract_is_allow({CId, {CatId, Visibility, Action, IfOwner, IsAllow}, GId}) ->
	K1 = {CId, {CatId, Visibility, Action, IfOwner}, GId},
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

%% @doc "Digests" ACL rules into a more usable form (aka expanded) and stores them
%% in dedicated database tables, for easier 'JOIN's/'EXISTS' operations.
digest(State, Context) ->
	io:format("debug digest: ~p~n", [State]),
	digest_rsc_rules(State, Context),
	digest_collab_rules(State, Context),
	ok.

% TODO: reduce duplication:

digest_rsc_rules(State, Context) ->
	ensure_rsc_digest_table(State, Context),
	ExpandedRscRules = acl_user_groups_rules:expand_rsc(State, Context),
	z_db:transaction(
		fun (TrCtx) ->
			clear_rsc_digest_table(State, TrCtx),
			lists:foreach(
				fun (ExpandedRscRule) -> digest_rsc_rule(ExpandedRscRule, State, TrCtx) end,
				ExpandedRscRules
			)
		end,
		Context
	).

digest_collab_rules(State, Context) ->
	ensure_collab_digest_table(State, Context),
	ExpandedCollabRules = acl_user_groups_rules:expand_collab(State, Context),
	z_db:transaction(
		fun (TrCtx) ->
			clear_collab_digest_table(State, TrCtx),
			lists:foreach(
				fun (ExpandedCollabRule) -> digest_collab_rule(ExpandedCollabRule, State, TrCtx) end,
				ExpandedCollabRules
			)
		end,
		Context
	).

digest_rsc_rule({CgId, {CatId, Visibility, Action, OnlyOwner, IsAllow}, UgId}, State, Context) ->
	TableName = rsc_digest_table(State),
	% TODO: check (dis)allow rules order:
    z_db:q1(
        "INSERT INTO " ++ TableName ++ " (cg_id, cat_id, ug_id, visibility, action, only_owner, is_allow)
        VALUES ($1, $2, $3, $4, $5, $6, $7)
        ON CONFLICT (cg_id, cat_id, ug_id, visibility, action, only_owner)
        DO UPDATE SET is_allow = $7
        RETURNING id",
        [CgId, CatId, UgId, Visibility, Action, OnlyOwner, IsAllow],
        Context
    ).

digest_collab_rule({collab, {CatId, Visibility, Action, OnlyOwner, IsAllow}, collab}, State, Context) ->
	TableName = collab_digest_table(State),
	% TODO: check (dis)allow rules order:
    z_db:q1(
        "INSERT INTO " ++ TableName ++ " (cat_id, visibility, action, only_owner, is_allow)
        VALUES ($1, $2, $3, $4, $5)
        ON CONFLICT (cat_id, visibility, action, only_owner)
        DO UPDATE SET is_allow = $5
        RETURNING id",
        [CatId, Visibility, Action, OnlyOwner, IsAllow],
        Context
    ).


ensure_rsc_digest_table(State, Context) ->
	TableName = rsc_digest_table(State),
	[] = z_db:q("
        CREATE TABLE IF NOT EXISTS " ++ TableName ++ " (
            id bigserial NOT NULL,
            cg_id int NOT NULL,
            cat_id int NOT NULL,
            ug_id int NOT NULL,
            visibility int,
            action character varying(32) NOT NULL,
            only_owner boolean NOT NULL DEFAULT false,
            is_allow boolean NOT NULL,

            CONSTRAINT " ++ TableName ++ "_pkey
                PRIMARY KEY (id),
            CONSTRAINT fk_" ++ TableName ++ "_cg_id
                FOREIGN KEY (cg_id) REFERENCES rsc (id)
                ON UPDATE CASCADE ON DELETE CASCADE,
            CONSTRAINT fk_" ++ TableName ++ "_cat_id
                FOREIGN KEY (cat_id) REFERENCES rsc (id)
                ON UPDATE CASCADE ON DELETE CASCADE,
            CONSTRAINT fk_" ++ TableName ++ "_ug_id
                FOREIGN KEY (ug_id) REFERENCES rsc (id)
                ON UPDATE CASCADE ON DELETE CASCADE,
            CONSTRAINT " ++ TableName ++ "_ukey
                UNIQUE (cg_id, cat_id, ug_id, visibility, action, only_owner)
        )",
        Context
    ),
	[] = z_db:q("
        CREATE INDEX IF NOT EXISTS " ++ TableName ++ "_visibility_idx
        ON " ++ TableName ++ " (visibility)
        ",
        Context
    ),
	[] = z_db:q("
        CREATE INDEX IF NOT EXISTS " ++ TableName ++ "_action_idx
        ON " ++ TableName ++ " (action)
        ",
        Context
    ),
    [] = z_db:q("
        CREATE INDEX IF NOT EXISTS " ++ TableName ++ "_only_owner_idx
        ON " ++ TableName ++ " (only_owner)
        ",
        Context
    ),
    [] = z_db:q("
        CREATE INDEX IF NOT EXISTS " ++ TableName ++ "_is_allow_idx
        ON " ++ TableName ++ " (is_allow)
        ",
        Context
    ),
   	ok.

ensure_collab_digest_table(State, Context) ->
	TableName = collab_digest_table(State),
	[] = z_db:q("
        CREATE TABLE IF NOT EXISTS " ++ TableName ++ " (
            id bigserial NOT NULL,
            cat_id int NOT NULL,
            visibility int,
            action character varying(32) NOT NULL,
            only_owner boolean NOT NULL DEFAULT false,
            is_allow boolean NOT NULL,

            CONSTRAINT " ++ TableName ++ "_pkey
                PRIMARY KEY (id),
            CONSTRAINT fk_" ++ TableName ++ "_cat_id
                FOREIGN KEY (cat_id) REFERENCES rsc (id)
                ON UPDATE CASCADE ON DELETE CASCADE,
            CONSTRAINT " ++ TableName ++ "_ukey
                UNIQUE (cat_id, visibility, action, only_owner)
        )",
        Context
    ),
	[] = z_db:q("
        CREATE INDEX IF NOT EXISTS " ++ TableName ++ "_visibility_idx
        ON " ++ TableName ++ " (visibility)
        ",
        Context
    ),
	[] = z_db:q("
        CREATE INDEX IF NOT EXISTS " ++ TableName ++ "_action_idx
        ON " ++ TableName ++ " (action)
        ",
        Context
    ),
    [] = z_db:q("
        CREATE INDEX IF NOT EXISTS " ++ TableName ++ "_only_owner_idx
        ON " ++ TableName ++ " (only_owner)
        ",
        Context
    ),
    [] = z_db:q("
        CREATE INDEX IF NOT EXISTS " ++ TableName ++ "_is_allow_idx
        ON " ++ TableName ++ " (is_allow)
        ",
        Context
    ),
   	ok.

clear_rsc_digest_table(State, Context) ->
	z_db:q("DELETE FROM " ++ rsc_digest_table(State), Context).

clear_collab_digest_table(State, Context) ->
	z_db:q("DELETE FROM " ++ collab_digest_table(State), Context).

rsc_digest_table(State) ->
	"acl_rule_digest_rsc_" ++ z_convert:to_list(State).

collab_digest_table(State) ->
	"acl_rule_digest_collab_" ++ z_convert:to_list(State).
