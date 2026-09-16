%% @copyright 2015-2023 Marc Worrell
%% TODO: update docs:
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
    digest/2,
    rsc_digest_table/1,
    collab_digest_table/1
]).

%% @doc "Digests" ACL rsc and collab rules into a more usable form (aka expanded)
%% and stores them in dedicated database tables, for easier 'JOIN'/'EXISTS's.
digest(State, Context) ->
    ExpandedRscRules = acl_user_groups_rules:expand_rsc(State, Context),
    ExpandedCollabRules = acl_user_groups_rules:expand_collab(State, Context),
    z_db:transaction(
        fun (TrCtx) ->
            ensure_clear_tables(State, TrCtx),
            lists:foreach(
                fun (ExpandedRule) -> digest_rule(ExpandedRule, State, TrCtx) end,
                ExpandedRscRules ++ ExpandedCollabRules
            )
        end,
        Context
    ),
    ok.

% TODO: check (dis)allow rules order:
digest_rule({collab, {CatId, Visibility, Action, OnlyOwner, IsAllow}, collab}, State, Context) ->
    z_db:q1(
        "INSERT INTO " ++ collab_digest_table(State) ++ "
            (cat_id, visibility, action, only_owner, is_allow)
        VALUES ($1, $2, $3, $4, $5)
        ON CONFLICT (cat_id, visibility, action, only_owner)
        DO UPDATE SET is_allow = $5
        RETURNING id",
        [CatId, Visibility, Action, OnlyOwner, IsAllow],
        Context
    );
digest_rule({CgId, {CatId, Visibility, Action, OnlyOwner, IsAllow}, UgId}, State, Context) ->
    z_db:q1(
        "INSERT INTO " ++ rsc_digest_table(State) ++ "
            (cg_id, cat_id, ug_id, visibility, action, only_owner, is_allow)
        VALUES ($1, $2, $3, $4, $5, $6, $7)
        ON CONFLICT (cg_id, cat_id, ug_id, visibility, action, only_owner)
        DO UPDATE SET is_allow = $7
        RETURNING id",
        [CgId, CatId, UgId, Visibility, Action, OnlyOwner, IsAllow],
        Context
    ).


ensure_clear_tables(State, Context) ->
    RscTableName = rsc_digest_table(State),
    CollabTableName = collab_digest_table(State),
    [] = z_db:q("
        CREATE TABLE IF NOT EXISTS " ++ RscTableName ++ " (
            id bigserial NOT NULL,
            cg_id int NOT NULL,
            cat_id int NOT NULL,
            ug_id int NOT NULL,
            visibility int,
            action character varying(32) NOT NULL,
            only_owner boolean NOT NULL DEFAULT false,
            is_allow boolean NOT NULL,

            CONSTRAINT " ++ RscTableName ++ "_pkey
                PRIMARY KEY (id),
            CONSTRAINT fk_" ++ RscTableName ++ "_cg_id
                FOREIGN KEY (cg_id) REFERENCES rsc (id)
                ON UPDATE CASCADE ON DELETE CASCADE,
            CONSTRAINT fk_" ++ RscTableName ++ "_cat_id
                FOREIGN KEY (cat_id) REFERENCES rsc (id)
                ON UPDATE CASCADE ON DELETE CASCADE,
            CONSTRAINT fk_" ++ RscTableName ++ "_ug_id
                FOREIGN KEY (ug_id) REFERENCES rsc (id)
                ON UPDATE CASCADE ON DELETE CASCADE,
            CONSTRAINT " ++ RscTableName ++ "_ukey
                UNIQUE (cg_id, cat_id, ug_id, visibility, action, only_owner)
        )",
        Context
    ),
    z_db:q("DELETE FROM " ++ RscTableName, Context),
    ensure_indexes(RscTableName, Context),
    [] = z_db:q("
        CREATE TABLE IF NOT EXISTS " ++ CollabTableName ++ " (
            id bigserial NOT NULL,
            cat_id int NOT NULL,
            visibility int,
            action character varying(32) NOT NULL,
            only_owner boolean NOT NULL DEFAULT false,
            is_allow boolean NOT NULL,

            CONSTRAINT " ++ CollabTableName ++ "_pkey
                PRIMARY KEY (id),
            CONSTRAINT fk_" ++ CollabTableName ++ "_cat_id
                FOREIGN KEY (cat_id) REFERENCES rsc (id)
                ON UPDATE CASCADE ON DELETE CASCADE,
            CONSTRAINT " ++ CollabTableName ++ "_ukey
                UNIQUE (cat_id, visibility, action, only_owner)
        )",
        Context
    ),
    z_db:q("DELETE FROM " ++ CollabTableName, Context),
    ensure_indexes(CollabTableName, Context),
       ok.

ensure_indexes(TableName, Context) ->
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


rsc_digest_table(State) ->
    "acl_rule_digest_rsc_" ++ z_convert:to_list(State).

collab_digest_table(State) ->
    "acl_rule_digest_collab_" ++ z_convert:to_list(State).
