%% @author Arjan Scherpenisse <marc@worrell.nl>
%% @copyright 2015 Arjan Scherpenisse
%% Date: 2015-03-09
%%
%% @doc Access to the ACL rules

%% Copyright 2015 Arjan Scherpenisse
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

-module(m_acl_rule).
-author("Arjan Scherpenisse <marc@worrell.nl").

-behaviour(zotonic_model).

%% interface functions
-export([
    m_get/2,

    is_valid_code/2,

    manage_schema/2,
    all_rules/3,

    get/3,
    update/4,
    insert/3,
    delete/3,
    replace_managed/3,

    revert/2,
    publish/2,

    import_rules/4,
    ids_to_names/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(valid_acl_kind(T), ((T) =:= rsc orelse (T) =:= module orelse (T) =:= collab)).
-define(valid_acl_state(T), ((T) =:= edit orelse (T) =:= publish)).


%% @doc Fetch the value for the key from a model source
-spec m_get( list(), z:context() ) -> {term(), list()}.
m_get([ is_valid_code, Code | Rest ], Context) ->
    {is_valid_code(Code, Context), Rest};
m_get([ generate_code | Rest ], Context) ->
    {generate_code(Context), Rest};

m_get([ default_upload_size | Rest ], _Context) ->
    {acl_user_groups_checks:max_upload_size_default(), Rest};
m_get([ upload_size | Rest ], Context) ->
    {acl_user_groups_checks:max_upload_size(Context), Rest};

m_get([ can_insert, none, CategoryId | Rest ], Context) ->
    {acl_user_groups_checks:can_insert_category(CategoryId, Context), Rest};
m_get([ can_insert, acl_collaboration_group, CategoryId | Rest ], Context) ->
    {acl_user_groups_checks:can_insert_category_collab(CategoryId, Context), Rest};
m_get([ can_insert, ContentGroupId, CategoryId | Rest ], Context) ->
    {acl_user_groups_checks:can_insert_category(ContentGroupId, CategoryId, Context), Rest};

m_get([ can_move, ContentGroupId, RscId | Rest ], Context) ->
    {acl_user_groups_checks:can_move(ContentGroupId, RscId, Context), Rest};

m_get([ T, actions | Rest ], Context) when ?valid_acl_kind(T) ->
    {actions(T, Context), Rest};
m_get([ T, S, {all, Opts} | Rest ], Context) when ?valid_acl_kind(T), ?valid_acl_state(S) ->
    {all_rules(T, S, Opts, Context), Rest};
m_get([ T, S | Rest ], Context) when ?valid_acl_kind(T), ?valid_acl_state(S) ->
    {all_rules(T, S, [], Context), Rest};
m_get([ T, Id | Rest ], Context) when ?valid_acl_kind(T), is_integer(Id) ->
    {ok, Props} = get(T, Id, Context),
    {Props, Rest};
m_get([ T, undefined | Rest ], _Context) when ?valid_acl_kind(T) ->
    {undefined, Rest};

m_get(Vs, _Context) ->
    lager:error("Unknown ~p lookup: ~p", [?MODULE, Vs]),
    {undefined, []}.


%% @doc Generate a code for testing out the 'test' acl rules
generate_code(Context) ->
    case z_acl:is_allowed(use, mod_acl_user_groups, Context) of
        true ->
            z_utils:pickle({acl_test, calendar:universal_time()}, Context);
        false ->
            undefined
    end.

is_valid_code(Code, Context) ->
    try
        {acl_test, DT} = z_utils:depickle(Code, Context),
        calendar:universal_time() < z_datetime:next_day(DT)
    catch
        _:_ ->
            false
    end.


-type acl_rule() :: list().
-spec all_rules(rsc | module | collab, edit | publish, #context{}) -> [acl_rule()].
all_rules(Kind, State, Context) ->
    all_rules(Kind, State, [], Context).

-type acl_rules_opt() :: {group, string()}.
-spec all_rules(rsc | module | collab, edit | publish, [acl_rules_opt()], #context{}) -> [acl_rule()].
all_rules(Kind, State, Opts, Context) ->
    Query = "SELECT * FROM " ++ z_convert:to_list(table(Kind))
        ++ " WHERE " ++ state_sql_clause(State),
    All = z_db:assoc(Query, Context),
    sort_by_user_group(normalize_actions(All), z_convert:to_integer(proplists:get_value(group, Opts)), Context).


sort_by_user_group(Rs, undefined, Context) ->
    Tree = m_hierarchy:menu(acl_user_group, Context),
    Ids = lists:reverse(flatten_tree(Tree, [])),
    Zipped = lists:zip(Ids, lists:seq(1,length(Ids))),
    WithNr = lists:map(fun(R) ->
                               UGId = proplists:get_value(acl_user_group_id, R),
                               Nr = proplists:get_value(UGId, Zipped),
                               IsBlock = proplists:get_value(is_block, R),
                               {{Nr,
                                 not IsBlock,
                                 cat_key(proplists:get_value(category_id, R), Context),
                                 proplists:get_value(created,R),
                                 proplists:get_value(id, R)}, R}
                       end,
                       Rs),
    Rs1 = lists:sort(WithNr),
    [ R1 || {_,R1} <- Rs1 ];

sort_by_user_group(Rs, Group, Context) ->
    Parents = m_hierarchy:parents(acl_user_group, Group, Context),
    Tree = m_hierarchy:menu(acl_user_group, Context),
    Ids = [Group | Parents] ++ lists:reverse(flatten_tree(Tree, [])),
    Zipped = lists:zip(Ids, lists:seq(1,length(Ids))),
    WithNr = lists:map(fun(R) ->
                               UGId = proplists:get_value(acl_user_group_id, R),
                               Nr = proplists:get_value(UGId, Zipped),
                               IsBlock = proplists:get_value(is_block, R),
                               {{Nr,not IsBlock}, R}
                       end,
                       Rs),
    Rs1 = lists:sort(WithNr),
    [ R1 || {_,R1} <- Rs1 ].

cat_key(undefined, _Context) ->
    -1;
cat_key(CatId, Context) ->
    {From, _To} = m_category:get_range(CatId, Context),
    From.

flatten_tree([], Acc) ->
    Acc;
flatten_tree([{Id,Sub}|Rest], Acc) ->
    Acc1 = flatten_tree(Sub, [Id|Acc]),
    flatten_tree(Rest, Acc1).

table(rsc) -> acl_rule_rsc;
table(module) -> acl_rule_module;
table(collab) -> acl_rule_collab.

state_sql_clause(edit) -> "is_edit = true";
state_sql_clause(publish) -> "is_edit = false".

normalize_actions(Rows) ->
    [normalize_action(Row) || Row <- Rows].

normalize_action(Row) ->
    Actions = proplists:get_value(actions, Row),
    [{actions, [{z_convert:to_atom(T), true} || T <- string:tokens(z_convert:to_list(Actions), ",")]}
     | proplists:delete(actions, Row)].

actions(Kind, Context) when Kind =:= rsc; Kind =:= collab ->
    [
     {view, ?__("view (acl action)", Context)},
     {insert, ?__("insert (acl action)", Context)},
     {update, ?__("edit (acl action)", Context)},
     {delete, ?__("delete (acl action)", Context)},
     {link, ?__("link (acl action)", Context)}
    ];
actions(module, Context) ->
    [
     {use, ?__("use (acl action)", Context)}
    ].


update(Kind, Id, Props, Context) ->
    lager:debug(
        "ACL user groups update by ~p of ~p:~p with ~p",
       [z_acl:user(Context), Kind, Id, Props]
    ),
    Result = z_db:update(
               table(Kind), Id,
               [{is_edit, true},
                {modifier_id, z_acl:user(Context)},
                {modified, calendar:universal_time()}
                | map_props(Props, Context)], Context
              ),
    mod_acl_user_groups:rebuild(edit, Context),
    Result.

get(Kind, Id, Context) ->
    {ok, Row} = z_db:select(table(Kind), Id, Context),
    {ok, normalize_action(Row)}.

insert(Kind, Props, Context) ->
    lager:debug(
        "ACL user groups insert by ~p of ~p with ~p",
       [z_acl:user(Context), Kind, Props]
    ),

    Result = z_db:insert(
               table(Kind),
               [{is_edit, true},
                {modifier_id, z_acl:user(Context)},
                {modified, calendar:universal_time()},
                {creator_id, z_acl:user(Context)},
                {created, calendar:universal_time()} | map_props(Props, Context)], Context
              ),
    mod_acl_user_groups:rebuild(edit, Context),
    Result.

map_props(Props, Context) ->
    lists:map(
        fun(Prop) ->
            map_prop(Prop, Context)
        end,
        Props
    ).

map_prop({acl_user_group_id, Id}, Context) ->
    {acl_user_group_id, m_rsc:rid(Id, Context)};
map_prop({content_group_id, Id}, Context) ->
    {content_group_id, m_rsc:rid(Id, Context)};
map_prop({category_id, Id}, Context) ->
    {category_id, m_rsc:rid(Id, Context)};
map_prop({actions, Actions}, _Context) ->
    Joined = string:join(
        [z_convert:to_list(Action) || Action <- Actions],
        ","
    ),
    {actions, Joined};
map_prop(Prop, _Context) ->
    Prop.

delete(Kind, Id, Context) ->
    lager:debug(
        "ACL user groups delete by ~p of ~p:~p",
       [z_acl:user(Context), Kind, Id]
    ),
    %% Assertion, can only delete edit version of a rule
    {ok, Row} = z_db:select(table(Kind), Id, Context),
    true = proplists:get_value(is_edit, Row),
    {ok, _} = z_db:delete(table(Kind), Id, Context),
    mod_acl_user_groups:rebuild(edit, Context),
    ok.

replace_managed(Rules, Module, Context) ->
    delete_managed(Module, Context),
    lists:foreach(
        fun(Rule) ->
            manage_acl_rule(Rule, Module, Context)
        end,
        Rules),
    m_acl_rule:publish(rsc, Context),
    m_acl_rule:publish(module, Context),
    m_acl_rule:publish(collab, Context).

manage_acl_rule({Type, Props}, Module, Context) ->
    insert(Type, [{managed_by, Module} | Props], Context).

%% Remove all edit versions, add edit versions of published rules
revert(Kind, Context) ->
    lager:warning("ACL user groups revert by ~p of ~p",
                  [z_acl:user(Context), Kind]),
    T = z_convert:to_list(table(Kind)),
    Result = z_db:transaction(
               fun(Ctx) ->
                       z_db:q("DELETE FROM " ++ T ++ " WHERE is_edit = true", Ctx),
                       All = z_db:assoc("SELECT * FROM " ++ T ++ " WHERE is_edit = false", Ctx),
                       [z_db:insert(table(Kind),
                                    z_utils:prop_delete(id, z_utils:prop_replace(is_edit, true, Row)),
                                    Context) || Row <- All],
                       ok
               end,
               Context),
    mod_acl_user_groups:rebuild(edit, Context),
    Result.


%% Remove all publish versions, add published versions of unpublished rules
publish(Kind, Context) ->
    lager:debug(
        "ACL user groups publish by ~p",
        [z_acl:user(Context)]
    ),
    T = z_convert:to_list(table(Kind)),
    Result = z_db:transaction(
               fun(Ctx) ->
                       z_db:q("DELETE FROM " ++ T ++ " WHERE is_edit = false", Ctx),
                       All = z_db:assoc("SELECT * FROM " ++ T ++ " WHERE is_edit = true", Ctx),
                       [z_db:insert(table(Kind),
                                    z_utils:prop_delete(id, z_utils:prop_replace(is_edit, false, Row)),
                                    Context) || Row <- All],
                       ok
               end,
               Context),
    mod_acl_user_groups:rebuild(publish, Context),
    Result.


manage_schema(_Version, Context) ->
    ensure_acl_rule_rsc(Context),
    ensure_acl_rule_module(Context),
    ensure_acl_rule_collab(Context).

ensure_acl_rule_rsc(Context) ->
    case z_db:table_exists(acl_rule_rsc, Context) of
        false ->
            z_db:create_table(acl_rule_rsc,
                              shared_table_columns() ++
                                  [
                                   #column_def{name=acl_user_group_id, type="integer", is_nullable=false},
                                   #column_def{name=is_owner, type="boolean", is_nullable=false, default="false"},
                                   #column_def{name=category_id, type="integer", is_nullable=true},
                                   #column_def{name=content_group_id, type="integer", is_nullable=true}
                                  ],
                              Context),
            fk_setnull("acl_rule_rsc", "creator_id", Context),
            fk_setnull("acl_rule_rsc", "modifier_id", Context),
            fk_cascade("acl_rule_rsc", "acl_user_group_id", Context),
            fk_cascade("acl_rule_rsc", "content_group_id", Context),
            fk_cascade("acl_rule_rsc", "category_id", Context),
            ok;

        true ->
            ensure_column_is_block(acl_rule_rsc, Context),
            ensure_column_managed_by(acl_rule_rsc, Context),
            ensure_fix_nullable(acl_rule_rsc, Context),
            ok
    end.

ensure_acl_rule_module(Context) ->
    case z_db:table_exists(acl_rule_module, Context) of
        false ->
            z_db:create_table(acl_rule_module,
                              shared_table_columns() ++
                                  [
                                   #column_def{name=acl_user_group_id, type="integer", is_nullable=false},
                                   #column_def{name=module, type="character varying", length=300, is_nullable=false}
                                  ],
                              Context),
            fk_setnull("acl_rule_module", "creator_id", Context),
            fk_setnull("acl_rule_module", "modifier_id", Context),
            fk_cascade("acl_rule_module", "acl_user_group_id", Context),
            ok;
        true ->
            ensure_column_is_block(acl_rule_module, Context),
            ensure_column_managed_by(acl_rule_module, Context),
            ensure_fix_nullable(acl_rule_module, Context),
            ok
    end,
    ok.

ensure_acl_rule_collab(Context) ->
    case z_db:table_exists(acl_rule_collab, Context) of
        false ->
            z_db:create_table(acl_rule_collab,
                              shared_table_columns() ++
                                  [
                                   #column_def{name=is_owner, type="boolean", is_nullable=false, default="false"},
                                   #column_def{name=category_id, type="integer", is_nullable=true}
                                  ],
                              Context),
            fk_setnull("acl_rule_collab", "creator_id", Context),
            fk_setnull("acl_rule_collab", "modifier_id", Context),
            fk_cascade("acl_rule_collab", "category_id", Context),
            ok;
        true ->
            ensure_column_is_block(acl_rule_module, Context),
            ensure_column_managed_by(acl_rule_module, Context),
            ok
    end,
    ok.

ensure_column_is_block(Table, Context) ->
    Columns = z_db:column_names(Table, Context),
    case lists:member(is_block, Columns) of
        true ->
            ok;
        false ->
            [] = z_db:q("alter table "++atom_to_list(Table)++" add column is_block boolean not null default false", Context),
            z_db:flush(Context)
    end.

ensure_column_managed_by(Table, Context) ->
    Columns = z_db:column_names(Table, Context),
    case lists:member(managed_by, Columns) of
        true ->
            ok;
        false ->
            [] = z_db:q("alter table "++atom_to_list(Table)++" add column managed_by character varying(255)", Context),
            z_db:flush(Context)
    end.

% Ensure that the creator_id/modifier_id are nullable and that the fk constraint doesn't cascade on delete
ensure_fix_nullable(Table, Context) ->
    Columns = z_db:columns(Table, Context),
    [CreatorCol] = [ Col || Col <- Columns, Col#column_def.name =:= creator_id ],
    case CreatorCol#column_def.is_nullable of
        true ->
            ok;
        false ->
            TableS = z_convert:to_list(Table),
            [] = z_db:q(
                "alter table " ++ TableS ++
                " drop constraint fk_" ++ TableS ++ "_creator_id_id,"++
                " drop constraint fk_" ++ TableS ++ "_modifier_id_id,"++
                " alter column creator_id drop not null,"++
                " alter column modifier_id drop not null",
                Context),
            fk_setnull(Table, creator_id, Context),
            fk_setnull(Table, modifier_id, Context)
    end.

shared_table_columns() ->
    [
     #column_def{name=id, type="serial", is_nullable=false},
     #column_def{name=is_edit, type="boolean", is_nullable=false, default="false"},
     #column_def{name=creator_id, type="integer", is_nullable=true},
     #column_def{name=modifier_id, type="integer", is_nullable=true},
     #column_def{name=created, type="timestamp with time zone", is_nullable=true},
     #column_def{name=modified, type="timestamp with time zone", is_nullable=true},
     #column_def{name=managed_by, type="character varying", length=255, is_nullable=true},
     #column_def{name=is_block, type="boolean", is_nullable=false, default="false"},
     #column_def{name=actions, type="character varying", length=300, is_nullable=true}
    ].

fk_cascade(Table0, Field0, Context) ->
    Table = z_convert:to_list(Table0),
    Field = z_convert:to_list(Field0),
    z_db:equery(
        "alter table " ++ Table ++
        " add constraint fk_" ++ Table ++ "_" ++ Field ++ "_id" ++
        " foreign key (" ++ Field ++ ") references rsc(id) " ++
        " on update cascade " ++
        " on delete cascade", Context).

fk_setnull(Table0, Field0, Context) ->
    Table = z_convert:to_list(Table0),
    Field = z_convert:to_list(Field0),
    z_db:equery(
        "alter table " ++ Table ++
        " add constraint fk_" ++ Table ++ "_" ++ Field ++ "_id" ++
        " foreign key (" ++ Field ++ ") references rsc(id) " ++
        " on update cascade " ++
        " on delete set null", Context).


%% @doc Replace all rules of a certain kind/state
import_rules(Kind, State, Rules0, Context) ->
    Rules = names_to_ids(Rules0, Context),
    z_db:transaction(
        fun(Ctx) ->
            Query = "DELETE FROM " ++ z_convert:to_list(table(Kind))
                  ++ " WHERE " ++ state_sql_clause(State),
            z_db:equery(Query, Ctx),
            lists:foreach(
                    fun
                        ([]) ->
                            ok;
                        (R) ->
                            R1 = [
                                {modifier_id, z_acl:user(Context)},
                                {creator_id, z_acl:user(Context)}
                                | proplists:delete(modifier_id,
                                    proplists:delete(creator_id, R))
                            ],
                            z_db:insert(table(Kind), R1, Ctx)
                    end,
                    Rules)
        end,
        Context),
    mod_acl_user_groups:rebuild(State, Context).


ids_to_names(Rows, Context) ->
    [
        begin
            R1 = proplists:delete(id, R),
            ids_to_names_row(R1, Context)
        end
        || R <- Rows
    ].

ids_to_names_row(R, Context) ->
    lists:foldl(
        fun(K, Acc) ->
            case proplists:get_value(K, Acc) of
                Id when is_integer(Id) ->
                    case m_rsc:p_no_acl(Id, name, Context) of
                        undefined ->
                            % Problem this rule might be skipped on import
                            z_utils:prop_replace(K,
                                                {id, z_context:site(Context), m_rsc:is_a(Id, Context), Id},
                                                Acc);
                        Name ->
                            z_utils:prop_replace(K, Name, Acc)
                    end;
                undefined ->
                    Acc
            end
        end,
        R,
        fields()).

names_to_ids(Rows, Context) ->
    [
        begin
            R1 = proplists:delete(id, R),
            names_to_ids_row(R1, Context)
        end
        || R <- Rows
    ].

names_to_ids_row(R, Context) ->
    lists:foldl(
      fun
        (actions, Acc) ->
            A1 = implode_actions(proplists:get_value(actions, Acc)),
            z_utils:prop_replace(actions, A1, Acc);
        (K, Acc) ->
            case proplists:get_value(K, Acc) of
                Value when is_binary(Value) ->
                    case m_rsc:rid(Value, Context) of
                        undefined ->
                            case lists:member(K, [creator_id, modifier_id]) of
                                true ->
                                    z_utils:prop_replace(K, undefined, Acc);
                                false ->
                                    lager:notice("ACL import dropping rule, due to missing ~p ~p: ~p",
                                                 [K, Value, R]),
                                    []
                            end;
                        Id ->
                            z_utils:prop_replace(K, Id, Acc)
                    end;
                {id, Host, IsA, Id} ->
                    MyHost = z_context:site(Context),
                    MyIsA = m_rsc:is_a(Id, Context),
                    case {MyHost, MyIsA} of
                        {Host, IsA} ->
                            Acc;
                        _ ->
                            case lists:member(K, [creator_id, modifier_id]) of
                                true ->
                                    z_utils:prop_replace(K, undefined, Acc);
                                false ->
                                    lager:notice("ACL import dropping rule, due to missing ~p ~p: ~p",
                                                 [K, Id, R]),
                                    []
                            end
                    end;
                undefined ->
                    Acc
            end
      end,
      R,
      [actions|fields()]).

fields() ->
    [acl_user_group_id, category_id, content_group_id, creator_id, modifier_id].

implode_actions(L) ->
    string:join(
      [z_convert:to_list(K) || {K, true} <- L],
      ",").

%% @doc Delete ACL rules that are managed by a module
-spec delete_managed(atom(), #context{}) -> integer().
delete_managed(Module, Context) ->
    delete_managed(Module, rsc, Context)
        + delete_managed(Module, module, Context)
        + delete_managed(Module, collab, Context).

-spec delete_managed(atom(), atom(), #context{}) -> integer().
delete_managed(Module, Kind, Context) ->
    T = z_convert:to_list(table(Kind)),
    z_db:q("DELETE FROM " ++ T ++ " WHERE managed_by = $1", [Module], Context).
