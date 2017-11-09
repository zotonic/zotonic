%% @copyright 2015 Marc Worrell
%% @doc Model for named hierarchies

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

-module(m_hierarchy).

-export([
    m_get/2,
    tree/2,
    tree1/2,
    tree_flat/2,
    tree_flat/3,
    parents/3,
    contains/3,
    children/3,
    menu/2,
    ensure/2,
    ensure/3,
    append/3,
    save/3,
    save_nocheck/3,
    flush/2
]).

% For testing
-export([
    assign_nrs/2
]).

-include_lib("zotonic.hrl").

% Default delta between hierarchy items to minimize renumbering
-define(DELTA, 1000000).
-define(DELTA_MIN, 1000).
-define(DELTA_MAX, 2000000000). % ~ 1^31


%% @doc Fetch the value for the key from a model source
-spec m_get( list(), z:context()) -> {term(), list()}.
m_get([ Name, tree | Rest ], Context) -> {tree(Name, Context), Rest};
m_get([ Name, tree1 | Rest ], Context) -> {tree1(Name, Context), Rest};
m_get([ Name, tree_flat | Rest ], Context) -> {tree_flat(Name, Context), Rest};
m_get([ Name, menu | Rest ], Context) -> {menu(Name, Context), Rest};

m_get([ Name, menu_ensured | Rest ], Context) ->
    {ok, _} = ensure(Name, Context),
    m_get([ Name, menu | Rest ], Context);
m_get([ Name, Id, menu_ensured | Rest ], Context) ->
    {ok, _} = ensure(Name, Context),
    m_get([ Name, Id, menu | Rest ], Context);

m_get([ Name, Id, tree | Rest ], Context) ->
    V = case m_rsc:rid(Id, Context) of
        undefined -> undefined;
        RId -> tree({sub, Name, RId}, Context)
    end,
    {V, Rest};
m_get([ Name, Id, tree1 | Rest ], Context) ->
    V = case m_rsc:rid(Id, Context) of
        undefined -> undefined;
        RId -> tree1({sub, Name, RId}, Context)
    end,
    {V, Rest};
m_get([ Name, Id, tree_flat | Rest ], Context) ->
    V = case m_rsc:rid(Id, Context) of
        undefined -> undefined;
        RId -> tree_flat({sub, Name, RId}, Context)
    end,
    {V, Rest};
m_get([ Name, Id, menu | Rest ], Context) ->
    V = case m_rsc:rid(Id, Context) of
        undefined -> undefined;
        RId -> menu({sub, Name, RId}, Context)
    end,
    {V, Rest};

m_get(Vs, _Context) ->
    lager:error("Unknown ~p lookup: ~p", [?MODULE, Vs]),
    {undefined, []}.


%% @doc Fetch a named tree
tree({sub, Tree, RId}, Context) ->
    case find_tree_node(tree(Tree, Context), RId) of
        undefined -> [];
        {ok, Node} -> [Node]
    end;
tree(undefined, _Context) ->
    [];
tree(<<>>, _Context) ->
    [];
tree(Id, Context) when is_integer(Id) ->
    tree(m_rsc:p_no_acl(Id, name, Context), Context);
tree(Name, Context) when is_binary(Name) ->
    F = fun() ->
        CatTuples = z_db:q("
                select id, parent_id, lvl, lft, rght
                from hierarchy h
                where name = $1
                order by nr",
            [Name],
            Context),
        build_tree(CatTuples, [], [])
    end,
    z_depcache:memo(F, {hierarchy, Name}, ?DAY, [hierarchy, {hierarchy, Name}], Context);
tree(Name, Context) ->
    tree(z_convert:to_binary(Name), Context).

%% @doc Fetch a 1 level deep tree
tree1({sub, _, _} = Tree, Context) ->
    case tree(Tree, Context) of
        [] -> [];
        [Node] -> proplists:get_value(children, Node)
    end;
tree1(Name, Context) ->
    tree(Name, Context).


%% @doc Return a list of all this id's ancestor nodes
parents(Name, Id, Context) when is_binary(Name), is_integer(Id) ->
    case z_depcache:memo(
        fun() ->
            z_db:q1("SELECT parent_id FROM hierarchy WHERE name = $1 AND id = $2", [Name, Id], Context)
        end, {hierarchy_parent, Name, Id}, ?DAY, [hierarchy, {hierarchy, Name}], Context) of
        undefined ->
            [];
        P ->
            [P | parents(Name, P, Context)]
    end;
parents(Name, Id, Context) when is_atom(Name) ->
    parents(z_convert:to_binary(Name), Id, Context).

%% @doc Return a list of all the ids below the id, excluding the id itself
children(Name, Id, Context) ->
    case find_tree_node(tree(Name, Context), Id) of
        undefined -> [];
        {ok, Node} -> ids(proplists:get_value(children, Node))
    end.

%% @doc Return all ids in a node list
ids([]) ->
    [];
ids(Ns) ->
    ids(Ns, []).

ids([], Acc) ->
    Acc;
ids([N | Ns], Acc) ->
    Acc1 = [proplists:get_value(id, N) | ids(proplists:get_value(children, N), Acc)],
    ids(Ns, Acc1).

%% @doc Return the list of ids contained within (and including) the id.
contains(_Name, undefined, _Context) ->
    [];
contains(Name0, Id, Context) when is_integer(Id) ->
    Name = z_convert:to_binary(Name0),
    z_depcache:memo(
        fun() ->
            [{Lft, Rght}] = z_db:q(
                "SELECT lft, rght FROM hierarchy "
                "WHERE name = 'content_group' AND id = $1",
                [Id],
                Context
            ),
            R = z_db:q(
                "SELECT id FROM hierarchy "
                "WHERE name = 'content_group' AND lft >= $1 AND rght <= $2",
                [Lft, Rght],
                Context
            ),
            [CId || {CId} <- R]
        end,
        {hierarchy_contains, Name, Id},
        3600,
        [{hierarchy, Name}, Id],
        Context);
contains(Name, Id, Context) ->
    contains(Name, m_rsc:rid(Id, Context), Context).

%% @doc Make a flattened list with indentations showing the level of the tree entries.
%%      Useful for select lists.
tree_flat(Name, Context) ->
    List = flatten_tree(tree(Name, Context)),
    [
        [{indent, indent(proplists:get_value(level, E, 0))} | E]
        || E <- List
    ].

tree_flat(Name, Id, Context) ->
    tree_flat({sub, Name, Id}, Context).

%% @doc Transform a hierarchy to a menu structure
menu(Name, Context) ->
    tree_to_menu(tree(Name, Context), []).

tree_to_menu([], Acc) ->
    lists:reverse(Acc);
tree_to_menu([E | Rest], Acc) ->
    {id, Id} = proplists:lookup(id, E),
    {children, Cs} = proplists:lookup(children, E),
    Cs1 = tree_to_menu(Cs, []),
    tree_to_menu(Rest, [{Id, Cs1} | Acc]).


%% @doc Ensure that all resources are present in a hierarchy.
-spec ensure(atom()|binary()|string()|integer(), #context{}) ->
    {ok, integer()} | {error, term()}.
ensure(Category, Context) ->
    case m_category:name_to_id(Category, Context) of
        {ok, CatId} ->
            Name = m_rsc:p_no_acl(CatId, name, Context),
            ensure(Name, CatId, Context);
        {error, _} = Error ->
            Error
    end.

%% @doc Ensure that all resources of a certain category are present in a hierarchy.
-spec ensure(atom()|binary()|string(), atom()|integer()|string(), #context{}) ->
    {ok, integer()}.
ensure(Name, CatId, Context) when is_binary(Name), is_integer(CatId) ->
    {ok, Total} = z_db:transaction(
        fun(Ctx) ->
            Ids0 = z_db:q("select id from hierarchy where name = $1", [Name], Ctx),
            Ids = [Id || {Id} <- Ids0],
            F = fun(Id, {All, Acc}, _Ctx) ->
                case lists:member(Id, Ids) of
                    true -> {[Id | All], Acc};
                    false -> {[Id | All], [Id | Acc]}
                end
            end,
            {All, Missing} = m_category:fold(CatId, F, {[], []}, Ctx),
            {ok, N1} = append(Name, Missing, Ctx),
            {ok, N2} = remove(Name, Ids -- All, Ctx),
            {ok, N1 + N2}
        end,
        Context),
    flush(Name, Context),
    case Total of
        0 ->
            {ok, 0};
        _ ->
            z_notifier:notify(#hierarchy_updated{root_id = Name, predicate = undefined}, Context),
            {ok, Total}
    end;
ensure(Name, Category, Context) when not is_integer(Category) ->
    {ok, CatId} = m_category:name_to_id(Category, Context),
    ensure(Name, CatId, Context);
ensure(Name, CatId, Context) when not is_binary(Name) ->
    ensure(z_convert:to_binary(Name), CatId, Context).


%% @doc Save a new hierarchy, replacing a previous one.
save(Name, Tree, Context) ->
    case z_acl:is_allowed(use, mod_admin_config, Context) of
        true ->
            case m_category:name_to_id(Name, Context) of
                {ok, CatId} ->
                    Name1 = m_rsc:p_no_acl(CatId, name, Context),
                    save_nocheck(Name1, Tree, Context);
                {error, _} = Error ->
                    lager:warning("[m_hierarchy] Hierarchy save for unknown category ~p", [Name]),
                    Error
            end;
        false ->
            {error, eacces}
    end.

save_nocheck(Name, NewTree, Context) when is_binary(Name); is_atom(Name) ->
    NewFlat = flatten_save_tree(NewTree),
    {ok, New, Del} = z_db:transaction(
        fun(Ctx) ->
            save_nocheck_trans(Name, NewFlat, Ctx)
        end,
        Context),
    flush(Name, Context),
    z_notifier:notify(
        #hierarchy_updated{
            root_id = z_convert:to_binary(Name),
            predicate = undefined,
            inserted_ids = New,
            deleted_ids = Del
        }, Context),
    ok.

save_nocheck_trans(Name, NewFlat, Context) ->
    OldFlatNr = z_db:q("
                select id, parent_id, lvl, nr
                from hierarchy
                where name = $1
                order by nr
                for update",
        [Name],
        Context),
    OldFlat = [{Id, P, Lvl} || {Id, P, Lvl, _Nr} <- OldFlatNr],
    Diff = diffy_term:diff(OldFlat, NewFlat),
    NewFlatNr = assign_nrs(Diff, OldFlatNr),

    OldIds = [Id || {Id, _P, _Lvl, _Nr} <- OldFlatNr],
    NewIds = [Id || {Id, _P, _Lvl, _Nr} <- NewFlatNr],
    InsIds = NewIds -- OldIds,
    UpdIds = NewIds -- InsIds,
    DelIds = OldIds -- NewIds,

    lists:foreach(fun(Id) ->
        {Id, P, Lvl, Nr} = lists:keyfind(Id, 1, NewFlatNr),
        {Left, Right} = range(Id, NewFlatNr),
        z_db:q("
                            insert into hierarchy
                                (name, id, parent_id, lvl, nr, lft, rght)
                            values
                                ($1, $2, $3, $4, $5, $6, $7)",
            [Name, Id, P, Lvl, Nr, Left, Right],
            Context)
    end,
        InsIds),
    lists:foreach(fun(Id) ->
        {Id, P, Lvl, Nr} = lists:keyfind(Id, 1, NewFlatNr),
        {Left, Right} = range(Id, NewFlatNr),
        z_db:q("
                            update hierarchy
                            set parent_id = $3,
                                lvl = $4,
                                nr = $5,
                                lft = $6,
                                rght = $7
                            where name = $1
                              and id = $2
                              and (  parent_id <> $3
                                  or lvl <> $4
                                  or nr <> $5
                                  or lft <> $6
                                  or rght <> $7)",
            [Name, Id, P, Lvl, Nr, Left, Right],
            Context)
    end,
        UpdIds),
    lists:foreach(fun(Id) ->
        z_db:q("delete from hierarchy
                            where name = $1 and id = $2",
            [Name, Id],
            Context)
    end,
        DelIds),
    {ok, InsIds, DelIds}.

range(Id, [{Id, _P, Lvl, Nr} | Rest]) ->
    Right = range1(Lvl, Nr, Rest),
    {Nr, Right};
range(Id, [_ | Rest]) ->
    range(Id, Rest).

range1(Lvl, _Max, [{_Id, _P, Lvl1, Nr} | Rest]) when Lvl < Lvl1 ->
    range1(Lvl, Nr, Rest);
range1(_Lvl, Max, _Rest) ->
    Max.

%% @doc Go through the flattened tree and assign the range nrs
assign_nrs(Diff, OldFlatNr) ->
    IdNr = lists:foldl(fun({Id, _, _, Nr}, D) ->
        dict:store(Id, Nr, D)
    end,
        dict:new(),
        OldFlatNr),
    assign_nrs_1(Diff, [], 0, IdNr).

assign_nrs_1([], Acc, _LastNr, _IdNr) ->
    lists:reverse(Acc);
assign_nrs_1([{_Op, []} | Rest], Acc, LastNr, IdNr) ->
    assign_nrs_1(Rest, Acc, LastNr, IdNr);
assign_nrs_1([{equal, [{Id, P, Lvl} | Rs]} | Rest], Acc, LastNr, IdNr) ->
    PrevNr = dict:fetch(Id, IdNr),
    case erlang:max(PrevNr, LastNr + 1) of
        PrevNr ->
            Acc1 = [{Id, P, Lvl, PrevNr} | Acc],
            assign_nrs_1([{equal, Rs} | Rest], Acc1, PrevNr, IdNr);
        _ ->
            Diff1 = [{equal, Rs} | Rest],
            NewNr = case next_equal_nr(Diff1, IdNr) of
                        undefined ->
                            LastNr + ?DELTA;
                        EqNr when EqNr >= LastNr + 2 ->
                            LastNr + (EqNr-LastNr) div 2;
                        _ ->
                            LastNr + ?DELTA_MIN
                    end,
            Acc1 = [{Id, P, Lvl, NewNr} | Acc],
            assign_nrs_1(Diff1, Acc1, NewNr, IdNr)
    end;
assign_nrs_1([{insert, [{Id, P, Lvl} | Rs]} | Rest], Acc, LastNr, IdNr) ->
    CurrNr = case dict:find(Id, IdNr) of
        {ok, Nr} -> Nr;
        error -> undefined
    end,
    NextEq = next_equal_nr(Rest, IdNr),
    NewNr = new_nr(LastNr, CurrNr, NextEq),
    Acc1 = [{Id, P, Lvl, NewNr} | Acc],
    assign_nrs_1([{insert, Rs} | Rest], Acc1, NewNr, IdNr);
assign_nrs_1([{delete, _}|Rest], Acc, LastNr, IdNr) ->
    assign_nrs_1(Rest, Acc, LastNr, IdNr).

new_nr(Last, undefined, undefined) ->
    Last + ?DELTA;
new_nr(Last, Current, undefined) when Current > Last ->
    Current;
new_nr(Last, Current, Next) when Next =/= undefined, Current > Last, Next > Current ->
    Current;
new_nr(Last, _Current, Next) when Next =/= undefined, Next >= Last + 2 ->
    erlang:min(Last + ?DELTA, Last + (Next - Last) div 2);
new_nr(Last, _, _) ->
    % We have to shift the NextEq, as it is =< LastNr+2
    Last + ?DELTA_MIN.

next_equal_nr(Diff, IdNr) ->
    case next_equal(Diff) of
        undefined -> undefined;
        Id -> dict:fetch(Id, IdNr)
    end.

next_equal([]) -> undefined;
next_equal([{equal, [{Id, _, _} | _]} | _]) -> Id;
next_equal([_ | Ds]) -> next_equal(Ds).

flatten_save_tree(Tree) ->
    lists:reverse(flatten_save_tree(Tree, undefined, 1, [])).

flatten_save_tree([], _ParentId, _Lvl, Acc) ->
    Acc;
flatten_save_tree([{Id, Cs} | Ts], ParentId, Lvl, Acc) ->
    Acc1 = flatten_save_tree(Cs, Id, Lvl + 1, [{Id, ParentId, Lvl} | Acc]),
    flatten_save_tree(Ts, ParentId, Lvl, Acc1).

append(Name0, Missing, Context) ->
    Name = z_convert:to_binary(Name0),
    case append_1(Name, Missing, Context) of
        {ok, N} when N > 0 ->
            flush(Name, Context),
            z_notifier:notify(#hierarchy_updated{root_id = Name, predicate = undefined}, Context),
            {ok, N};
        {ok, 0} ->
            {ok, 0}
    end.

append_1(_Name, [], _Context) ->
    {ok, 0};
append_1(Name, Missing, Context) ->
    Nr = next_nr(Name, Context),
    lists:foldl(fun(Id, NextNr) ->
                    z_db:q("
                        insert into hierarchy (name, id, nr, lft, rght, lvl)
                        values ($1, $2, $3, $4, $5, 1)",
                        [Name, Id, NextNr, NextNr, NextNr],
                        Context),
                    NextNr+?DELTA
                end,
                Nr,
                Missing),
    {ok, length(Missing)}.

remove(_Name, [], _Context) ->
    {ok, 0};
remove(Name, Ids, Context) ->
    lists:foreach(fun(Id) ->
                    z_db:q("delete from hierarchy where name = $1 and id = $2", [Name, Id], Context)
                  end,
                  Ids),
    flush(Name, Context),
    {ok, length(Ids)}.

next_nr(Name, Context) ->
    case z_db:q1("select max(nr) from hierarchy where name = $1", [Name], Context) of
        undefined -> ?DELTA;
        Nr -> Nr + ?DELTA
    end.

flush(Name, Context) ->
    z_depcache:flush({hierarchy, z_convert:to_binary(Name)}, Context).


indent(Level) when Level =< 0 ->
    <<>>;
indent(Level) when is_integer(Level) ->
    iolist_to_binary(string:copies("&nbsp;&nbsp;&nbsp;&nbsp;", Level - 1)).

flatten_tree(Tree) ->
    lists:reverse(flatten_tree(Tree, [], [])).

flatten_tree([], _Path, Acc) ->
    Acc;
flatten_tree([E | Ts], Path, Acc) ->
    Acc1 = [[{path, Path} | E] | Acc],
    Path1 = [proplists:get_value(id, E) | Path],
    Acc2 = flatten_tree(proplists:get_value(children, E, []), Path1, Acc1),
    flatten_tree(Ts, Path, Acc2).


%% @doc Build a tree from the queried arguments
build_tree([], _Stack, Acc) ->
    lists:reverse(Acc);
build_tree([{Id, _Parent, _Lvl, _Left, _Right} = C | Rest], Stack, Acc) ->
    {C1, Rest1} = build_tree(C, [Id | Stack], [], Rest),
    build_tree(Rest1, Stack, [C1 | Acc]).

build_tree(
    {Id, _Parent, _Lvl, _Left, _Right} = P,
    Stack,
    Acc,
    [{Id2, Id, _Lvl2, _Left2, _Right2} = C | Rest]) ->
    {C1, Rest1} = build_tree(C, [Id2 | Stack], [], Rest),
    build_tree(P, Stack, [C1 | Acc], Rest1);
build_tree({Id, Parent, Lvl, Left, Right}, Stack, Acc, Rest) ->
    {[{id, Id},
        {parent_id, Parent},
        {level, Lvl},
        {children, lists:reverse(Acc)},
        {path, lists:reverse(Stack)},
        {left, Left},
        {right, Right}
    ],
        Rest}.


%% @doc Find a id and sub-tree in a tree (tree nodes are proplists)
find_tree_node([], _Id) ->
    undefined;
find_tree_node([Node | Ns], Id) ->
    case lists:keyfind(id, 1, Node) of
        {id, Id} ->
            {ok, Node};
        _ ->
            {children, Cs} = lists:keyfind(children, 1, Node),
            case find_tree_node(Cs, Id) of
                {ok, FoundNode} ->
                    {ok, FoundNode};
                undefined ->
                    find_tree_node(Ns, Id)
            end
    end.
