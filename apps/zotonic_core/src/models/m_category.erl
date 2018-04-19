%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2015 Marc Worrell
%%
%% @doc Model for categories.  Add, change and re-order categories.

%% Copyright 2009-2015 Marc Worrell
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

-module(m_category).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(zotonic_model).

%% interface functions
-export([
    m_get/2,

    flush/1,

    is_used/2,

    insert/4,
    delete/3,
    image/2,
    ensure_hierarchy/1,

    tree/1,
    tree2/1,
    tree_flat/1,
    tree_flat/2,
    tree_flat_meta/1,
    menu/1,

    tree/2,
    tree1/2,
    tree2/2,

    get/2,
    get_by_name/2,
    get_path/2,
    get_range/2,
    get_range_by_name/2,
    ranges/2,
    last_modified/2,
    is_a/2,
    is_a/3,
    is_meta/2,
    is_a_prim/3,
    name_to_id/2,
    id_to_name/2,
    foreach/3,
    fold/4,

    move_below/3,
    move_after/3,
    is_tree_dirty/1,
    renumber/1,
    renumber_pivot_task/1
]).

-type category() :: pos_integer() | atom() | binary() | string().

-include_lib("zotonic.hrl").

%% @doc Fetch the value for the key from a model source
-spec m_get( list(), z:context()) -> {term(), list()}.
m_get([ tree | Rest ], Context) ->
    {tree(Context), Rest};
m_get([ tree2 | Rest ], Context) ->
    {tree2(Context), Rest};
m_get([ menu | Rest ], Context) ->
    {menu(Context), Rest};
m_get([ tree_flat | Rest ], Context) ->
    {tree_flat(Context), Rest};
m_get([ tree_flat_meta | Rest ], Context) ->
    {tree_flat_meta(Context), Rest};
m_get([ is_used, Cat | Rest ], Context) ->
    {is_used(Cat, Context), Rest};
m_get([ Cat, path | Rest ], Context) ->
    V = case name_to_id(Cat, Context) of
        {ok, Id} -> get_path(Id, Context);
        {error, _} -> undefined
    end,
    {V, Rest};
m_get([ Cat, is_a | Rest ], Context) ->
    V = case name_to_id(Cat, Context) of
        {ok, Id} -> is_a(Id, Context);
        {error, _} -> undefined
    end,
    {V, Rest};
m_get([ Cat, tree | Rest ], Context) ->
    V = case name_to_id(Cat, Context) of
        {ok, Id} -> tree(Id, Context);
        {error, _} -> undefined
    end,
    {V, Rest};
m_get([ Cat, tree_flat | Rest ], Context) ->
    V = case name_to_id(Cat, Context) of
        {ok, Id} -> tree_flat(Id, Context);
        {error, _} -> undefined
    end,
    {V, Rest};
m_get([ Cat, tree1 | Rest ], Context) ->
    V = case name_to_id(Cat, Context) of
        {ok, Id} -> tree1(Id, Context);
        {error, _} -> undefined
    end,
    {V, Rest};
m_get([ Cat, tree2 | Rest ], Context) ->
    V = case name_to_id(Cat, Context) of
        {ok, Id} -> tree2(Id, Context);
        {error, _} -> undefined
    end,
    {V, Rest};
m_get([ Cat, image | Rest ], Context) ->
    V = case name_to_id(Cat, Context) of
        {ok, Id} -> image(Id, Context);
        {error, _} -> undefined
    end,
    {V, Rest};
m_get([ Cat | Rest ], Context) ->
    V = case name_to_id(Cat, Context) of
        {ok, Id} -> get(Id, Context);
        {error, _} -> undefined
    end,
    {V, Rest};
m_get(Vs, _Context) ->
    lager:error("Unknown ~p lookup: ~p", [?MODULE, Vs]),
    {undefined, []}.


% ======================================== API =======================================

-spec flush(#context{}) -> ok.
flush(Context) ->
    m_hierarchy:flush('$category', Context),
    z_depcache:flush(category, Context).

%% @doc Check if a category is actually in use.
is_used(Category, Context) ->
    Id = m_rsc:rid(Category, Context),
    Ids = [Id | m_hierarchy:children('$category', Id, Context)],
    lists:any(fun(CatId) ->
                 z_db:q1("select id from rsc where category_id = $1 limit 1", [CatId], Context) =/= undefined
              end,
              Ids).


%% Insert a category
-spec insert(undefined|integer(), binary()|atom()|string(), list(), #context{}) -> integer().
insert(ParentId, Name, Props, Context) ->
    {ok, CatId} = name_to_id(category, Context),
    {ok, Id} = m_rsc_update:insert(Props ++ [{name, Name}, {category_id, CatId}], Context),
    case ParentId of
        undefined ->
            Id;
        _ ->
            move_below(Id, ParentId, Context),
            Id
    end.


%% @doc Delete the category, move referring pages to another category.
%%      After this routine the caches are dirty and child-categories might need renumbering if a TransferId
%%      was defined and there were sub-categories.
-spec delete(m_rsc:resource(), integer() | undefined, #context{}) -> ok | {error, term()}.
delete(Id, TransferId, Context) ->
    % fail when deleting 'other', 'meta', 'category' or 'predicate'
    case z_db:q("select name from rsc where id = $1", [m_rsc:rid(Id, Context)], Context) of
        N when N == <<"other">>;
            N == <<"meta">>;
            N == <<"category">>;
            N == <<"predicate">> ->
            {error, is_system_category};
        _ ->
            case z_acl:is_allowed(delete, Id, Context)
                andalso z_acl:is_allowed(insert, category, Context)
            of
                true ->
                    F = fun(Ctx) ->
                        ParentId = z_db:q1("select parent_id
                                              from hierarchy
                                                where id = $1
                                                 and name = '$category'",
                                              [m_rsc:rid(Id, Context)],
                                              Ctx),
                        ToId = case {TransferId, ParentId} of
                                   {undefined, undefined} ->
                                       %% The removed category is a top-category, move all content to 'other'
                                       case z_db:q1("
                                                select c.id
                                                from rsc r
                                                    join hierarchy c
                                                    on c.id = r.id and c.name = '$category'
                                                where r.name = 'other'", Ctx) of
                                           N when is_integer(N) -> N
                                       end;
                                   {undefined, ParentId} ->
                                       ParentId;
                                   {TransferId, _ParentId} ->
                                       TransferId = z_db:q1("select id
                                                              from hierarchy
                                                              where id = $1
                                                                and name = '$category'",
                                                             [TransferId],
                                                             Ctx)
                                end,

                        % Move all sub-categories of the deleted category one level "up"
                        case z_db:q("update hierarchy
                                     set parent_id = $1
                                     where parent_id = $2
                                       and name = '$category'",
                                    [ParentId, Id],
                                    Ctx)
                        of
                            0 -> ok;
                            _ -> set_tree_dirty(true, Ctx)
                        end,

                        % Move all resources to the new category
                        ToNr = z_db:q1("select nr
                                        from hierarchy
                                        where id = $1
                                          and name = '$category'",
                                       [ToId],
                                       Ctx),
                        z_db:q("update rsc
                                set category_id = $1,
                                    pivot_category_nr = $2
                                where category_id = $3",
                               [ToId, ToNr, m_rsc:rid(Id, Context)],
                               Ctx),
                        ok = m_rsc_update:delete_nocheck(Id, Ctx)
                    end,
                    ok = z_db:transaction(F, Context),
                    renumber_if_dirty(Context);
                false ->
                    {error, eacces}
            end
    end.


%% @doc Return a random depiction of some resource with the given category.
-spec image(category(), z:context()) -> integer() | undefined.
image(Cat, Context) ->
    case name_to_id(Cat, Context) of
        {ok, Id} ->
            F = fun() ->
                #search_result{result = Result1} = z_search:search(
                    {media_category_image, [{cat, Id}]},
                    Context
                ),
                #search_result{result = Result2} = z_search:search({
                    media_category_depiction,
                    [{cat, Id}]},
                    Context
                ),
                Result1 ++ Result2
            end,
            Files = z_depcache:memo(F, {category_image, Id}, ?DAY, [category], Context),
            case Files of
                [] -> undefined;
                _ -> lists:nth(z_ids:number(length(Files)), Files)
            end;
        {error, _} ->
            undefined
    end.


%% @doc Return the category tree, every entry is a proplist.
-spec tree(#context{}) -> list(list()).
tree(Context) ->
    m_hierarchy:tree('$category', Context).

%% @doc Return the flattened category tree, every entry is a proplist. Used for select lists.
-spec tree_flat(#context{}) -> list(list()).
tree_flat(Context) ->
    {ok, MetaId} = name_to_id(meta, Context),
    lists:filter(fun(Cat) ->
                    case proplists:get_value(id, Cat) of
                        MetaId ->
                            false;
                        _ ->
                            case proplists:get_value(path, Cat) of
                                [MetaId|_] -> false;
                                _ -> true
                            end
                    end
                 end,
                 tree_flat_meta(Context)).

%% @doc Return the flattened category tree, every entry is a proplist. Used for select lists.
-spec tree_flat(category(), #context{}) -> list(list()).
tree_flat(CatId, Context) ->
    case name_to_id(CatId, Context) of
        {ok, Id} ->
            m_hierarchy:tree_flat('$category', Id, Context);
        {error, _} ->
            []
    end.


%% @doc Return the flattened category tree, every entry is a proplist. Used for select lists.
-spec tree_flat_meta(#context{}) -> list(list()).
tree_flat_meta(Context) ->
    m_hierarchy:tree_flat('$category', Context).

%% @doc Return the category tree from the category down, max children of children
-spec tree2(#context{}) -> list() | undefined.
tree2(Context) ->
    prune(2, tree(Context)).

%% @doc Return the menu representation of the category tree.
-spec menu(#context{}) -> list({integer(), list()}).
menu(Context) ->
    m_hierarchy:menu('$category', Context).

%% @doc Return the category tree from the category down
-spec tree(category(), #context{}) -> list() | undefined.
tree(Cat, Context) ->
    case name_to_id(Cat, Context) of
        {ok, Id} ->
            case find_tree_node(tree(Context), Id) of
                {ok, Tree} -> Tree;
                undefined -> undefined
            end;
        {error, _} ->
            undefined
    end.

%% @doc Return the category trees below the category
-spec tree1(category(), #context{}) -> list() | undefined.
tree1(Cat, Context) ->
    case tree(Cat, Context) of
        undefined -> undefined;
        Node -> proplists:get_value(children, Node)
    end.

%% @doc Return the category tree from the category down, max children of children
-spec tree2(category(), #context{}) -> list() | undefined.
tree2(Cat, Context) ->
    case tree(Cat, Context) of
        undefined ->
            undefined;
        Node ->
            [{children, prune(2, proplists:get_value(children, Node))}
                | proplists:delete(children, Node)
            ]
    end.

prune(_N, []) ->
    [];
prune(1, CS) ->
    [
        [{children, []}
            | proplists:delete(children, C)
        ]
        || C <- CS
    ];
prune(N, CS) ->
    [
        [{children, prune(N - 1, proplists:get_value(children, C))}
            | proplists:delete(children, C)
        ]
        || C <- CS
    ].


%% @doc Get the basic properties of a category
-spec get(m_rsc:resource(), #context{}) -> list() | undefined.
get(undefined, _Context) ->
    undefined;
get(Id, Context) when is_integer(Id) ->
    F = fun() ->
            case lists:dropwhile(fun(Cat) ->
                                    proplists:get_value(id, Cat) =/= Id
                                 end,
                                 tree_flat_meta(Context))
            of
                [] ->
                    undefined;
                [C|_] ->
                    {path, PathIds} = proplists:lookup(path, C),
                    PathNames = [ z_convert:to_atom(m_rsc:p_no_acl(CId, name, Context)) || CId <- PathIds ],
                    Name = z_convert:to_atom(m_rsc:p_no_acl(Id, name, Context)),
                    IsA = lists:reverse([Name|PathNames]),
                    [
                        {name, Name},
                        {is_a, IsA}
                        | C
                    ]
            end
    end,
    z_depcache:memo(F, {category, Id}, ?WEEK, [category], Context);
get(Name, Context) ->
    get_by_name(Name, Context).

-spec get_by_name(category(), #context{}) -> list() | undefined.
get_by_name(Name, Context) ->
    case name_to_id(Name, Context) of
        {ok, Id} -> get(Id, Context);
        {error, _} -> undefined
    end.

get_range(Id, Context) ->
    case get(Id, Context) of
        undefined ->
            {1, 0}; % empty range
        C when is_list(C) ->
            {proplists:get_value(left, C),
                proplists:get_value(right, C)}
    end.

get_range_by_name(Name, Context) ->
    case get_by_name(Name, Context) of
        undefined ->
            {1, 0}; % empty range
        C when is_list(C) ->
            {proplists:get_value(left, C),
                proplists:get_value(right, C)}
    end.


%% @doc Given a list of category ids, return the list of numeric ranges they cover.
-spec ranges(category() | [category()], #context{}) ->
    [{integer(), integer()}].
ranges(undefined, _Context) ->
    [];
ranges([], _Context) ->
    [];
ranges(Cat, Context) when is_atom(Cat); is_integer(Cat); is_binary(Cat) ->
    ranges([Cat], Context);
ranges(CatList0, Context) ->
    CatList = case length(CatList0) > 1 andalso z_string:is_string(CatList0) of
                  true -> [CatList0];
                  false -> CatList0
              end,
    F = fun
            (undefined, Acc) ->
                Acc;
            ('$error', Acc) ->
                [{-1, -1} | Acc];
            (Nm, Acc) ->
                case get(Nm, Context) of
                    undefined -> [{-1, -1} | Acc];
                    Props -> [{proplists:get_value(left, Props), proplists:get_value(right, Props)} | Acc]
                end
        end,
    Ranges = lists:sort(lists:foldl(F, [], flatten_string(CatList, []))),
    maybe_drop_empty_range(merge_ranges(Ranges, [])).

maybe_drop_empty_range([]) ->
    [];
maybe_drop_empty_range([_] = Range) ->
    Range;
maybe_drop_empty_range(Ranges) ->
    case [Range || Range <- Ranges, Range =/= {-1, -1}] of
        [] -> [{-1, -1}];
        Ranges1 -> Ranges1
    end.

%% Flatten the list of cats, but do not flatten strings
flatten_string([], Acc) ->
    Acc;
flatten_string([[A | _] = L | T], Acc) when is_list(A); is_atom(A); is_binary(A); is_tuple(A) ->
    Acc1 = flatten_string(L, Acc),
    flatten_string(T, Acc1);
flatten_string([H | T], Acc) ->
    flatten_string(T, [H | Acc]).


merge_ranges([], Acc) ->
    Acc;
merge_ranges([{A, B}, {C, D} | T], Acc) when C =< B + 1 ->
    merge_ranges([{A, erlang:max(B, D)} | T], Acc);
merge_ranges([H | T], Acc) ->
    merge_ranges(T, [H | Acc]).


%% @doc Return the path from a root to the category. Excluding the category
%% itself, most specific last.
-spec get_path(category(), #context{}) -> list(integer()).
get_path(undefined, _Context) ->
    [];
get_path(Id, Context) ->
    case get(Id, Context) of
        undefined -> [];
        C -> proplists:get_value(path, C)
    end.

%% @doc Return the categories (as atoms) the category is part of, including the
%% category itself (as last member).
-spec is_a(m_rsc:resource(), #context{}) -> list(atom()).
is_a(Id, Context) ->
    case get(Id, Context) of
        undefined -> [];
        C -> proplists:get_value(is_a, C)
    end.


%% @doc Check if the id is within a category.
-spec is_a(category(), atom(), #context{}) -> boolean().
is_a(Id, Cat, Context) ->
    CatName = m_category:id_to_name(Cat, Context),
    lists:member(CatName, is_a(Id, Context)).

%% @doc Check if a category is a meta category. This can't use the m_rsc routines as it is also
%%      used to determine the default content group during the m_rsc:get/2
-spec is_meta(integer(), #context{}) -> boolean().
is_meta(CatId, Context) when is_integer(CatId) ->
    is_a_prim(CatId, <<"meta">>, Context).

%% @doc Check if a category is within another category. This can be used within primitive rsc
%%      routines that are not able to use the rsc caching (due to recursion).
-spec is_a_prim(integer(), binary()|string()|atom(), #context{}) -> boolean().
is_a_prim(CatId, Name, Context) ->
    z_depcache:memo(
        fun() ->
             1 =:= z_db:q1("
                    select count(*)
                    from hierarchy a,
                         hierarchy b
                    where a.name = '$category'
                      and b.name = '$category'
                      and a.id = (select id from rsc where name = $2)
                      and b.id = $1
                      and b.lft >= a.lft
                      and b.rght <= a.rght",
                    [CatId, Name],
                    Context)
        end,
        {is_category_prim, Name, CatId},
        ?WEEK,
        [{hierarchy, <<"$category">>}],
        Context).

%% @doc Map a category name to an id, be flexible with the input
-spec name_to_id(m_rsc:resource() | {m_rsc:resource_id()}, #context{}) ->
    {ok, m_rsc:resource_id()} | {error, {unknown_category, term()}}.
name_to_id({Id}, _Context) when is_integer(Id) ->
    {ok, Id};
name_to_id(Id, _Context) when is_integer(Id) ->
    {ok, Id};
name_to_id(undefined, _Context) ->
    {error, {unknown_category, undefined}};
name_to_id(Name, Context) when is_atom(Name); is_binary(Name); is_list(Name) ->
    case z_depcache:get({category_name_to_id, Name}, Context) of
        {ok, Result} ->
            Result;
        undefined ->
            Result = case z_db:q1("
                            select r.id
                            from rsc r
                                join hierarchy c
                                on r.id = c.id
                                and c.name = '$category'
                            where r.name = $1", [Name], Context)
                     of
                        undefined -> {error, {unknown_category, Name}};
                        Id -> {ok, Id}
                     end,
            case Result of
                {ok, ResultId} ->
                    z_depcache:set(
                        {category_name_to_id, Name},
                        Result, ?DAY,
                        [category, ResultId],
                        Context
                    );
                {error, _Error} ->
                    z_depcache:set(
                        {category_name_to_id, Name},
                        Result,
                        ?DAY,
                        [category],
                        Context
                    )
            end,
            Result
    end.

%% @doc Perform a function on all resource ids in a category. Order of the ids
%% is unspecified.
-spec foreach(Category :: integer()|atom(), function(), #context{})
        -> ok | {error, term()}.
foreach(Category, F, Context) ->
    case name_to_id(Category, Context) of
        {ok, Id} ->
            {From, To} = get_range(Id, Context),
            Ids = z_db:q(
                "select id from rsc "
                "where pivot_category_nr >= $1 "
                "and pivot_category_nr <= $2",
                [From, To],
                Context
            ),
            lists:foreach(fun({RscId}) ->
                F(RscId, Context)
            end,
                Ids),
            ok;
        {error, _} = Error ->
            Error
    end.

%% @doc Perform a function on all resource ids in a category. Order of the ids
%% is unspecified.
-spec fold(Category :: integer()|atom(), function(), term(), #context{})
        -> term() | {error, term()}.
fold(Category, F, Acc0, Context) ->
    case name_to_id(Category, Context) of
        {ok, Id} ->
            {From, To} = get_range(Id, Context),
            Ids = z_db:q(
                "select id from rsc "
                "where pivot_category_nr >= $1 "
                "and pivot_category_nr <= $2",
                [From, To],
                Context
            ),
            lists:foldl(fun({RscId}, Acc) ->
                F(RscId, Acc, Context)
            end,
                Acc0,
                Ids);
        {error, _} = Error ->
            Error
    end.

%% @doc Return the last modification date of the category. Returns false
-spec last_modified(category(), #context{}) -> {ok, calendar:datetime()} | {error, term()}.
last_modified(Cat, Context) ->
    case name_to_id(Cat, Context) of
        {ok, CatId} ->
            {Left, Right} = get_range(CatId, Context),
            case z_db:q1(
                "select max(modified) from rsc "
                "where pivot_category_nr >= $1 "
                "and pivot_category_nr <= $2",
                [Left, Right],
                Context
            ) of
                false -> {error, {no_rsc_in_cat, CatId}};
                Date -> {ok, Date}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


%% @doc Return the name for a given category.
%%
%% If the category does not have a unique name will result in undefined.
%% If the lookup is made by name, the name is checked for existence,
%% and if not found, results in undefined.
-spec id_to_name(category(), #context{}) -> atom() | undefined.
id_to_name(Name, Context) when is_atom(Name); is_binary(Name); is_list(Name) ->
    F = fun() ->
        Nm = z_db:q1("
                    select r.name
                    from rsc r
                        join hierarchy c
                        on r.id = c.id
                        and c.name = '$category'
                    where r.name = $1", [Name], Context),
        z_convert:to_atom(Nm)
    end,
    z_depcache:memo(F, {category_id_to_name, Name}, ?DAY, [category], Context);
id_to_name(Id, Context) when is_integer(Id) ->
    F = fun() ->
        Nm = z_db:q1("select r.name
                      from rsc r
                            join hierarchy c
                            on r.id = c.id
                            and c.name = '$category'
                      where r.id = $1", [Id], Context),
        z_convert:to_atom(Nm)
    end,
    z_depcache:memo(F, {category_id_to_name, Id}, ?DAY, [category], Context).


%% @doc Check if the category tree is dirty (e.g. resource pivot numbers are being updated)
-spec is_tree_dirty(#context{}) -> boolean().
is_tree_dirty(Context) ->
    case m_config:get(?MODULE, meta, Context) of
        undefined -> false;
        Props -> proplists:get_value(tree_dirty, Props, false)
    end.

%% @doc Set the tree dirty flag
-spec set_tree_dirty(boolean(), #context{}) -> ok.
set_tree_dirty(Flag, Context) when Flag =:= true; Flag =:= false ->
    m_config:set_prop(?MODULE, meta, tree_dirty, Flag, Context).


%% @doc Ensure that all categories are present in the $category hierarchy.
%%      This appends any newly found categories to the end of the category tree.
-spec ensure_hierarchy(z:context()) -> ok | {error, renumbering}.
ensure_hierarchy(Context) ->
    case is_tree_dirty(Context) of
        false ->
            {ok, CatId} = name_to_id(category, Context),
            case m_hierarchy:ensure('$category', CatId, Context) of
                {ok, N} when N > 0 ->
                    lager:warning("Ensure category found ~p new categories.", [N]),
                    flush(Context);
                {ok, 0} ->
                    ok
            end;
        true ->
            lager:warning("Ensure category requested while renumbering."),
            {error, renumbering}
    end.


%% @doc Move a category below another category (or the root set if undefined)
-spec move_below(integer(), integer(), #context{}) -> ok.
move_below(Cat, Parent, Context) ->
    {ok, Id} = name_to_id(Cat, Context),
    ParentId = maybe_name_to_id(Parent, Context),
    Tree = menu(Context),
    {ok, {Tree1, Node, PrevParentId}} = remove_node(Tree, Id, undefined),
    case PrevParentId of
        ParentId ->
            ok;
        _ ->
            Tree2 = insert_node(ParentId, Node, Tree1, []),
            m_hierarchy:save_nocheck('$category', Tree2, Context),
            flush(Context),
            renumber(Context)
    end.

%% @doc Move a category after another category (on the same level).
-spec move_after(pos_integer(), pos_integer(), z:context()) -> ok.
move_after(Cat, After, Context) ->
    {ok, Id} = name_to_id(Cat, Context),
    AfterId = maybe_name_to_id(After, Context),
    Tree = menu(Context),
    {ok, {Tree1, Node, _}} = remove_node(Tree, Id, undefined),
    case insert_after(AfterId, Node, Tree1, []) of
        Tree1 ->
            ok;
        NewTree ->
            m_hierarchy:save_nocheck('$category', NewTree, Context),
            flush(Context),
            renumber(Context)
    end.

maybe_name_to_id(undefined, _Context) ->
    undefined;
maybe_name_to_id(Id, _Context) when is_integer(Id) ->
    Id;
maybe_name_to_id(Name, Context) ->
    {ok, Id} = name_to_id(Name, Context),
    Id.

insert_node(undefined, Node, Tree, []) ->
    Tree ++ [Node];
insert_node(_ParentId, _Node, [], Acc) ->
    lists:reverse(Acc);
insert_node(ParentId, Node, [{ParentId, TCs} | Tree], Acc) ->
    lists:reverse(Acc, [{ParentId, TCs ++ [Node]} | Tree]);
insert_node(ParentId, Node, [{TId, TCs} | Tree], Acc) ->
    T1 = {TId, insert_node(ParentId, Node, TCs, [])},
    insert_node(ParentId, Node, Tree, [T1 | Acc]).

insert_after(_AfterId, _Node, [], Acc) ->
    lists:reverse(Acc);
insert_after(AfterId, Node, [{AfterId, Children} | Tree], Acc) ->
    lists:reverse(Acc, [{AfterId, Children}] ++ [Node | Tree]);
insert_after(AfterId, Node, [{CurrId, Children} | Tree], Acc) ->
    Tree1 = {CurrId, insert_after(AfterId, Node, Children, [])},
    insert_after(AfterId, Node, Tree, [Tree1 | Acc]).

remove_node([], _Id, _ParentId) ->
    notfound;
remove_node([{Id, _Cs} = Node | Ts], Id, ParentId) ->
    {ok, {Ts, Node, ParentId}};
remove_node([{TId, TCs} | Ts], Id, ParentId) ->
    case remove_node(TCs, Id, ParentId) of
        {ok, {TCs1, Node, PId}} ->
            {ok, {[{TId, TCs1} | Ts], Node, PId}};
        notfound ->
            case remove_node(Ts, Id, ParentId) of
                {ok, {Ts1, Node, PId}} ->
                    {ok, {[{TId, TCs} | Ts1], Node, PId}};
                notfound ->
                    notfound
            end
    end.


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


%% @doc Start synchronizing all resources if the category tree is marked dirty.
-spec renumber_if_dirty(#context{}) -> ok.
renumber_if_dirty(Context) ->
    case is_tree_dirty(Context) of
        true -> renumber(Context);
        false -> ok
    end.

%% @doc Start synchronizing all resources, so that the pivot_category_nr is in
%% sync with the category hierarchy.
-spec renumber(#context{}) -> ok.
renumber(Context) ->
    set_tree_dirty(true, Context),
    z_pivot_rsc:insert_task_after(10, ?MODULE, renumber_pivot_task, "m_category:renumber", [], Context),
    ok.

%% @doc Resync all ids that have their pivot_category_nr changed.
-spec renumber_pivot_task(#context{}) -> ok | {delay, integer()}.
renumber_pivot_task(Context) ->
    Nrs = z_db:q("
                select r.id, c.nr
                from rsc r
                    join hierarchy c
                    on c.id = r.category_id
                    and c.name = '$category'
                where c.id = r.category_id
                  and (r.pivot_category_nr is null or r.pivot_category_nr <> c.nr)
                limit 1000", Context, 60000),
    case Nrs of
        [] ->
            lager:info("Category renumbering completed", Context),
            set_tree_dirty(false, Context),
            ok;
        Ids ->
            ok = z_db:transaction(fun(Ctx) ->
                lists:foreach(
                    fun({Id, CatNr}) ->
                        z_db:q(
                            "update rsc set pivot_category_nr = $2
                            where id = $1
                            and (pivot_category_nr is null or pivot_category_nr <> $2)",
                            [Id, CatNr],
                            Ctx
                        )
                    end,
                    Ids),
                ok
            end,
                Context),
            {delay, 1}
    end.
