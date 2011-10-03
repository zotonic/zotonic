%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-04-08
%%
%% @doc Model for categories.  Add, change and re-order categories.

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

-module(m_category).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,
    get/2,
    get_by_name/2,
    get_by_parent/2,
    get_root/1,
    get_range/2,
    get_range_by_name/2,
    get_path/2,
    last_modified/2,
    is_a/2,
    is_a/3,
    insert/4,
    get_page_count/2,
    delete/3,
	image/2,
    name_to_id/2,
    name_to_id_check/2,
    id_to_name/2,
    move/2,
    move_below/3,
    move_end/2,
    move_before/3,
    update_sequence/2,
    all_flat/1,
    all_flat/2,
    all_flat_meta/1,
    ranges/2,
    menu/1,
    tree/1,
    tree/2,
    tree_depth/2,
    tree_depth/3,
    renumber/1,
    renumber_pivot_task/1,
    enumerate/1,
    boundaries/2      
]).


-include_lib("zotonic.hrl").


%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(tree, #m{value=undefined}, Context) ->
    tree(Context);
m_find_value(tree1, #m{value=undefined}, Context) ->
    get_root(Context);
m_find_value(tree2, #m{value=undefined}, Context) ->
    tree_depth(2, Context);
m_find_value(menu, #m{value=undefined}, Context) ->
    menu(Context);
m_find_value(all_flat, #m{value=undefined}, Context) ->
    all_flat(Context);
m_find_value(all_flat_meta, #m{value=undefined}, Context) ->
    all_flat_meta(Context);

m_find_value(Index, #m{value=undefined} = M, Context) ->
    case name_to_id(Index, Context) of
        {ok, Id} -> M#m{value={cat, Id}};
        {error, _} -> undefined
    end;

m_find_value(tree, #m{value={cat, Id}}, Context) ->
    tree(Id, Context);
m_find_value(tree1, #m{value={cat, Id}}, Context) ->
    get_by_parent(Id, Context);
m_find_value(tree2, #m{value={cat, Id}}, Context) ->
    tree_depth(Id, 2, Context);
m_find_value(path, #m{value={cat, Id}}, Context) ->
    get_path(Id, Context);
m_find_value(image, #m{value={cat, Id}}, Context) ->
    image(Id, Context);
m_find_value(all_flat, #m{value={cat, Id}}, Context) ->
    all_flat(Id, Context);
m_find_value(Key, #m{value={cat, Id}}, Context) ->
    proplists:get_value(Key, get(Id, Context));
m_find_value(_Key, _Value, _Context) ->
    undefined.

%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context) -> List
m_to_list(#m{value=undefined}, Context) ->
    tree(Context);
m_to_list(#m{value={cat, Id}}, Context) ->
    get(Id, Context);
m_to_list(_, _Context) ->
    [].
    
%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, Context) ->
    tree(Context);
m_value(#m{value=#m{value={cat, Id}}}, Context) ->
    get(Id, Context).

get(Name, Context) when not is_integer(Name) ->
	get_by_name(Name, Context);
get(Id, Context) ->
    F = fun() ->
        z_db:assoc_props_row("
                select c.*, r.name
                from category c join rsc r on r.id = c.id
                where c.id = $1", [Id], Context)
    end,
    z_depcache:memo(F, {category, Id}, ?WEEK, [category], Context).

get_by_name(Name, Context) ->
    F = fun() ->
        z_db:assoc_props_row("
                select c.*, r.name 
                from category c join rsc r on r.id = c.id
                where r.name = $1", [Name], Context)
    end,
    z_depcache:memo(F, {category_by_name, Name}, ?WEEK, [category], Context).

get_root(Context) ->
    F = fun() ->
        z_db:assoc_props("
                select c.*, r.name 
                from category c join rsc r on c.id = r.id 
                where c.parent_id is null
                order by c.nr", Context)
    end,
    z_depcache:memo(F, {category_root}, ?WEEK, [category], Context).

get_by_parent(Id, Context) ->
    F = fun() ->
        case Id of
            undefined ->
                get_root(Context);
            _ ->
                z_db:assoc_props("
                    select c.*, r.name 
                    from category c join rsc r on r.id = c.id
                    where c.parent_id = $1 order by c.nr", [Id], Context)
        end
    end,
    z_depcache:memo(F, {category_parent, Id}, ?WEEK, [category], Context).

get_range(Id, Context) ->
    F = fun() ->
        case z_db:q("
                select lft, rght 
                from category 
                where id = $1", [Id], Context) of
            [Row] -> Row;
            _ -> {1, 0} % empty range
        end
    end,
    z_depcache:memo(F, {category_range, Id}, ?WEEK, [category], Context).

get_range_by_name(Name, Context) ->
    F = fun() ->
        case z_db:q("
                select c.lft, c.rght
                from category c join rsc r on r.id = c.id
                where r.name = $1", [Name], Context) of
            [Row] -> Row;
            _ -> {1, 0} % empty range
        end
    end,
    z_depcache:memo(F, {category_range_name, Name}, ?WEEK, [category], Context).

get_page_count(Id, Context) ->
    z_db:q1("select count(*) from rsc where category_id = $1", [Id], Context).


name_to_id({Id}, _Context) when is_integer(Id) ->
    {ok, Id};
name_to_id(Id, _Context) when is_integer(Id) ->
    {ok, Id};
name_to_id(Name, Context) ->
    case z_depcache:get({category_name_to_id, Name}, Context) of
        {ok, Result} ->
            Result;
        undefined ->
            Result = case z_db:q1("
                    select r.id 
                    from rsc r join category c on r.id = c.id 
                    where r.name = $1", [Name], Context) of
                undefined -> {error, {unknown_category, Name}};
                Id -> {ok, Id}
            end,
            case Result of
                {ok, ResultId} ->
                    z_depcache:set({category_name_to_id, Name}, Result, ?WEEK, [category, ResultId], Context);
                {error, _Error} ->
                    z_depcache:set({category_name_to_id, Name}, Result, ?WEEK, [category], Context)
            end,
            Result
    end.

name_to_id_check(Name, Context) ->
    {ok, Id} = name_to_id(Name, Context),
    Id.

id_to_name(Name, Context) when is_atom(Name); is_binary(Name); is_list(Name) ->
    F = fun() ->
        Nm = z_db:q1("select r.name from rsc r join category c on r.id = c.id where r.name = $1", [Name], Context),
        z_convert:to_atom(Nm)
    end,
    z_depcache:memo(F, {category_id_to_name, Name}, ?WEEK, [category], Context);
id_to_name(Id, Context) when is_integer(Id) ->
    F = fun() ->
        Nm = z_db:q1("select r.name from rsc r join category c on r.id = c.id where r.id = $1", [Id], Context),
        z_convert:to_atom(Nm)
    end,
    z_depcache:memo(F, {category_id_to_name, Id}, ?WEEK, [category], Context).


%% @doc Return the last modification date of the category. Returns false
%% @spec last_modified(Cat::term(), Context) -> {ok, {{Y,M,D},{Hour,Min,Sec}}} | {error, Reason}
last_modified(Cat, Context) ->
    case name_to_id(Cat, Context) of
        {ok, CatId} ->
            {Left, Right} = get_range(CatId, Context),
            case z_db:q1("select max(modified) from rsc where pivot_category_nr >= $1 and pivot_category_nr <= $2", [Left, Right], Context) of
                false -> {error, {no_rsc_in_cat, CatId}};
                Date -> {ok, Date}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


%% @doc Move categories to the given positions, no checks (except ACL done)
move(Cats, Context) ->
    [
        true = z_acl:is_allowed(update, Id, Context)
        || {Id, _ParentId, _Seq} <- Cats
    ],
    F = fun(Ctx) ->
            [
                z_db:q("update category 
                        set parent_id = $2, 
                            seq = $3
                        where id = $1",
                       [Id,ParentId,Seq],
                       Ctx)
                || {Id, ParentId, Seq} <- Cats
            ],
            renumber(Ctx),
            ok
        end,
    ok = z_db:transaction(F, Context),
    z_depcache:flush(category, Context),
    ok.


%% @doc Move the category below another category, placing it at the end of the children of that category.
%% @spec move_below(CatId::int(), NewParentId::int(), Context) -> ok | {error, Reason}
move_below(Id, ParentId, Context) ->
    case z_acl:is_allowed(update, Id, Context) of
        true ->
            PathParentId = [ParentId | get_path(ParentId, Context)],
            case lists:member(Id, PathParentId) of
                false ->
                    F = fun(Ctx) ->
                        z_db:q("update category set parent_id = $1, seq = 10000 where id = $2", [ParentId, Id], Context),
                        renumber(Ctx)
                    end,
                    z_db:transaction(F, Context),
                    z_depcache:flush(category, Context);
                true ->
                    {error, cycle}
            end;
        false ->
            {error, eacces}
    end.

%% @doc Move the category to the end of all categories, making it a top category in the process
%% @spec move_end(CatId::int(), Context) -> ok | {error, Reason}
move_end(Id, Context) ->
    case z_acl:is_allowed(update, Id, Context) of
        true ->
            F = fun(Ctx) ->
                z_db:q("update category set parent_id = null, seq = 10000 where id = $1", [Id], Context),
                renumber(Ctx)
            end,
            z_db:transaction(F, Context),
            z_depcache:flush(category, Context);
        false ->
            {error, eacces}
    end.

%% @doc Move a category in front of another category, resetting the parent of the moved category to
%% the parent of the other category.
%% @spec move_before(CatId::int(), BeforeCatId::int(), Context) -> ok | {error, Reason}
move_before(Id, BeforeId, Context) ->
    case z_acl:is_allowed(update, Id, Context) of
        true ->
            F = fun(Ctx) ->
                    {ParentId, Seq} = z_db:q_row("select parent_id, seq from category where id = $1", [BeforeId], Context),
                    PathParentId = [ParentId | get_path(ParentId, Context)],
                    case lists:member(Id, PathParentId) of
                        false ->
                            case ParentId of
                                undefined ->
                                    z_db:q("update category set seq = seq+1 where parent_id is null and seq >= $1", [Seq], Context);
                                _ -> 
                                    z_db:q("update category set seq = seq+1 where parent_id = $2 and seq >= $1", [Seq, ParentId], Context)
                            end,
                            z_db:q("update category set parent_id = $1, seq = $2 where id = $3", [ParentId, Seq, Id], Context),
                            renumber(Ctx);
                        true -> 
                            {error, cycle}
                    end
            end,

            case z_db:transaction(F, Context) of
                ok ->
                    z_depcache:flush(category, Context), 
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end;
        false ->
            {error, eacces}
    end.


update_sequence(Ids, Context) ->
    case z_acl:is_allowed(insert, category, Context) of
        true ->
            F = fun(Ctx) ->
                z_db:update_sequence(category, Ids, Ctx),
                renumber(Ctx)
            end,
            z_db:transaction(F, Context),
            z_depcache:flush(category, Context);
        false ->
            {error, eacces}
    end.


%% @doc Delete the category, move referring pages to another category. Fails when the transfer id is not a category.
%% @spec delete(Id::int(), TransferId::int(), Context) -> ok | {error, Reason}
delete(Id, TransferId, Context) ->
    % fail when deleting 'other', 'meta', 'category' or 'predicate'
    case z_db:q("select name from rsc where id = $1", [Id], Context) of
        N when  N == <<"other">>; 
                N == <<"meta">>; 
                N == <<"category">>; 
                N == <<"predicate">> ->
            {error, is_system_category};
        _ ->
            case z_acl:is_allowed(delete, Id, Context) of
                true ->
                    F = fun(Ctx) ->
                        ToId = case TransferId of
                            undefined ->
                                case z_db:q1("select parent_id from category where id = $1", [Id], Ctx) of
                                    undefined ->
                                        %% The removed category is a top-category, move all content to 'other'
                                        case z_db:q1("
                                                select c.id 
                                                from rsc r join category c on c.id = r.id
                                                where r.name = 'other'", Context) of
                                            N when is_integer(N) -> N
                                        end;
                                    N ->
                                        N
                                end;
                            N when is_integer(N) ->
                                N = z_db:q1("select id from category where id = $1", [TransferId], Ctx)
                        end,
                
                        _RscRows = z_db:q("update rsc set category_id = $1 where category_id = $2", [ToId, Id], Ctx),
                        case Id of
                            undefined ->
                                z_db:q("update category set parent_id = $1 where parent_id is null", [ToId], Ctx);
                            _ ->
                                z_db:q("update category set parent_id = $1 where parent_id = $2", [ToId, Id], Ctx)
                        end,
                        ok = m_rsc_update:delete_nocheck(Id, Ctx),
                        ok = renumber(Ctx)
                    end,
                    case z_db:transaction(F, Context) of
                        ok ->  z_depcache:flush(Context);
                        {error, Reason} -> {error, Reason}
                    end;
                false ->
                    {error, eacces}
            end
    end.


image(Id, Context) ->
    F = fun() ->
        #search_result{result=Result1} = z_search:search({media_category_image, [{cat,Id}]}, Context),
        #search_result{result=Result2} = z_search:search({media_category_depiction, [{cat,Id}]}, Context),
        Result1 ++ Result2
    end,
    Files = z_depcache:memo(F, {category_image, Id}, ?DAY, [category], Context),
    case Files of
        [] -> undefined;
        _ -> lists:nth(z_ids:number(length(Files)), Files)
    end.

    
%% @doc Return the path from a root to the category (excluding the category itself)
%% @spec get_path(Id, Context) -> [CatId]
get_path(undefined, _Context) ->
    [];
get_path(Id, Context) ->
    Cat = get(Id, Context),
    case proplists:get_value(path, Cat) of
        {ok, Path} -> Path;
        _ -> []
    end.


%% @doc Return the list of categories (as atoms) that the category is part of
%% @spec is_a(int(), Context) -> atomlist()
is_a(Id, Context) ->
    F = fun() ->
        case m_category:name_to_id(Id, Context) of
            {ok, CatId} ->
                Path = m_category:get_path(CatId, Context),
                [ z_convert:to_atom(m_category:id_to_name(C, Context)) || C <- Path ++ [CatId]];
            {error, _} ->
                []
        end
    end,
    z_depcache:memo(F, {category_is_a, Id}, ?DAY, [category], Context).


%% @doc Check if the id is within another category.
%% @spec is_a(int(), Cat, Context) -> atomlist()
is_a(Id, Cat, Context) ->
    CatName = m_category:id_to_name(Cat, Context),
    lists:member(CatName, is_a(Id, Context)).


%% @doc Given a list of category ids, return the list of numeric ranges they cover.
%% @spec ranges(CatList, Context) -> RangeList
ranges(Cat, Context) when is_atom(Cat); is_integer(Cat); is_binary(Cat) ->
    ranges([Cat], Context);
ranges(CatList0, Context) ->
    CatList = case z_string:is_string(CatList0) of
                  true -> [CatList0];
                  false -> CatList0
              end,
    F = fun(Nm, Acc) ->
        case name_to_id(Nm, Context) of
            {ok, CId} ->
                case get(CId, Context) of
                    undefined -> [{-100,-100} | Acc];
                    Props -> [{proplists:get_value(lft, Props), proplists:get_value(rght, Props)} | Acc]
                end;
            _ -> Acc
        end
    end,
    Ranges = lists:sort(lists:foldl(F, [], flatten_string(CatList, []))),
    merge_ranges(Ranges, []).

    %% Flatten the list of cats, but do not flatten strings
    flatten_string([], Acc) ->
        Acc;
    flatten_string([[A|_]=L|T], Acc) when is_list(A); is_atom(A); is_binary(A); is_tuple(A) ->
        Acc1 = flatten_string(L, Acc),
        flatten_string(T, Acc1);
    flatten_string([H|T], Acc) ->
        flatten_string(T, [H|Acc]).


    merge_ranges([], Acc) ->
        Acc;
    merge_ranges([{A,B},{C,D}|T], Acc) when C =< B+1 ->
        merge_ranges([{A,max(B,D)}|T], Acc);
    merge_ranges([H|T], Acc) ->
        merge_ranges(T, [H|Acc]).

    max(A,B) when A > B -> A;
    max(_,B) -> B.


%% @doc Return a flattened representation of the complete category tree.  Can be used for overviews or select boxes.
%% The "meta" categories of predicate, category and group are suppressed.
all_flat(Context) ->
    all_flat1(Context, false).

all_flat_meta(Context) ->
    all_flat1(Context, true).
    
all_flat1(Context, ShowMeta) ->
    F = fun() ->
        z_db:q("select c.id, c.lvl, r.name, c.props from category c join rsc r on r.id = c.id order by c.nr", Context)
    end,
    All = z_depcache:memo(F, {category_flat}, ?WEEK, [category], Context),
    All1 = case ShowMeta of
        true -> All;
        false -> lists:filter(fun is_not_meta/1, All)
    end,
    [ {Id, Lvl, string:copies("&nbsp;&nbsp;&nbsp;&nbsp;", Lvl-1), flat_title(Name, Props)} || {Id, Lvl, Name, Props} <- All1 ].
    
    flat_title(Name, Props) ->
        case proplists:get_value(title, Props) of
            undefined -> Name;
            Title -> Title
        end.

    is_not_meta({_Id, _Lvl, Name, _Props}) ->
        Name /= <<"meta">> andalso Name /= <<"predicate">> andalso Name /= <<"category">> andalso Name /= <<"acl_role">>.
    

all_flat(CatId, Context) ->
    F = fun() ->
                {L,R} = boundaries(CatId, Context),
                z_db:q("select c.id, c.lvl, r.name, c.props from category c join rsc r on r.id = c.id where $1 <= c.nr and c.nr <= $2 order by c.nr", [L, R], Context)
        end,
    All = z_depcache:memo(F, {category_flat, CatId}, ?WEEK, [category], Context),
    [ {Id, Lvl, string:copies("&nbsp;&nbsp;&nbsp;&nbsp;", Lvl-1), flat_title(Name, Props)} || {Id, Lvl, Name, Props} <- All ].


%% @doc Return the category tree as a menu resource
menu(Context) ->
    tree_to_menu(tree(Context), []).

    tree_to_menu([], Acc) ->
        lists:reverse(Acc);
    tree_to_menu([Cat|Rest], Acc) ->
        {id, Id} = proplists:lookup(id, Cat),
        {children, {ok, Cs}} = proplists:lookup(children, Cat),
        Cs1 = tree_to_menu(Cs, []),
        tree_to_menu(Rest, [{Id,Cs1}|Acc]).


%% @doc Return the tree of all categories
%% @spec tree(Context) -> Tree
tree(Context) ->
    F = fun() ->
        CatTuples = z_db:q("
                select c.id, c.parent_id, c.lvl, r.name, c.props 
                from category c join rsc r on r.id = c.id  
                order by c.nr", Context),
        build_tree(CatTuples, [])
    end,
    z_depcache:memo(F, {category_tree}, ?WEEK, [category], Context).

%% @doc Return the tree of all categories till a certain depth
%% @spec tree_depth(Depth, Context) -> Tree
tree_depth(Depth, Context) ->
    F = fun() ->
        CatTuples = z_db:q("
                select c.id, c.parent_id, c.lvl, r.name, c.props 
                from category c join rsc r on r.id = c.id 
                where c.lvl <= $1 
                order by c.nr", [Depth], Context),
        build_tree(CatTuples, [])
    end,
    z_depcache:memo(F, {category_tree_depth, Depth}, ?WEEK, [category], Context).


%% @doc Return the tree of all categories below a category id
%% @spec tree(CatId, Context) -> TreeNode
tree(CatId, Context) ->
    F = fun() ->
        CatTuples = z_db:q("
            select a.id, a.parent_id, a.lvl, r.name, a.props 
            from category a join rsc r on a.id = r.id, category p
            where p.id = $1
              and a.nr <= p.rght
              and a.nr >= p.lft
            order by a.nr", [CatId], Context),
        case build_tree(CatTuples, []) of 
            [TreeNode] -> TreeNode;
            [] -> []
        end
    end,
    z_depcache:memo(F, {category_tree_cat, CatId}, ?WEEK, [category], Context).

%% @doc Return the tree of all categories below a category id till a certain depth
%% @spec tree_depth(CatId, Depth, Context) -> TreeNode
tree_depth(CatId, Depth, Context) ->
    F = fun() ->
        CatTuples = z_db:q("
            select a.id, a.parent_id, a.lvl, r.name, a.props 
            from category a join rsc r on a.id = r.id, category p
            where p.id = $1
              and a.nr <= p.rght
              and a.nr >= p.lft
              and a.lvl <= p.lvl + $2
            order by a.nr", [CatId, Depth], Context),
        case build_tree(CatTuples, []) of 
            [TreeNode] -> TreeNode;
            [] -> []
        end
    end,
    z_depcache:memo(F, {category_tree_cat_depth, CatId, Depth}, ?WEEK, [category], Context).


build_tree([], Acc) ->
    lists:reverse(Acc);
build_tree([{_Id, _Parent, _Lvl, _Name, _Props} = C|Rest], Acc) ->
    {C1, Rest1} = build_tree(C, [], Rest),
    build_tree(Rest1, [C1|Acc]).
    
build_tree({Id, _Parent, _Lvl, _Name, _Props} = P, Acc, [{_Id2, Parent2, _Lvl2, _Name2, _Props2} = C|Rest])
    when Id == Parent2 ->
    {C1, Rest1} = build_tree(C, [], Rest),
    build_tree(P, [C1|Acc], Rest1);
build_tree({Id, Parent, Lvl, Name, Props}, Acc, Rest) ->
    Props1 = case Props of
        <<>> -> [];
        _ -> Props
    end,
    {[{id,Id}, {parent_id,Parent}, {level,Lvl}, {children, {ok, lists:reverse(Acc)}}, {name, Name} | Props1], Rest}.



%% @doc Renumber all categories so that the left/right and level indices are correct.
%% @spec renumber(Context) -> ok
renumber(Context) ->
    ok = z_db:transaction(fun renumber_transaction/1, Context),
    z_depcache:flush(category, Context),
    ok.
    

renumber_transaction(Context) ->
    CatTuples = z_db:q("select id, parent_id, seq from category order by seq,id", Context),
    Enums = enumerate(CatTuples),
    [
        z_db:q("update category
                set nr = $2,
                    seq = $2,
                    lvl = $3,
                    lft = $4,
                    rght = $5,
                    props = $6
                where id = $1", 
                [CatId, Nr, Level, Left, Right, [{path, {ok, Path}}]]
                , Context)
        || {CatId, Nr, Level, Left, Right, Path} <- Enums
    ],
    z_pivot_rsc:insert_task_after(10, ?MODULE, renumber_pivot_task, "m_category:renumber", [], Context),
    ok.

%% @doc Resync all ids that have their category nr changed.
renumber_pivot_task(Context) ->
    Nrs = z_db:q("select r.id, c.nr
                 from rsc r, category c
                 where c.id = r.category_id
                   and (r.pivot_category_nr is null or r.pivot_category_nr <> c.nr)
                 order by r.id
                 limit 500", Context),
    case Nrs of
        [] ->
            ok;
        Ids ->
            ok = z_db:transaction(fun(Ctx) ->
                    [
                        z_db:q("update rsc
                                set pivot_category_nr = $2 
                                where id = $1 
                                  and (pivot_category_nr is null or pivot_category_nr <> $2)", [Id, CatNr], Ctx)
                        || {Id,CatNr} <- Ids
                    ],
                    ok
                end, Context),
            {delay, 1}
    end.


%% @doc Take a category list and make it into a tree, recalculating the left/right and lvl nrs
%% @spec enumerate([Cat]) -> [Sort]
%%  Cat = {CatId, Parent, NodeSeq} 
%%  Sort = {CatId, Nr, Level, Left, Right, Path}
enumerate(Cats) ->
    % Fetch all the roots of our forest
    {Roots, Rest} = lists:partition(fun({_Id, Parent, _Seq}) -> Parent == undefined end, Cats),
    % Make the trees from the roots down
    Trees = [ make_tree(Root, Rest, 1, []) || Root <- Roots],
    % Flatten the trees, enumerating all nodes depth-first
    {Flatten,_Nr} = lists:foldl(fun(Tree, {Acc,Nr}) -> flatten_tree(Tree, Acc, Nr) end, {[],1}, Trees),
    Flatten.

make_tree({NodeId,_Parent,NodeSeq} = Node, Nodes, Level, Path) ->
    SubNodes = lists:filter(fun ({_,Parent,_}) -> Parent == NodeId end, Nodes),
    SubTrees = [ make_tree(SubNode, Nodes, Level+1, [NodeId|Path]) || SubNode <- SubNodes ],
    {Level, NodeSeq, Node, lists:keysort(2, SubTrees), lists:reverse(Path)}.
    
flatten_tree({Level, _NodeSeq, {NodeId,_Parent,_Seq}, SubTrees, Path}, NodesAcc, NodeNr) ->
    {NodesAcc1, NodeNr1} = lists:foldl(fun(Tree, {Acc,Nr}) -> flatten_tree(Tree, Acc, Nr) end, {NodesAcc,NodeNr+1}, SubTrees),
    {[ {NodeId, NodeNr, Level, NodeNr, NodeNr1-1, Path} | NodesAcc1], NodeNr1}.




%% Insert a category
insert(ParentId, Name, Props, Context) ->
    {ok, CatId} = name_to_id("category", Context),
    F = fun(Ctx) ->
                {ok, Id} = m_rsc_update:insert(Props ++ [{name, Name}, {category_id, CatId}], Ctx),
                case ParentId of 
                    undefined ->
                        Id;
                    _ -> move_below(Id, ParentId, Ctx),
                         Id
                end
        end,
    F(Context).
    %%z_db:transaction(F, Context).


%%
%% Return the left/right boundaries of the given category.
%% @spec boundaries(Id, C) -> {Left, Right}
boundaries(CatId, Context) ->
    F = fun() ->
                case z_db:q_row("SELECT lft, rght FROM category WHERE id = $1", [CatId], Context) of
                	{_,_} = LR -> LR;
					_ -> {-100,-100}
				end
        end,
    z_depcache:memo(F, {category_bounds, CatId}, ?WEEK, [CatId, category], Context).
