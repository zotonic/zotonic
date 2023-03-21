%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2015 Marc Worrell, Arjan Scherpenisse
%% @doc Update routines for resources.  For use by the m_rsc module.

%% Copyright 2009-2015 Marc Worrell, Arjan Scherpenisse
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

-module(m_rsc_update).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    insert/2,
    insert/3,
    delete/2,
    update/3,
    update/4,
    duplicate/3,
    merge_delete/4,

    flush/2,

    normalize_props/3,
    normalize_props/4,

    delete_nocheck/2,
    props_filter/3,

    to_slug/1,
    test/0
]).

-include_lib("zotonic.hrl").

-record(rscupd, {
    id=undefined,
    is_escape_texts=true,
    is_acl_check=true,
    is_import=false,
    is_no_touch=false,
    expected=[]
}).


%% @doc Insert a new resource. Crashes when insertion is not allowed.
-spec insert(list(), #context{}) -> {ok, integer()}.
insert(Props, Context) ->
    insert(Props, [{escape_texts, true}], Context).

-spec insert(list(), list() | boolean(), #context{}) -> {ok, integer()}.
insert(Props, Options, Context) ->
    PropsDefaults = props_defaults(Props, Context),
    update(insert_rsc, PropsDefaults, Options, Context).


%% @doc Delete a resource
-spec delete(m_rsc:resource(), #context{}) -> ok | {error, atom()}.
delete(Id, Context) when is_integer(Id), Id /= 1 ->
    case z_acl:rsc_deletable(Id, Context) of
        true ->
            case m_rsc:is_a(Id, category, Context) of
                true ->
                    m_category:delete(Id, undefined, Context);
                false ->
                    delete_nocheck(Id, Context)
            end;
        false ->
            throw({error, eacces})
    end;
delete(Name, Context) when Name /= undefined ->
    delete(m_rsc:rid(Name, Context), Context).

%% @doc Delete a resource, no check on rights etc is made. This is called by m_category:delete/3
%% @throws {error, Reason}
-spec delete_nocheck(m_rsc:resource(), #context{}) -> ok | {error, term()}.
delete_nocheck(Id, Context) ->
    delete_nocheck(m_rsc:rid(Id, Context), undefined, Context).

delete_nocheck(Id, OptFollowUpId, Context) when is_integer(Id) ->
    Referrers = m_edge:subjects(Id, Context),
    CatList = m_rsc:is_a(Id, Context),
    Props = m_rsc:get(Id, Context),

    % Outside transaction as due to race conditions we might
    % have a duplicate insert into the rsc_gone table. That
    % triggers an error which cancels the transaction F below.
    _ = m_rsc_gone:gone(Id, OptFollowUpId, Context),

    F = fun(Ctx) ->
        z_notifier:notify_sync(#rsc_delete{id=Id, is_a=CatList}, Ctx),
        z_db:delete(rsc, Id, Ctx)
    end,
    case z_db:transaction(F, Context) of
        {ok, _RowsDeleted} ->
            % Sync the caches
            [ z_depcache:flush(SubjectId, Context) || SubjectId <- Referrers ],
            flush(Id, CatList, Context),
            %% Notify all modules that the rsc has been deleted
            z_notifier:notify_sync(
                            #rsc_update_done{
                                action=delete,
                                id=Id,
                                pre_is_a=CatList,
                                post_is_a=[],
                                pre_props=Props,
                                post_props=[]
                            }, Context),
            z_edge_log_server:check(Context),
            ok;
        {error, _} = Error ->
            Error
    end.

%% @doc Merge two resources, delete the losing resource.
-spec merge_delete(m_rsc:resource(), m_rsc:resource(), list(), #context{}) -> ok | {error, term()}.
merge_delete(WinnerId, WinnerId, _Options, _Context) ->
    ok;
merge_delete(_WinnerId, 1, _Options, _Context) ->
    throw({error, eacces});
merge_delete(_WinnerId, admin, _Options, _Context) ->
    throw({error, eacces});
merge_delete(WinnerId, LoserId, Options, Context) ->
    case z_acl:rsc_deletable(LoserId, Context)
        andalso z_acl:rsc_editable(WinnerId, Context)
    of
        true ->
            case m_rsc:is_a(WinnerId, category, Context) of
                true ->
                    m_category:delete(LoserId, WinnerId, Context);
                false ->
                    merge_delete_nocheck(m_rsc:rid(WinnerId, Context), m_rsc:rid(LoserId, Context), Options, Context)
            end;
        false ->
            throw({error, eacces})
    end.

%% @doc Merge two resources, delete the 'loser'
-spec merge_delete_nocheck(integer(), integer(), list(), #context{}) -> ok.
merge_delete_nocheck(WinnerId, LoserId, Opts, Context) ->
    IsMergeTrans = proplists:get_value(is_merge_trans, Opts, false),
    z_notifier:map(#rsc_merge{
            winner_id = WinnerId,
            looser_id = LoserId,
            is_merge_trans = IsMergeTrans
        },
        Context),
    ok = m_edge:merge(WinnerId, LoserId, Context),
    m_media:merge(WinnerId, LoserId, Context),
    m_identity:merge(WinnerId, LoserId, Context),
    move_creator_modifier_ids(WinnerId, LoserId, Context),
    PropsLooser = m_rsc:get(LoserId, Context),
    ok = delete_nocheck(LoserId, WinnerId, Context),
    case merge_copy_props(WinnerId, PropsLooser, IsMergeTrans, Context) of
        [] ->
            ok;
        UpdProps ->
            {ok, _} = update(WinnerId, UpdProps, [{escape_texts, false}], Context)
    end,
    ok.

move_creator_modifier_ids(WinnerId, LoserId, Context) ->
    Ids = z_db:q("select id
                  from rsc
                  where (creator_id = $1 or modifier_id = $1)
                    and id <> $1",
                 [LoserId],
                 Context),
    z_db:q("update rsc set creator_id = $1 where creator_id = $2",
           [WinnerId, LoserId],
           Context,
           1200000),
    z_db:q("update rsc set modifier_id = $1 where modifier_id = $2",
           [WinnerId, LoserId],
           Context,
           1200000),
    lists:foreach(
            fun({Id}) ->
                flush(Id, [], Context)
            end,
            Ids).

merge_copy_props(WinnerId, Props, IsMergeTrans, Context) ->
    Props1 = ensure_merge_language(Props, Context),
    merge_copy_props_1(WinnerId, Props1, IsMergeTrans, [], Context).

merge_copy_props_1(_WinnerId, [], _IsMergeTrans, Acc, _Context) ->
    lists:reverse(Acc);
merge_copy_props_1(WinnerId, [{P,_}|Ps], IsMergeTrans, Acc, Context)
    when P =:= creator; P =:= creator_id; P =:= modifier; P =:= modifier_id;
         P =:= created; P =:= modified; P =:= version;
         P =:= id; P =:= is_published; P =:= is_protected; P =:= is_dependent;
         P =:= is_authoritative; P =:= pivot_geocode; P =:= pivot_geocode_qhash;
         P =:= category_id ->
    merge_copy_props_1(WinnerId, Ps, IsMergeTrans, Acc, Context);
merge_copy_props_1(WinnerId, [{blocks, LoserBs}|Ps], IsMergeTrans, Acc, Context) ->
    WinnerBs = m_rsc:p_no_acl(WinnerId, blocks, Context),
    NewBs = merge_copy_props_blocks(WinnerBs, LoserBs, IsMergeTrans, Context),
    merge_copy_props_1(WinnerId, Ps, IsMergeTrans, [ {blocks, NewBs} | Acc ], Context);
merge_copy_props_1(WinnerId, [{_,Empty}|Ps], IsMergeTrans, Acc, Context)
    when Empty =:= []; Empty =:= <<>>; Empty =:= undefined ->
    merge_copy_props_1(WinnerId, Ps, IsMergeTrans, Acc, Context);
merge_copy_props_1(WinnerId, [{P,LoserValue} = PV|Ps], IsMergeTrans, Acc, Context) ->
    case m_rsc:p_no_acl(WinnerId, P, Context) of
        undefined when IsMergeTrans, P =:= language, is_list(LoserValue) ->
            V1 = lists:usort([ z_trans:default_language(Context) ] ++ LoserValue),
            merge_copy_props_1(WinnerId, Ps, IsMergeTrans, [{P,V1}|Acc], Context);
        Empty when Empty =:= []; Empty =:= <<>>; Empty =:= undefined ->
            merge_copy_props_1(WinnerId, Ps, IsMergeTrans, [PV|Acc], Context);
        Value when IsMergeTrans, P =:= language, is_list(Value), is_list(LoserValue) ->
            V1 = lists:usort(Value ++ LoserValue),
            merge_copy_props_1(WinnerId, Ps, IsMergeTrans, [{P,V1}|Acc], Context);
        Value when IsMergeTrans ->
            V1 = merge_trans(Value, LoserValue, Context),
            merge_copy_props_1(WinnerId, Ps, IsMergeTrans, [{P,V1}|Acc], Context);
        _Value ->
            merge_copy_props_1(WinnerId, Ps, IsMergeTrans, Acc, Context)
    end.

ensure_merge_language(Props, Context) ->
    case proplists:get_value(language, Props) of
        undefined -> [ {language, [ z_trans:default_language(Context) ]} | Props ];
        _ -> Props
    end.

merge_trans({trans, Winner}, {trans, Loser}, _Context) ->
    Tr = lists:foldl(
        fun ({Lang,Text}, Acc) ->
            case proplists:get_value(Lang, Acc) of
                undefined -> [ {Lang,Text} | Acc ];
                _ -> Acc
            end
        end,
        Winner,
        Loser),
    {trans, Tr};
merge_trans(Winner, {trans, _} = Loser, Context) when is_binary(Winner) ->
    V1 = {trans, [ {z_trans:default_language(Context), Winner} ]},
    merge_trans(V1, Loser, Context);
merge_trans({trans, _} = Winner, Loser, Context) when is_binary(Loser) ->
    V1 = {trans, [ {z_trans:default_language(Context), Loser} ]},
    merge_trans(Winner, V1, Context);
merge_trans(Winner, _Loser, _Context) ->
    Winner.


% Merge the blocks.
% Problem is that we don't know for sure if we want to merge blocks to a superset.
% Merging might have some unintentional side effects (think of surveys, and randomly named blocks).
% So for now we only merge the translations in the like-named blocks, and only if their type is the
% same.
merge_copy_props_blocks(WinnerBs, LoserBs, _IsMergeTrans, _Context) when not is_list(LoserBs) ->  WinnerBs;
merge_copy_props_blocks(WinnerBs, LoserBs, _IsMergeTrans, _Context) when not is_list(WinnerBs) ->  LoserBs;
merge_copy_props_blocks(WinnerBs, _LoserBs, false, _Context) -> WinnerBs;
merge_copy_props_blocks(WinnerBs, LoserBs, true, Context) ->
    lists:map(
        fun(WB) ->
            case find_block( proplists:get_value(name, WB), LoserBs ) of
                undefined ->
                    WB;
                LB ->
                    WT = proplists:get_value(type,WB),
                    case proplists:get_value(type, LB) of
                        WT -> merge_block_single(WB, LB, Context);
                        _ -> WB
                    end
            end
        end,
        WinnerBs).

find_block(_Name, []) -> undefined;
find_block(Name, [ B | Bs ]) ->
    case proplists:get_value(name, B) of
        Name -> B;
        _ -> find_block(Name, Bs)
    end.

merge_block_single(W, L, Context) ->
    lists:map(
        fun({K, WV}) ->
            case proplists:get_value(K, L) of
                undefined ->
                    {K, WV};
                LV ->
                    WV1 = merge_trans(WV, LV, Context),
                    {K, WV1}
            end
        end,
        W).


%% Flush all cached entries depending on this entry, one of its subjects or its categories.
flush(Id, Context) ->
    CatList = m_rsc:is_a(Id, Context),
    flush(Id, CatList, Context).

flush(Id, CatList, Context) ->
    z_depcache:flush(m_rsc:rid(Id, Context), Context),
    [ z_depcache:flush(Cat, Context) || Cat <- CatList ],
    ok.


%% @doc Duplicate a resource, creating a new resource with the given title.
%% @throws {error, Reason}
-spec duplicate(m_rsc:resource(), list(), #context{}) -> {ok, m_rsc:resource_id()} | {error, term()}.
duplicate(Id, DupProps, Context) ->
    case z_acl:rsc_visible(Id, Context) of
        true ->
            Props = m_rsc:get_raw(Id, Context),
            FilteredProps = props_filter_protected(Props, #rscupd{id=insert_rsc, is_escape_texts=false}),
            SafeDupProps = z_sanitize:escape_props(DupProps, Context),
            InsProps = lists:foldl(
                            fun({Key, Value}, Acc) ->
                                z_utils:prop_replace(Key, Value, Acc)
                            end,
                            FilteredProps,
                            SafeDupProps ++ [
                                {name,undefined}, {uri,undefined}, {page_path,undefined},
                                {is_authoritative,true}, {is_protected,false},
                                {title_slug, undefined}, {slug, undefined}, {custom_slug, false}
                            ]),
            {ok, NewId} = insert(InsProps, false, Context),
            m_edge:duplicate(Id, NewId, Context),
            m_media:duplicate(Id, NewId, Context),
            {ok, NewId};
        false ->
            throw({error, eacces})
    end.


%% @doc Update a resource
%% @spec update(Id, Props, Context) -> {ok, Id}
%% @throws {error, Reason}
-spec update(m_rsc:resource() | insert_rsc, list(), #context{}) -> {ok, integer()} | {error, term()}.
update(Id, Props, Context) ->
    update(Id, Props, [], Context).

%% @doc Update a resource
-spec update(m_rsc:resource() | insert_rsc, list(), list() | boolean(), #context{}) -> {ok, integer()} | {error, term()}.
update(Id, Props, false, Context) ->
    update(Id, Props, [{escape_texts, false}], Context);
update(Id, Props, true, Context) ->
    update(Id, Props, [{escape_texts, true}], Context);

%% [Options]: {escape_texts, true|false (default: true}, {acl_check: true|false (default: true)}
%% {escape_texts, false} checks if the texts are escaped, and if not then it will escape. This prevents "double-escaping" of texts.
update(Id, Props, Options, Context) when is_integer(Id) orelse Id =:= insert_rsc ->
    RscUpd = #rscupd{
        id = Id,
        is_escape_texts = proplists:get_value(escape_texts, Options, true),
        is_acl_check = proplists:get_value(acl_check, Options, true),
        is_import = proplists:get_value(is_import, Options, false),
        is_no_touch = proplists:get_value(no_touch, Options, false)
                        andalso z_acl:is_admin(Context),
        expected = proplists:get_value(expected, Options, [])
    },
    update_imported_check(RscUpd, Props, Context);
update(Id, Props, Options, Context) ->
    update(m_rsc:name_to_id_check(Id, Context), Props, Options, Context).

update_imported_check(#rscupd{is_import=true, id=Id} = RscUpd, Props, Context) when is_integer(Id) ->
    case m_rsc:exists(Id, Context) of
        false ->
            {ok, CatId} = m_category:name_to_id(other, Context),
            1 = z_db:q("insert into rsc (id, creator_id, is_published, category_id)
                        values ($1, $2, false, $3)",
                       [Id, z_acl:user(Context), CatId],
                       Context);
        true ->
            ok
    end,
    update_editable_check(RscUpd, Props, Context);
update_imported_check(RscUpd, Props, Context) ->
    update_editable_check(RscUpd, Props, Context).


update_editable_check(#rscupd{id=Id, is_acl_check=true} = RscUpd, Props, Context) when is_integer(Id) ->
    case z_acl:rsc_editable(Id, Context) of
        true ->
            update_normalize_props(RscUpd, Props, Context);
        false ->
            E = case m_rsc:p(Id, is_authoritative, Context) of
                false -> {error, non_authoritative};
                true -> {error, eacces}
            end,
            throw(E)
    end;
update_editable_check(RscUpd, Props, Context) ->
    update_normalize_props(RscUpd, Props, Context).

update_normalize_props(#rscupd{id = Id, is_import = IsImport} = RscUpd, Props, Context) when is_list(Props) ->
    AtomProps = normalize_props(Id, Props, [{is_import, IsImport}], Context),
    update_transaction(RscUpd, fun(_, _, _) -> {ok, AtomProps} end, Context);
update_normalize_props(RscUpd, Func, Context) when is_function(Func) ->
    update_transaction(RscUpd, Func, Context).

update_transaction(RscUpd, Func, Context) ->
    Result = z_db:transaction(
                    fun (Ctx) ->
                        update_transaction_fun_props(RscUpd, Func, Ctx)
                    end,
                    Context),
    update_result(Result, RscUpd, Context).

update_result({ok, NewId, notchanged}, _RscUpd, _Context) ->
    {ok, NewId};
update_result({ok, NewId, OldProps, NewProps, OldCatList, IsCatInsert}, #rscupd{id=Id}, Context) ->
    % Flush some low level caches
    case proplists:get_value(name, NewProps) of
        undefined -> nop;
        Name -> z_depcache:flush({rsc_name, z_string:to_name(Name)}, Context)
    end,
    case proplists:get_value(uri, NewProps) of
        undefined -> nop;
        Uri -> z_depcache:flush({rsc_uri, z_convert:to_list(Uri)}, Context)
    end,

    % Flush category caches if a category is inserted.
    case IsCatInsert of
        true -> m_category:flush(Context);
        false -> nop
    end,

    % Flush all cached content that is depending on one of the updated categories
    z_depcache:flush(NewId, Context),
    NewCatList = m_rsc:is_a(NewId, Context),
    Cats = lists:usort(NewCatList ++ OldCatList),
    [ z_depcache:flush(Cat, Context) || Cat <- Cats ],

     % Notify that a new resource has been inserted, or that an existing one is updated
    Note = #rsc_update_done{
        action= case Id of insert_rsc -> insert; _ -> update end,
        id=NewId,
        pre_is_a=OldCatList,
        post_is_a=NewCatList,
        pre_props=OldProps,
        post_props=NewProps
    },
    z_notifier:notify_sync(Note, Context),

    % Return the updated or inserted id
    {ok, NewId};
update_result({error, {expected, _, _}} = Error, #rscupd{ id = Id }, Context) ->
    % We might have an out-of-date cached value, flush caches to force
    % an update from the database.
    z_depcache:flush_process_dict(),
    flush(Id, Context),
    lager:error("Error updating resource ~p: ~p", [ Id, Error ]),
    Error;
update_result({rollback, {_Why, _} = Er}, _RscUpd, _Context) ->
    lager:error("Rolling back update: ~p", [ Er ]),
    throw(Er);
update_result({error, _} = Error, _RscUpd, _Context) ->
    lager:error("Error updating resource: ~p", [ Error ]),
    throw(Error).


%% @doc This is running inside the rsc update db transaction
update_transaction_fun_props(#rscupd{id=Id} = RscUpd, Func, Context) ->
    Raw = get_raw_lock(Id, Context),
    case Func(Id, Raw, Context) of
        {ok, UpdateProps} ->
            EditableProps = props_filter_protected(
                                props_filter(
                                    props_trim(UpdateProps), [], Context),
                                RscUpd),
            AclCheckedProps = case z_acl:rsc_update_check(Id, EditableProps, Context) of
                                 L when is_list(L) -> L;
                                 {error, Reason} -> throw({error, Reason})
                              end,
            AutogeneratedProps = props_autogenerate(Id, AclCheckedProps, Context),
            DefaultProps = props_defaults(Id, AutogeneratedProps, Context),
            SafeProps = case RscUpd#rscupd.is_escape_texts of
                            true -> z_sanitize:escape_props(DefaultProps, Context);
                            false -> z_sanitize:escape_props_check(DefaultProps, Context)
                        end,
            ok = preflight_check(Id, SafeProps, Context),
            throw_if_category_not_allowed(Id, SafeProps, RscUpd#rscupd.is_acl_check, Context),
            update_transaction_fun_insert(RscUpd, SafeProps, Raw, UpdateProps, Context);
        {error, _} = Error ->
            {rollback, Error}
    end.

get_raw_lock(insert_rsc, _Context) -> [];
get_raw_lock(Id, Context) -> m_rsc:get_raw_lock(Id, Context).

update_transaction_fun_insert(#rscupd{id=insert_rsc} = RscUpd, Props, _Raw, UpdateProps, Context) ->
     % Allow the initial insertion props to be modified.
    CategoryId = z_convert:to_integer(proplists:get_value(category_id, Props)),
    InsProps = z_notifier:foldr(#rsc_insert{}, [{category_id, CategoryId}, {version, 0}], Context),

    % Check if the user is allowed to create the resource
    InsertId = case proplists:get_value(creator_id, UpdateProps) of
                    self ->
                        {ok, InsId} = z_db:insert(rsc, [{creator_id, undefined} | InsProps], Context),
                        1 = z_db:q("update rsc set creator_id = id where id = $1", [InsId], Context),
                        InsId;
                    CreatorId when is_integer(CreatorId) ->
                        {ok, InsId} = z_db:insert(rsc, [{creator_id, CreatorId} | InsProps], Context),
                        InsId;
                    undefined ->
                        {ok, InsId} = z_db:insert(rsc, [{creator_id, z_acl:user(Context)} | InsProps], Context),
                        InsId
                end,

    % Insert a category record for categories. Categories are so low level that we want
    % to make sure that all categories always have a category record attached.
    IsA = m_category:is_a(CategoryId, Context),
    IsCatInsert = case lists:member(category, IsA) of
                      true ->
                            m_hierarchy:append('$category', [InsertId], Context),
                            true;
                      false ->
                            false
                  end,
     % Place the inserted properties over the update properties, replacing duplicates.
    Props1 = lists:foldl(
                    fun
                        ({version, _}, Acc) -> Acc;
                        ({creator_id, _}, Acc) -> Acc;
                        ({P,_} = V, Acc) -> [ V | proplists:delete(P, Acc) ]
                    end,
                    Props,
                    InsProps),
    update_transaction_fun_db(RscUpd, InsertId, Props1, InsProps, [], IsCatInsert, Context);
update_transaction_fun_insert(#rscupd{id=Id} = RscUpd, Props, Raw, UpdateProps, Context) ->
    Props1 = proplists:delete(creator_id, Props),
    Props2 = case z_acl:is_admin(Context) of
                true ->
                    case proplists:get_value(creator_id, UpdateProps) of
                        self ->
                            [{creator_id, Id} | Props1];
                        CreatorId when is_integer(CreatorId) ->
                            [{creator_id, CreatorId} | Props1 ];
                        undefined ->
                            Props1
                    end;
                false ->
                    Props1
            end,
    IsA = m_rsc:is_a(Id, Context),
    update_transaction_fun_expected(RscUpd, Id, Props2, Raw, IsA, false, Context).

update_transaction_fun_expected(#rscupd{expected=Expected} = RscUpd, Id, Props1, Raw, IsA, false, Context) ->
    case check_expected(Raw, Expected, Context) of
        ok ->
            update_transaction_fun_db(RscUpd, Id, Props1, Raw, IsA, false, Context);
        {error, _} = Error ->
            Error
    end.


check_expected(_Raw, [], _Context) ->
    ok;
check_expected(Raw, [{Key,F}|Es], Context) when is_function(F) ->
    case F(Key, Raw, Context) of
        true -> check_expected(Raw, Es, Context);
        false -> {error, {expected, Key, proplists:get_value(Key, Raw)}}
    end;
check_expected(Raw, [{Key,Value}|Es], Context) ->
    case proplists:get_value(Key, Raw) of
        Value -> check_expected(Raw, Es, Context);
        Other -> {error, {expected, Key, Other}}
    end.


update_transaction_fun_db(RscUpd, Id, Props, Raw, IsABefore, IsCatInsert, Context) ->
    {version, Version} = proplists:lookup(version, Raw),
    UpdateProps = [ {version, Version+1} | proplists:delete(version, Props) ],
    UpdateProps1 = set_if_normal_update(RscUpd, modified, erlang:universaltime(), UpdateProps),
    UpdateProps2 = set_if_normal_update(RscUpd, modifier_id, z_acl:user(Context), UpdateProps1),
    {IsChanged, UpdatePropsN} = z_notifier:foldr(#rsc_update{
                                            action=case RscUpd#rscupd.id of
                                                        insert_rsc -> insert;
                                                        _ -> update
                                                   end,
                                            id=Id,
                                            props=Raw
                                        },
                                        {false, UpdateProps2},
                                        Context),

    % Pre-pivot of the category-id to the category sequence nr.
    UpdatePropsN1 = case proplists:get_value(category_id, UpdatePropsN) of
                        undefined ->
                            UpdatePropsN;
                        CatId ->
                            CatNr = z_db:q1("select nr
                                             from hierarchy
                                             where id = $1
                                               and name = '$category'",
                                            [CatId],
                                            Context),
                            [ {pivot_category_nr, CatNr} | UpdatePropsN]
                    end,

    case RscUpd#rscupd.id =:= insert_rsc orelse IsChanged orelse is_changed(Raw, UpdatePropsN1) of
        true ->
            UpdatePropsPrePivoted = z_pivot_rsc:pivot_resource_update(Id, UpdatePropsN1, Raw, Context),
            {ok, 1} = z_db:update(rsc, Id, UpdatePropsPrePivoted, Context),
            ok = update_page_path_log(Id, Raw, UpdatePropsN, Context),
            {ok, Id, Raw, UpdatePropsN, IsABefore, IsCatInsert};
        false ->
            {ok, Id, notchanged}
    end.

%% @doc Recombine all properties from the ones that are posted by a form.
normalize_props(Id, Props, Context) ->
    normalize_props(Id, Props, [], Context).

normalize_props(Id, Props, Options, Context) ->
    DateProps = recombine_dates(Id, Props, Context),
    TextProps = recombine_languages(DateProps, Context),
    BlockProps = recombine_blocks(TextProps, Props, Context),
    IsImport = proplists:get_value(is_import, Options, false),
    [ {map_property_name(IsImport, P), V} || {P, V} <- BlockProps ].


set_if_normal_update(#rscupd{} = RscUpd, K, V, Props) ->
    set_if_normal_update_1(
            is_normal_update(RscUpd),
            K, V, Props).

set_if_normal_update_1(false, _K, _V, Props) ->
    Props;
set_if_normal_update_1(true, K, V, Props) ->
    [ {K,V} | proplists:delete(K, Props) ].

is_normal_update(#rscupd{is_import=true}) -> false;
is_normal_update(#rscupd{is_no_touch=true}) -> false;
is_normal_update(#rscupd{}) -> true.


%% @doc Check if the update will change the data in the database
%% @spec is_changed(Current, Props) -> bool()
is_changed(Current, Props) ->
    is_prop_changed(Props, Current).

is_prop_changed([], _Current) ->
    false;
is_prop_changed([{version, _}|Rest], Current) ->
    is_prop_changed(Rest, Current);
is_prop_changed([{modifier_id, _}|Rest], Current) ->
    is_prop_changed(Rest, Current);
is_prop_changed([{modified, _}|Rest], Current) ->
    is_prop_changed(Rest, Current);
is_prop_changed([{pivot_category_nr, _}|Rest], Current) ->
    is_prop_changed(Rest, Current);
is_prop_changed([{Prop, Value}|Rest], Current) ->
    case is_equal(Value, proplists:get_value(Prop, Current)) of
        true -> is_prop_changed(Rest, Current);
        false -> true  % The property Prop has been changed.
    end.

is_equal(A, A) -> true;
is_equal(_, undefined) -> false;
is_equal(undefined, _) -> false;
is_equal(A,B) -> z_utils:are_equal(A, B).


%% @doc Check if all props are acceptable. Examples are unique name, uri etc.
%% @spec preflight_check(Id, Props, Context) -> ok | {error, Reason}
preflight_check(insert_rsc, Props, Context) ->
    preflight_check(-1, Props, Context);
preflight_check(_Id, [], _Context) ->
    ok;
preflight_check(Id, [{name, Name}|T], Context) when Name =/= undefined ->
    case z_db:q1("select count(*) from rsc where name = $1 and id <> $2", [Name, Id], Context) of
        0 ->
            preflight_check(Id, T, Context);
        _N ->
            lager:warning("Trying to insert duplicate name ~p",
                          [Name]),
            throw({error, duplicate_name})
    end;
preflight_check(Id, [{page_path, Path}|T], Context) when Path =/= undefined ->
    case z_db:q1("select count(*) from rsc where page_path = $1 and id <> $2", [Path, Id], Context) of
        0 ->
            preflight_check(Id, T, Context);
        _N ->
            lager:warning("Trying to insert duplicate page_path ~p", [Path]),
            throw({error, duplicate_page_path})
    end;
preflight_check(Id, [{uri, Uri}|T], Context) when Uri =/= undefined ->
    case z_db:q1("select count(*) from rsc where uri = $1 and id <> $2", [Uri, Id], Context) of
        0 ->
            preflight_check(Id, T, Context);
        _N ->
            lager:warning("Trying to insert duplicate uri ~p", [Uri]),
            throw({error, duplicate_uri})
    end;
preflight_check(Id, [{'query', Query}|T], Context) ->
    Valid = case m_rsc:is_a(Id, 'query', Context) of
                true ->
                    try
                        SearchContext = z_context:new( Context ),
                        search_query:search(search_query:parse_query_text(z_html:unescape(Query)), SearchContext),
                        true
                    catch
                        _: {error, {_, _}} ->
                            false
                    end;
                false -> true
            end,
    case Valid of
        true -> preflight_check(Id, T, Context);
        false -> throw({error, invalid_query})
    end;
preflight_check(Id, [_H|T], Context) ->
    preflight_check(Id, T, Context).


throw_if_category_not_allowed(_Id, _SafeProps, false, _Context) ->
    ok;
throw_if_category_not_allowed(insert_rsc, SafeProps, _True, Context) ->
    case proplists:get_value(category_id, SafeProps) of
        undefined ->
            throw({error, nocategory});
        CatId ->
            throw_if_category_not_allowed_1(undefined, SafeProps, CatId, Context)
    end;
throw_if_category_not_allowed(Id, SafeProps, _True, Context) ->
    case proplists:get_value(category_id, SafeProps) of
        undefined ->
            ok;
        CatId ->
            PrevCatId = z_db:q1("select category_id from rsc where id = $1", [Id], Context),
            throw_if_category_not_allowed_1(PrevCatId, SafeProps, CatId, Context)
    end.

throw_if_category_not_allowed_1(CatId, _SafeProps, CatId, _Context) ->
    ok;
throw_if_category_not_allowed_1(_PrevCatId, SafeProps, CatId, Context) ->
    CategoryName = m_category:id_to_name(CatId, Context),
    case z_acl:is_allowed(insert, #acl_rsc{category = CategoryName, props = SafeProps}, Context) of
        true -> ok;
        _False -> throw({error, eacces})
    end.


%% @doc Remove whitespace around some predefined fields
props_trim(Props) ->
    [
        case is_trimmable(P,V) of
            true -> {P, z_string:trim(V)};
            false -> {P,V}
        end
        || {P,V} <- Props
    ].


%% @doc Remove properties the user is not allowed to change and convert some other to the correct data type
%% @spec props_filter(Props1, Acc, Context) -> Props2
props_filter([], Acc, _Context) ->
    Acc;

props_filter([{uri, Uri}|T], Acc, Context) ->
    case Uri of
        Empty when Empty == undefined; Empty == []; Empty == <<>> ->
            props_filter(T, [{uri, undefined} | Acc], Context);
        _ ->
            props_filter(T, [{uri, z_sanitize:uri(Uri)} | Acc], Context)
    end;

props_filter([{name, Name}|T], Acc, Context) ->
    case z_acl:is_allowed(use, mod_admin, Context) of
        true ->
            case z_utils:is_empty(Name) of
                true ->
                    props_filter(T, [{name, undefined} | Acc], Context);
                false ->
                    Name1 = case z_string:to_name(Name) of
                        <<"_">> -> undefined;
                        N -> N
                    end,
                    props_filter(T, [{name, Name1} | Acc], Context)
            end;
        false ->
            props_filter(T, Acc, Context)
    end;

props_filter([{page_path, Path}|T], Acc, Context) ->
    case z_acl:is_allowed(use, mod_admin, Context) of
        true ->
            case Path of
                Empty when Empty == undefined; Empty == []; Empty == <<>> ->
                    props_filter(T, [{page_path, undefined} | Acc], Context);
                _ ->
                    P = [ $/ | string:strip(z_utils:url_path_encode(Path), both, $/) ],
                    props_filter(T, [{page_path, P} | Acc], Context)
            end;
        false ->
            props_filter(T, Acc, Context)
    end;
props_filter([{title_slug, Slug}|T], Acc, Context) ->
    case z_utils:is_empty(Slug) of
        true ->
            props_filter(T, Acc, Context);
        false ->
            Slug1 = to_slug(Slug),
            SlugNoTr = z_trans:lookup_fallback(Slug1, en, Context),
            Acc1 = proplists:delete(slug, Acc),
            T1 = proplists:delete(slug, T),
            props_filter(T1, [ {title_slug, Slug1}, {slug, SlugNoTr} | Acc1 ], Context)
    end;

props_filter([{B, P}|T], Acc, Context)
    when  B =:= is_published; B =:= is_featured; B=:= is_protected;
          B =:= is_dependent; B =:= is_query_live; B =:= date_is_all_day;
          B =:= is_website_redirect; B =:= is_page_path_multiple;
          B =:= is_authoritative;
          B =:= custom_slug; B =:= seo_noindex ->
    props_filter(T, [{B, z_convert:to_bool(P)} | Acc], Context);

props_filter([{P, DT}|T], Acc, Context)
    when P =:= created; P =:= modified;
         P =:= date_start; P =:= date_end;
         P =:= publication_start; P =:= publication_end  ->
    props_filter(T, [{P,z_datetime:to_datetime(DT)}|Acc], Context);

props_filter([{P, Id}|T], Acc, Context)
    when P =:= creator_id; P =:= modifier_id ->
    case m_rsc:rid(Id, Context) of
        undefined ->
            props_filter(T, Acc, Context);
        RId ->
            props_filter(T, [{P,RId}|Acc], Context)
    end;

props_filter([{visible_for, Vis}|T], Acc, Context) ->
    VisibleFor = z_convert:to_integer(Vis),
    case VisibleFor of
        N when N >= 0 ->
            props_filter(T, [{visible_for, N} | Acc], Context);
        _ ->
            props_filter(T, Acc, Context)
    end;

props_filter([{category, CatName}|T], Acc, Context) ->
    props_filter([{category_id, m_category:name_to_id_check(CatName, Context)} | T], Acc, Context);
props_filter([{category_id, CatId}|T], Acc, Context) ->
    CatId1 = m_rsc:rid(CatId, Context),
    case m_rsc:is_a(CatId1, category, Context) of
        true ->
            props_filter(T, [{category_id, CatId1}|Acc], Context);
        false ->
            lager:error("Ignoring unknown category '~p' in update, using 'other' instead.",
                        [CatId]),
            props_filter(T, [{category_id,m_rsc:rid(other, Context)}|Acc], Context)
    end;

props_filter([{content_group, undefined}|T], Acc, Context) ->
    props_filter(T, [{content_group_id, undefined}|Acc], Context);
props_filter([{content_group, CgName}|T], Acc, Context) ->
    case m_rsc:rid(CgName, Context) of
        undefined ->
            lager:error("Ignoring unknown content group '~p' in update.", [CgName]),
            props_filter(T, Acc, Context);
        CgId ->
            props_filter([{content_group_id, CgId}|T], Acc, Context)
    end;
props_filter([{content_group_id, undefined}|T], Acc, Context) ->
    props_filter(T, [{content_group_id, undefined}|Acc], Context);
props_filter([{content_group_id, CgId}|T], Acc, Context) ->
    CgId1 = m_rsc:rid(CgId, Context),
    case m_rsc:is_a(CgId1, content_group, Context)
        orelse m_rsc:is_a(CgId1, acl_collaboration_group, Context)
    of
        true ->
            props_filter(T, [{content_group_id, CgId1}|Acc], Context);
        false ->
            lager:error("Ignoring unknown content group '~p' in update.", [CgId]),
            props_filter(T, Acc, Context)
    end;

props_filter([{Location, P}|T], Acc, Context)
    when Location =:= location_lat; Location =:= location_lng ->
    X = try
            z_convert:to_float(P)
        catch
            _:_ -> undefined
        end,
    props_filter(T, [{Location, X} | Acc], Context);

props_filter([{pref_language, Lang}|T], Acc, Context) ->
    Lang1 = case z_trans:to_language_atom(Lang) of
                {ok, LangAtom} -> LangAtom;
                {error, not_a_language} -> undefined
            end,
    props_filter(T, [{pref_language, Lang1} | Acc], Context);

props_filter([{language, Langs}|T], Acc, Context) ->
    props_filter(T, [{language, filter_languages(Langs)}|Acc], Context);

props_filter([{crop_center, undefined}=H|T], Acc, Context) ->
    props_filter(T, [H|Acc], Context);
props_filter([{crop_center, CropCenter}|T], Acc, Context) ->
    CropCenter1 = case z_string:trim(CropCenter) of
        <<>> -> undefined;
        Trimmed ->
            case re:run(Trimmed, "^\\+[0-9]+\\+[0-9]+$") of
                nomatch -> undefined;
                {match, _} -> Trimmed
            end
    end,
    props_filter(T, [{crop_center, CropCenter1}|Acc], Context);

props_filter([{_Prop, _V}=H|T], Acc, Context) ->
    props_filter(T, [H|Acc], Context).


%% Filter all given languages, drop unknown languages.
%% Ensure that the languages are a list of atoms.
filter_languages([]) -> [];
filter_languages(<<>>) -> [];
filter_languages(Lang) when is_binary(Lang); is_atom(Lang) ->
    filter_languages([Lang]);
filter_languages([C|_] = Lang) when is_integer(C) ->
    filter_languages([Lang]);
filter_languages([L|_] = Langs) when is_list(L); is_binary(L); is_atom(L) ->
    lists:foldr(
            fun(Lang, Acc) ->
                case z_trans:to_language_atom(Lang) of
                    {ok, LangAtom} -> [LangAtom|Acc];
                    {error, not_a_language} -> Acc
                end
            end,
            [],
            Langs).

%% @doc Automatically modify some props on update.
%% @spec props_autogenerate(Id, Props1, Context) -> Props2
props_autogenerate(Id, Props, Context) ->
    %% When title is updating, check the rsc 'custom_slug' field to see if we need to update the slug or not.
    Props1 = case proplists:get_value(title, Props) of
                 undefined -> Props;
                 Title ->
                    case {proplists:get_value(custom_slug, Props), m_rsc:p(Id, custom_slug, Context)} of
                         {true, _} -> Props;
                         {_, true} -> Props;
                         _X ->
                            %% Determine the slug from the title.
                            Slug = to_slug(Title),
                            SlugNoTr = z_trans:lookup_fallback(Slug, en, Context),
                            PropsSlug = proplists:delete(slug, proplists:delete(title_slug, Props)),
                            [ {title_slug, Slug}, {slug, SlugNoTr} | PropsSlug ]
                    end
            end,
    Props1.


%% @doc Fill in some defaults for empty props on insert.
%% @spec props_defaults(Props1, Context) -> Props2
props_defaults(Props, Context) ->
    % Generate slug from the title (when there is a title)
    Props1 = case proplists:get_value(title_slug, Props) of
        undefined ->
            case proplists:get_value(title, Props) of
                undefined ->
                    Props;
                Title ->
                    Slug = to_slug(Title),
                    SlugNoTr = z_trans:lookup_fallback(Slug, en, Context),
                    PropsSlug = lists:keystore(slug, 1, Props, {slug, SlugNoTr}),
                    lists:keystore(title_slug, 1, PropsSlug, {title_slug, Slug})
            end;
        _ ->
            Props
    end,
    % Assume content is authoritative, unless stated otherwise
    case proplists:get_value(is_authoritative, Props1) of
        undefined -> [{is_authoritative, true}|Props1];
        _ -> Props
    end.

%% @doc Set default properties on resource insert and update.
-spec props_defaults(m_rsc:resource(), m_rsc:properties(), z:context()) -> m_rsc:properties().
props_defaults(_Id, Props, Context) ->
    lists:foldl(
        fun(Key, Acc) ->
            prop_default(Key, proplists:get_value(Key, Props), Acc, Context)
        end,
        Props,
        [publication_start]
    ).

-spec prop_default(atom(), any(), m_rsc:properties(), z:context()) -> m_rsc:properties().
prop_default(publication_start, undefined, Props, _Context) ->
    case proplists:get_value(is_published, Props) of
        true ->
            z_utils:prop_replace(publication_start, erlang:universaltime(), Props);
        _ ->
            Props
    end;
prop_default(_Key, _Value, Props, _Context) ->
    Props.

props_filter_protected(Props, RscUpd) ->
    IsNormal = is_normal_update(RscUpd),
    lists:filter(fun
                    ({K, _}) -> not is_protected(K, IsNormal)
                 end,
                 Props).


to_slug(undefined) ->
    undefined;
to_slug({trans, Tr}) ->
    Tr1 = lists:map(
        fun({Lang, V}) -> {Lang, to_slug(V)} end,
        Tr),
    {trans, Tr1};
to_slug(B) when is_binary(B) ->
    B1 = z_string:to_lower( z_html:unescape(B) ),
    truncate_slug( slugify(B1, false, <<>>) );
to_slug(X) ->
    to_slug( z_convert:to_binary(X) ).

truncate_slug(Slug) ->
    z_string:truncate(Slug, 70, <<>>).

slugify(<<>>, _Last, Acc) ->
    Acc;
slugify(<<C/utf8, T/binary>>, $-, Acc) ->
    case is_slugchar(C) of
        false -> slugify(T, $-, Acc);
        true when Acc =:= <<>> -> slugify(T, false, <<C/utf8>>);
        true -> slugify(T, false, <<Acc/binary, $-, C/utf8>>)
    end;
slugify(<<C/utf8, T/binary>>, false, Acc) ->
    case is_slugchar(C) of
        false -> slugify(T, $-, Acc);
        true -> slugify(T, false, <<Acc/binary, C/utf8>>)
    end.

is_slugchar(C) when C =< 32 -> false;
is_slugchar(254) -> false;
is_slugchar(255) -> false;
is_slugchar(C) when C > 128 -> true;
is_slugchar(C) -> z_url:url_unreserved_char(C).


%% @doc Map property names to an atom, fold pivot and computed fields together for later filtering.
map_property_name(IsImport, P) when not is_list(P) -> map_property_name(IsImport, z_convert:to_list(P));
map_property_name(_IsImport, "computed_"++_) -> computed_xxx;
map_property_name(_IsImport, "pivot_"++_) -> pivot_xxx;
map_property_name(false, P) when is_list(P) -> erlang:list_to_existing_atom(P);
map_property_name(true,  P) when is_list(P) -> erlang:list_to_atom(P).


%% @doc Properties that can't be updated with m_rsc_update:update/3 or m_rsc_update:insert/2
is_protected(id, _IsNormal)            -> true;
is_protected(created, true)            -> true;
is_protected(creator_id, true)         -> true;
is_protected(modified, true)           -> true;
is_protected(modifier_id, true)        -> true;
is_protected(props, _IsNormal)         -> true;
is_protected(version, _IsNormal)       -> true;
is_protected(page_url, _IsNormal)      -> true;
is_protected(medium, _IsNormal)        -> true;
is_protected(pivot_xxx, _IsNormal)     -> true;
is_protected(computed_xxx, _IsNormal)  -> true;
is_protected(_, _IsNormal)             -> false.


is_trimmable(_, V) when not is_binary(V), not is_list(V) -> false;
is_trimmable(title, _)       -> true;
is_trimmable(title_short, _) -> true;
is_trimmable(summary, _)     -> true;
is_trimmable(chapeau, _)     -> true;
is_trimmable(subtitle, _)    -> true;
is_trimmable(email, _)       -> true;
is_trimmable(uri, _)         -> true;
is_trimmable(website, _)     -> true;
is_trimmable(page_path, _)   -> true;
is_trimmable(name, _)        -> true;
is_trimmable(slug, _)        -> true;
is_trimmable(category, _)    -> true;
is_trimmable(rsc_id, _)      -> true;
is_trimmable(_, _)           -> false.


%% @doc Combine all textual date fields into real date. Convert them to UTC afterwards.
recombine_dates(Id, Props, Context) ->
    LocalNow = local_now(Context),
    {Dates, Props1} = recombine_dates_1(Props, [], []),
    {Dates1, DateGroups} = group_dates(Dates),
    {DateGroups1, DatesNull} = collect_empty_date_groups(DateGroups, [], []),
    {Dates2, DatesNull1} = collect_empty_dates(Dates1, [], DatesNull),
    Dates3 = [
        {Name, date_from_default(default_date(Name, LocalNow), D)}
        || {Name, D} <- Dates2
    ],
    DateGroups2 = [
        {Name, dategroup_fill_parts(
                    Name,
                    date_from_default(default_date(Name, LocalNow), S),
                    E)}
        || {Name, {S,E}} <- DateGroups1
    ],
    Dates4 = lists:foldl(
                    fun({Name, {S, E}}, Acc) ->
                        [
                            {Name++"_start", S},
                            {Name++"_end", E}
                            | Acc
                        ]
                    end,
                    Dates3,
                    DateGroups2),
    DatesUTC = maybe_dates_to_utc(Id, Dates4, Props, Context),
    [
        {tz, z_context:tz(Context)}
        | DatesUTC ++ DatesNull1 ++ Props1
    ].

maybe_dates_to_utc(Id, Dates, Props, Context) ->
    IsAllDay = is_all_day(Id, Props, Context),
    [ maybe_to_utc(IsAllDay, NameDT,Context) || NameDT <- Dates ].

maybe_to_utc(true, {"date_start", _Date} = D, _Context) ->
    D;
maybe_to_utc(true, {"date_end", _Date} = D, _Context) ->
    D;
maybe_to_utc(_IsAllDay, {Name, Date}, Context) ->
    {Name, z_datetime:to_utc(Date, Context)}.

is_all_day(Id, Props, Context) ->
    case proplists:get_value(date_is_all_day, Props) of
        undefined ->
            case proplists:get_value("date_is_all_day", Props) of
                undefined ->
                    case is_integer(Id) of
                        false ->
                            false;
                        true  ->
                            z_convert:to_bool(m_rsc:p_no_acl(Id, date_is_all_day, Context))
                    end;
                IsAllDay ->
                    z_convert:to_bool(IsAllDay)
            end;
        IsAllDay ->
            z_convert:to_bool(IsAllDay)
    end.


collect_empty_date_groups([], Acc, Null) ->
    {Acc, Null};
collect_empty_date_groups([{"publication", _} = R|T], Acc, Null) ->
    collect_empty_date_groups(T, [R|Acc], Null);
collect_empty_date_groups([{Name, {
                            {{undefined, undefined, undefined}, {undefined, undefined, undefined}},
                            {{undefined, undefined, undefined}, {undefined, undefined, undefined}}
                            }}|T], Acc, Null) ->
    collect_empty_date_groups(T, Acc, [{Name++"_start", undefined}, {Name++"_end", undefined} | Null]);
collect_empty_date_groups([H|T], Acc, Null) ->
    collect_empty_date_groups(T, [H|Acc], Null).



collect_empty_dates([], Acc, Null) ->
    {Acc, Null};
collect_empty_dates([{Name, {{undefined, undefined, undefined}, {undefined, undefined, undefined}}}|T], Acc, Null) ->
    collect_empty_dates(T, Acc, [{Name, undefined}|Null]);
collect_empty_dates([H|T], Acc, Null) ->
    collect_empty_dates(T, [H|Acc], Null).



recombine_dates_1([], Dates, Acc) ->
    {Dates, Acc};
recombine_dates_1([{"dt:"++K,V}|T], Dates, Acc) ->
    [Part, End, Name] = string:tokens(K, ":"),
    Dates1 = recombine_date(Part, End, Name, V, Dates),
    recombine_dates_1(T, Dates1, Acc);
recombine_dates_1([H|T], Dates, Acc) ->
    recombine_dates_1(T, Dates, [H|Acc]).

    recombine_date(Part, End, Name, undefined, Dates) ->
        recombine_date(Part, End, Name, "", Dates);
    recombine_date(Part, _End, Name, V, Dates) ->
        Date = case proplists:get_value(Name, Dates) of
            undefined ->
                {{undefined, undefined, undefined}, {undefined, undefined, undefined}};
            D ->
                D
        end,
        Date1 = recombine_date_part(Date, Part, to_date_value(Part, string:strip(V))),
        lists:keystore(Name, 1, Dates, {Name, Date1}).

    recombine_date_part({{_Y,M,D},{H,I,S}}, "y", V) -> {{V,M,D},{H,I,S}};
    recombine_date_part({{Y,_M,D},{H,I,S}}, "m", V) -> {{Y,V,D},{H,I,S}};
    recombine_date_part({{Y,M,_D},{H,I,S}}, "d", V) -> {{Y,M,V},{H,I,S}};
    recombine_date_part({{Y,M,D},{_H,I,S}}, "h", V) -> {{Y,M,D},{V,I,S}};
    recombine_date_part({{Y,M,D},{H,_I,S}}, "i", V) -> {{Y,M,D},{H,V,S}};
    recombine_date_part({{Y,M,D},{H,I,_S}}, "s", V) -> {{Y,M,D},{H,I,V}};
    recombine_date_part({{Y,M,D},{_H,_I,S}}, "hi", {H,I,_S}) -> {{Y,M,D},{H,I,S}};
    recombine_date_part({{Y,M,D},_Time}, "his", {_,_,_} = V) -> {{Y,M,D},V};
    recombine_date_part({_Date,{H,I,S}}, "ymd", {_,_,_} = V) -> {V,{H,I,S}};
    recombine_date_part({_Date,{H,I,S}}, "dmy", {_,_,_} = V) -> {V,{H,I,S}}.

    to_date_value("ymd", "-" ++ V) ->
        case to_date_value("ymd", V) of
            {Y, M, D} when is_integer(Y) -> {-Y, M, D};
            YMD -> YMD
        end;
    to_date_value("dmy", V) ->
        case re:run(V, "([0-9]+)[-/: ]([0-9]+)[-/: ](-?[0-9]+)", [{capture, all_but_first, binary}]) of
            nomatch -> {undefined, undefined, undefined};
            % Negative years 13/7/-99
            {match, [D, M, Y]} -> {to_int(Y), to_int(M), to_int(D)}
        end;
    to_date_value(Part, V) when Part =:= "ymd"; Part =:= "his"->
        case string:tokens(V, "-/: ") of
            [] -> {undefined, undefined, undefined};
            [Y,M,D] -> {to_int(Y), to_int(M), to_int(D)}
        end;
    to_date_value("hi", V) ->
        case string:tokens(V, "-/: ") of
            [] -> {undefined, undefined, undefined};
            [H] -> {to_int(H), 0, undefined};
            [H,I] -> {to_int(H), to_int(I), undefined}
        end;
    to_date_value(_, V) ->
        to_int(V).

group_dates(Dates) ->
    group_dates(Dates, [], []).

    group_dates([], Groups, Acc) ->
        {Acc, Groups};
    group_dates([{Name,D}|T], Groups, Acc) ->
        case lists:suffix("_start", Name) of
            true ->
                Base = lists:sublist(Name, length(Name) - 6),
                Range = case proplists:get_value(Base, Groups) of
                    {_Start, End} ->
                        { D, End };
                    undefined ->
                        { D, {{undefined, undefined, undefined}, {undefined, undefined, undefined}} }
                end,
                Groups1 = lists:keystore(Base, 1, Groups, {Base, Range}),
                group_dates(T, Groups1, Acc);

            false ->
                case lists:suffix("_end", Name) of
                    true ->
                        Base = lists:sublist(Name, length(Name) - 4),
                        Range = case proplists:get_value(Base, Groups) of
                            {Start, _End} ->
                                { Start, D };
                            undefined ->
                                { {{undefined, undefined, undefined}, {0, 0, 0}}, D }
                        end,
                        Groups1 = lists:keystore(Base, 1, Groups, {Base, Range}),
                        group_dates(T, Groups1, Acc);

                    false ->
                        group_dates(T, Groups, [{Name,D}|Acc])
                end
        end.

default_date("date", _LocalNow) -> undefined;
default_date("date_start", _LocalNow) -> undefined;
default_date("date_end", _LocalNow) -> undefined;
default_date("publication", _LocalNow) -> undefined;
default_date("org_pubdate", _LocalNow) -> undefined;
default_date(_, LocalNow) -> LocalNow.

dategroup_fill_parts( "date", S, {{undefined,undefined,undefined},{undefined,undefined,undefined}} ) ->
    {S, undefined};
dategroup_fill_parts( _Name, S, {{undefined,undefined,undefined},{undefined,undefined,undefined}} ) ->
    {S, ?ST_JUTTEMIS};
dategroup_fill_parts( Name, {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{undefined,Me,De},{He,Ie,Se}} ) when is_integer(Ys) ->
    dategroup_fill_parts( Name, {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ys,Me,De},{He,Ie,Se}} );
dategroup_fill_parts( Name, {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ys,undefined,De},{He,Ie,Se}} ) when is_integer(Ms) ->
    dategroup_fill_parts( Name, {{Ys,Ms,Ds},{Hs,Is,Ss}} ,{{Ys,Ms,De},{He,Ie,Se}} );
dategroup_fill_parts( Name, {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ys,Ms,undefined},{He,Ie,Se}} ) when is_integer(Ds) ->
    dategroup_fill_parts( Name, {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ys,Ms,Ds},{He,Ie,Se}} );
dategroup_fill_parts( Name, S, {{undefined,Me,De},{He,Ie,Se}} ) ->
    dategroup_fill_parts( Name, S, {{9999,Me,De},{He,Ie,Se}} );
dategroup_fill_parts( Name, S, {{Ye,undefined,De},{He,Ie,Se}} ) ->
    dategroup_fill_parts( Name, S ,{{Ye,12,De},{He,Ie,Se}} );
dategroup_fill_parts( Name, S, {{Ye,Me,undefined},{He,Ie,Se}} ) ->
    De = z_datetime:last_day_of_the_month(Ye,Me),
    dategroup_fill_parts( Name, S, {{Ye,Me,De},{He,Ie,Se}} );
dategroup_fill_parts( Name, S, {{Ye,Me,De},{undefined,Ie,Se}} ) ->
    dategroup_fill_parts( Name, S, {{Ye,Me,De},{23,Ie,Se}} );
dategroup_fill_parts( Name, S, {{Ye,Me,De},{He,undefined,Se}} ) ->
    dategroup_fill_parts( Name, S, {{Ye,Me,De},{He,59,Se}} );
dategroup_fill_parts( Name, S, {{Ye,Me,De},{He,Ie,undefined}} ) ->
    dategroup_fill_parts( Name, S, {{Ye,Me,De},{He,Ie,59}} );
dategroup_fill_parts( _Name, S, E ) ->
    {S, E}.


date_from_default(S, {{undefined, undefined, undefined}, {undefined, undefined, undefined}}) ->
    S;
date_from_default(S, {{undefined, undefined, undefined}, {0, 0, 0}}) ->
    S;
date_from_default( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{undefined,Me,De},{He,Ie,Se}} ) when is_integer(Ys) ->
    date_from_default( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ys,Me,De},{He,Ie,Se}} );
date_from_default( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,undefined,De},{He,Ie,Se}} ) when is_integer(Ms) ->
    date_from_default( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Ms,De},{He,Ie,Se}} );
date_from_default( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,undefined},{He,Ie,Se}} ) when is_integer(Ds) ->
    date_from_default( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,Ds},{He,Ie,Se}} );
date_from_default( S, {{undefined,Me,De},{He,Ie,Se}} ) ->
    date_from_default( S, {{-4300,Me,De},{He,Ie,Se}} );
date_from_default( S, {{Ye,undefined,De},{He,Ie,Se}} ) ->
    date_from_default( S, {{Ye,1,De},{He,Ie,Se}} );
date_from_default( S, {{Ye,Me,undefined},{He,Ie,Se}} ) ->
    date_from_default( S, {{Ye,Me,1},{He,Ie,Se}} );
date_from_default( S, {{Ye,Me,De},{undefined,Ie,Se}} ) ->
    date_from_default( S, {{Ye,Me,De},{0,Ie,Se}} );
date_from_default( S, {{Ye,Me,De},{He,undefined,Se}} ) ->
    date_from_default( S, {{Ye,Me,De},{He,0,Se}} );
date_from_default( S, {{Ye,Me,De},{He,Ie,undefined}} ) ->
    date_from_default( S, {{Ye,Me,De},{He,Ie,0}} );
date_from_default( _S, {{Ye,Me,De},{He,Ie,Se}} ) ->
    {{Ye,Me,De},{He,Ie,Se}}.

to_int("") ->
    undefined;
to_int(<<>>) ->
    undefined;
to_int(A) when is_list(A) ->
    try
        list_to_integer(A)
    catch
        _:_ -> undefined
    end;
to_int(A) when is_binary(A) ->
    try
        binary_to_integer(A)
    catch
        _:_ -> undefined
    end.

% to_datetime(undefined) ->
%     erlang:universaltime();
% to_datetime(B) ->
%     case z_datetime:to_datetime(B) of
%         undefined -> erlang:universaltime();
%         DT -> DT
%     end.

%% @doc get all languages encoded in proplists' keys.
%% e.g. m_rsc_update:props_languages([{"foo$en", x}, {"bar$nl", x}]) -> ["en", "nl"]
props_languages(Props) ->
    lists:foldr(fun({Key, _}, Acc) ->
                        case string:tokens(z_convert:to_list(Key), [$$]) of
                            [_, Lang] ->
                                case lists:member(Lang, Acc) of
                                    true -> Acc;
                                    false -> [Lang|Acc]
                                end;
                            _ -> Acc
                        end
                end, [], Props).


%% @doc Combine language versions of texts. Assume we edit all texts or none.
recombine_languages(Props, Context) ->
    case props_languages(Props) of
        [] ->
            Props;
        L ->
            Cfg = [ atom_to_list(Code) || Code <- config_langs(Context) ],
            L1 = filter_langs(edited_languages(Props, L), Cfg),
            {LangProps, OtherProps} = comb_lang(Props, L1, [], []),
            LangProps ++ [{language, [list_to_atom(Lang) || Lang <- L1]}|proplists:delete("language", OtherProps)]
    end.

    %% @doc Fetch all the edited languages, from 'language' inputs or a merged 'language' property
    edited_languages(Props, PropLangs) ->
        case proplists:is_defined("language", Props) of
            true ->
                proplists:get_all_values("language", Props);
            false ->
                case proplists:get_value(language, Props) of
                    L when is_list(L) ->
                        [ z_convert:to_list(Lang) || Lang <- L ];
                    undefined ->
                        PropLangs
                end
        end.

    comb_lang([], _L1, LAcc, OAcc) ->
        {LAcc, OAcc};
    comb_lang([{P,V}|Ps], L1, LAcc, OAcc) when is_list(P) ->
        case string:tokens(P, "$") of
            [P1,Lang] ->
                case lists:member(Lang, L1) of
                    true -> comb_lang(Ps, L1, append_langprop(P1, Lang, V, LAcc), OAcc);
                    false -> comb_lang(Ps, L1, LAcc, OAcc)
                end;
            _ ->
                comb_lang(Ps, L1, LAcc, [{P,V}|OAcc])
        end;
    comb_lang([PV|Ps], L1, LAcc, OAcc) ->
        comb_lang(Ps, L1, LAcc, [PV|OAcc]).


    append_langprop(P, Lang, V, Acc) ->
        Lang1 = list_to_atom(Lang),
        case proplists:get_value(P, Acc) of
            {trans, Tr} ->
                Tr1 = [{Lang1, z_convert:to_binary(V)}|Tr],
                [{P, {trans, Tr1}} | proplists:delete(P, Acc)];
            undefined ->
                [{P, {trans, [{Lang1,z_convert:to_binary(V)}]}}|Acc]
        end.

recombine_blocks(Props, OrgProps, Context) ->
    Props1 = recombine_blocks_form(Props, OrgProps, Context),
    recombine_blocks_import(Props1, OrgProps, Context).

recombine_blocks_form(Props, OrgProps, Context) ->
    {BPs, Ps} = lists:partition(fun({"block-"++ _, _}) -> true; (_) -> false end, Props),
    case BPs of
        [] ->
            case proplists:get_value(blocks, Props) of
                Blocks when is_list(Blocks) ->
                    Blocks1 = [ {proplists:get_value(name, B),B} || B <- Blocks ],
                    z_utils:prop_replace(blocks, normalize_blocks(Blocks1, Context), Props);
                _ ->
                    Props
            end;
        _ ->
            Keys = block_ids(OrgProps, []),
            Dict = lists:foldr(
                            fun ({"block-", _}, Acc) ->
                                    Acc;
                                ({"block-"++Name, Val}, Acc) ->
                                    Ts = string:tokens(Name, "-"),
                                    BlockId = iolist_to_binary(tl(lists:reverse(Ts))),
                                    BlockField = lists:last(Ts),
                                    dict:append(BlockId, {BlockField, Val}, Acc)
                            end,
                            dict:new(),
                            BPs),
            Blocks = normalize_blocks([ {K, dict:fetch(K, Dict)} || K <- Keys ], Context),
            [{blocks, Blocks++proplists:get_value(blocks, Ps, [])} | proplists:delete(blocks, Ps) ]
    end.

recombine_blocks_import(Props, _OrgProps, Context) ->
    {BPs, Ps} = lists:partition(fun({"blocks."++ _, _}) -> true; (_) -> false end, Props),
    case BPs of
        [] ->
            Props;
        _ ->
            {Dict,Keys} = lists:foldr(
                            fun({"blocks."++Name, Val}, {Acc,KeyAcc}) ->
                                [BlockId,BlockField] = string:tokens(Name, "."),
                                KeyAcc1 = case lists:member(BlockId, KeyAcc) of
                                            true -> KeyAcc;
                                            false -> [ BlockId | KeyAcc ]
                                          end,
                                {dict:append(BlockId, {BlockField, Val}, Acc), KeyAcc1}
                            end,
                            {dict:new(),[]},
                            BPs),
            Blocks = normalize_blocks([ {K, dict:fetch(K, Dict)} || K <- Keys ], Context),
            [{blocks, Blocks++proplists:get_value(blocks, Ps, [])} | proplists:delete(blocks, Ps) ]
    end.

block_ids([], Acc) ->
    lists:reverse(Acc);
block_ids([{"block-"++Name,_}|Rest], Acc) when Name =/= [] ->
    Ts = string:tokens(Name, "-"),
    BlockId = iolist_to_binary(tl(lists:reverse(Ts))),
    case lists:member(BlockId, Acc) of
        true -> block_ids(Rest, Acc);
        false -> block_ids(Rest, [BlockId|Acc])
    end;
block_ids([_|Rest], Acc) ->
    block_ids(Rest, Acc).


normalize_blocks(Blocks, Context) ->
    Blocks1 = lists:map(
                 fun({Name,B}) ->
                    normalize_block(Name, B, Context)
                 end,
                 Blocks),
    lists:filter(fun(B) ->
                    case proplists:get_value(type, B) of
                        <<>> -> false;
                        undefined -> false;
                        _ -> true
                    end
                 end,
                 Blocks1).

normalize_block(Name, B, Context) ->
    Props = lists:map(fun
                  ({rsc_id, V})         -> {rsc_id, m_rsc:rid(V, Context)};
                  ({"rsc_id", V})       -> {rsc_id, m_rsc:rid(V, Context)};
                  ({<<"rsc_id">>, V})   -> {rsc_id, m_rsc:rid(V, Context)};
                  ({"is_" ++ _ = K, V}) -> {to_existing_atom(K), z_convert:to_bool(V)};
                  ({<<"is_", _/binary>> = K, V}) -> {to_existing_atom(K), z_convert:to_bool(V)};
                  ({K, V}) when is_list(K); is_binary(K) -> {to_existing_atom(K), V};
                  (Pair) -> Pair
              end,
              B),
    case proplists:is_defined(name, Props) of
        true -> Props;
        false -> [{name, Name} | Props]
    end.

to_existing_atom(K) when is_binary(K) ->
    binary_to_existing_atom(K, utf8);
to_existing_atom(K) when is_list(K) ->
    list_to_existing_atom(K);
to_existing_atom(K) when is_atom(K) ->
    K.

%% @doc Accept only configured languages
filter_langs(L, Cfg) ->
    lists:filter(fun(LangS) ->
                    lists:member(LangS, Cfg)
                 end,
                 L).



config_langs(Context) ->
    case m_config:get(i18n, language_list, Context) of
        undefined -> [en];
        Cfg -> [ Code || {Code, _} <- proplists:get_value(list, Cfg, [{en,[]}]) ]
    end.


update_page_path_log(RscId, OldProps, NewProps, Context) ->
    Old = proplists:get_value(page_path, OldProps),
    New = proplists:get_value(page_path, NewProps, not_updated),
    case {Old, New} of
        {_, not_updated} ->
            ok;
        {Old, Old} ->
            %% not changed
            ok;
        {undefined, _} ->
            %% no old page path
            ok;
        {Old, New} ->
            %% update
            z_db:q("DELETE FROM rsc_page_path_log WHERE page_path = $1 OR page_path = $2", [New, Old], Context),
            z_db:q("INSERT INTO rsc_page_path_log(id, page_path) VALUES ($1, $2)", [RscId, Old], Context),
            ok
    end.


test() ->
    [{"publication_start",{{2009,7,9},{0,0,0}}},
          {"publication_end",?ST_JUTTEMIS},
          {"plop","hello"}]
     = recombine_dates(insert_rsc, [
        {"dt:y:0:publication_start", "2009"},
        {"dt:m:0:publication_start", "7"},
        {"dt:d:0:publication_start", "9"},
        {"dt:y:1:publication_end", ""},
        {"dt:m:1:publication_end", ""},
        {"dt:d:1:publication_end", ""},
        {"plop", "hello"}
    ], z_context:new_tests()),
    ok.

-spec local_now(z:context()) -> calendar:datetime().
local_now(Context) ->
    z_datetime:to_local(erlang:universaltime(), Context).
