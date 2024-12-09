%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2024 Marc Worrell, Arjan Scherpenisse
%% @doc Update routines for resources.  For use by the m_rsc module.
%% @end

%% Copyright 2009-2024 Marc Worrell, Arjan Scherpenisse
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
    delete/3,
    update/3,
    update/4,
    duplicate/3,
    duplicate/4,
    merge_delete/4,

    flush/2,

    delete_nocheck/2,

    to_slug/1
]).

-include_lib("zotonic.hrl").

-record(rscupd, {
    id = undefined :: m_rsc:resource_id() | insert_rsc,
    is_escape_texts = true,
    is_acl_check = true,
    is_import = false,
    is_no_touch = false,
    tz = <<"UTC">>,
    expected = []
}).

-define(is_empty(V), (V =:= undefined orelse V =:= <<>> orelse V =:= "" orelse V =:= null)).

%% Minimum amount of characters for a language detect on the basis of texts.
-define(LANGUAGE_DETECT_THRESHOLD, 10).

%% @doc Insert a new resource. Crashes when insertion is not allowed.
-spec insert(m_rsc:props_all(), z:context()) -> {ok, m_rsc:resource_id()} | {error, term()}.
insert(Props, Context) ->
    insert(Props, [{is_escape_texts, true}], Context).

-spec insert(m_rsc:props_all(), list(), z:context()) -> {ok, m_rsc:resource_id()} | {error, term()}.
insert(Props, Options, Context) when is_list(Props) ->
    {ok, Map} = z_props:from_list(Props),
    insert(Map, Options, Context);
insert(Props, Options, Context) when is_map(Props) ->
    PropsDefaults = props_defaults(Props, Context),
    update(insert_rsc, PropsDefaults, Options, Context).


%% @doc Delete a resource
-spec delete(m_rsc:resource(), z:context()) -> ok | {error, atom()}.
delete(Id, Context) ->
    delete(Id, undefined, Context).

-spec delete(m_rsc:resource(), m_rsc:resource(), z:context()) -> ok | {error, atom()}.
delete(1, _FollowUpId, _Context) ->
    {error, eacces};
delete(undefined, _FollowUpId, _Context) ->
    {error, enoent};
delete(Id, FollowUpId, Context)
    when is_integer(Id),
        (is_integer(FollowUpId) orelse FollowUpId =:= undefined) ->
    case {
        z_acl:rsc_deletable(Id, Context),
        FollowUpId =:= undefined orelse m_rsc:exists(FollowUpId, Context)
        }
    of
        {true, true} ->
            case m_rsc:is_a(Id, category, Context) of
                true ->
                    m_category:delete(Id, FollowUpId, Context);
                false ->
                    delete_nocheck(Id, FollowUpId, Context)
            end;
        {false, _} ->
            {error, eacces};
        {_, false} ->
            {error, unknown_followup}
    end;
delete(Name, FollowUpId, Context) ->
    delete(
        m_rsc:rid(Name, Context),
        m_rsc:rid(FollowUpId, Context),
        Context).

%% @doc Delete a resource, no check on rights etc is made. This is called by m_category:delete/3
-spec delete_nocheck(m_rsc:resource(), z:context()) -> ok | {error, atom()}.
delete_nocheck(Id, Context) ->
    delete_nocheck(m_rsc:rid(Id, Context), undefined, Context).

delete_nocheck(1, _OptFollowUpId, _Context) ->
    {error, eacces};
delete_nocheck(Id, OptFollowUpId, Context) when is_integer(Id) ->
    Referrers = m_edge:subjects(Id, Context),
    CatList = m_rsc:is_a(Id, Context),
    Props = m_rsc:get(Id, Context),

    % Outside transaction as due to race conditions we might
    % have a duplicate insert into the rsc_gone table. That
    % triggers an error which cancels the transaction F below.
    _ = m_rsc_gone:gone(Id, OptFollowUpId, Context),

    F = fun(Ctx) ->
        z_notifier:notify_sync(#rsc_delete{id = Id, is_a = CatList}, Ctx),
        z_db:delete(rsc, Id, Ctx)
    end,
    case z_db:transaction(F, Context) of
        {ok, _RowsDeleted} ->
            % Sync the caches
            [z_depcache:flush(SubjectId, Context) || SubjectId <- Referrers],
            flush(Id, CatList, Context),
            %% Notify all modules that the rsc has been deleted
            z_notifier:notify_sync(
                #rsc_update_done{
                    action = delete,
                    id = Id,
                    pre_is_a = CatList,
                    post_is_a = [],
                    pre_props = Props,
                    post_props = #{}
                }, Context),
             z_mqtt:publish(
                 [ <<"model">>, <<"rsc">>, <<"event">>, Id, <<"delete">> ],
                 #{
                    id => Id,
                    pre_is_a => CatList
                 },
                 Context),
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
    {error, eacces};
merge_delete(_WinnerId, admin, _Options, _Context) ->
    {error, eacces};
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
            {error, eacces}
    end.

%% @doc Merge two resources, delete the 'loser'
-spec merge_delete_nocheck(integer(), integer(), list(), #context{}) -> ok.
merge_delete_nocheck(WinnerId, LoserId, Opts, Context) ->
    IsMergeTrans = proplists:get_value(is_merge_trans, Opts, false),
    z_notifier:map(#rsc_merge{
            winner_id = WinnerId,
            loser_id = LoserId,
            is_merge_trans = IsMergeTrans
        },
        Context),
    ok = m_edge:merge(WinnerId, LoserId, Context),
    m_media:merge(WinnerId, LoserId, Context),
    m_identity:merge(WinnerId, LoserId, Context),
    move_creator_modifier_ids(WinnerId, LoserId, Context),
    PropsLoser = m_rsc:get(LoserId, Context),
    ok = delete_nocheck(LoserId, WinnerId, Context),
    case merge_copy_props(WinnerId, PropsLoser, IsMergeTrans, Context) of
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

merge_copy_props(WinnerId, LoserProps, IsMergeTrans, Context) ->
    LoserProps1 = ensure_merge_language(LoserProps, Context),
    maps:fold(
        fun(K, V, Acc) ->
            case merge_copy_props_1(WinnerId, K, V, IsMergeTrans, Context) of
                {ok, V1} ->
                    Acc#{ K => V1 };
                false ->
                    Acc
            end
        end,
        #{},
        LoserProps1).

merge_copy_props_1(_WinnerId, P, _V, _IsMergeTrans, _Context)
    when P =:= <<"creator">>; P =:= <<"creator_id">>; P =:= <<"modifier">>; P =:= <<"modifier_id">>;
         P =:= <<"created">>; P =:= <<"modified">>; P =:= <<"version">>;
         P =:= <<"id">>; P =:= <<"is_published">>; P =:= <<"is_protected">>; P =:= <<"is_dependent">>;
         P =:= <<"is_authoritative">>; P =:= <<"pivot_geocode">>; P =:= <<"pivot_geocode_qhash">>;
         P =:= <<"category_id">> ->
    false;
merge_copy_props_1(WinnerId, <<"blocks">>, LoserBs, IsMergeTrans, Context) ->
    WinnerBs = m_rsc:p_no_acl(WinnerId, <<"blocks">>, Context),
    {ok, merge_copy_props_blocks(WinnerBs, LoserBs, IsMergeTrans, Context)};
merge_copy_props_1(_WinnerId, _K, undefined, _IsMergeTrans, _Context) -> false;
merge_copy_props_1(_WinnerId, _K, [], _IsMergeTrans, _Context) -> false;
merge_copy_props_1(_WinnerId, _K, <<>>, _IsMergeTrans, _Context) -> false;
merge_copy_props_1(WinnerId, P, LoserValue, IsMergeTrans, Context) ->
    case m_rsc:p_no_acl(WinnerId, P, Context) of
        undefined when IsMergeTrans, P =:= <<"language">>, is_list(LoserValue) ->
            V1 = lists:usort([ z_language:default_language(Context) ] ++ LoserValue),
            {ok, V1};
        undefined ->
            false;
        <<>> ->
            false;
        [] ->
            false;
        Value when IsMergeTrans, P =:= <<"language">>, is_list(Value), is_list(LoserValue) ->
            V1 = lists:usort(Value ++ LoserValue),
            {ok, V1};
        Value when IsMergeTrans ->
            {ok, merge_trans(Value, LoserValue, Context)};
        _Value ->
            false
    end.

ensure_merge_language(Props, Context) ->
    case maps:get(<<"language">>, Props, undefined) of
        Languages when ?is_empty(Languages) ->
            Props#{
                <<"language">> => [ z_language:default_language(Context) ]
            };
        _ ->
            Props
    end.

merge_trans(#trans{ tr = Winner }, #trans{ tr = Loser }, _Context) ->
    Tr = lists:foldl(
        fun ({Lang,Text}, Acc) ->
            case proplists:get_value(Lang, Acc) of
                undefined -> [ {Lang,Text} | Acc ];
                _ -> Acc
            end
        end,
        Winner,
        Loser),
    #trans{ tr = Tr };
merge_trans(Winner, #trans{} = Loser, Context) when is_binary(Winner) ->
    V1 = #trans{ tr = [ {z_language:default_language(Context), Winner} ] },
    merge_trans(V1, Loser, Context);
merge_trans(#trans{} = Winner, Loser, Context) when is_binary(Loser) ->
    V1 = #trans{ tr = [ {z_language:default_language(Context), Loser} ] },
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
            case find_block( maps:get(<<"name">>, WB, undefined), LoserBs ) of
                undefined ->
                    WB;
                LB ->
                    WT = maps:get(<<"type">>, WB, undefined),
                    case maps:get(<<"type">>, LB) of
                        WT -> merge_block_single(WB, LB, Context);
                        _ -> WB
                    end
            end
        end,
        WinnerBs).

find_block(_Name, []) -> undefined;
find_block(Name, [ #{ name := Name } = B | _Bs ]) -> B;
find_block(Name, [ _ | Bs ]) -> find_block(Name, Bs).

merge_block_single(W, L, Context) ->
    maps:map(
        fun(K, WV) ->
            case maps:get(K, L, undefined) of
                undefined ->
                    WV;
                LV ->
                    WV1 = merge_trans(WV, LV, Context),
                    WV1
            end
        end,
        W).


%% Flush all cached entries depending on this entry, one of its subjects or its categories.
flush(Id, Context) ->
    RscId = m_rsc:rid(Id, Context),
    CatList = m_rsc:is_a(RscId, Context),
    flush(RscId, CatList, Context).

flush(Id, CatList, Context) ->
    RscId = m_rsc:rid(Id, Context),
    z_depcache:flush(RscId, Context),
    [z_depcache:flush(Cat, Context) || Cat <- CatList],
    z_acl:flush(RscId),
    ok.


%% @doc Duplicate a resource, creating a new resource with the given title.
-spec duplicate(m_rsc:resource(), m_rsc:props_all(), z:context()) -> {ok, m_rsc:resource_id()} | {error, term()}.
duplicate(Id, DupProps, Context) ->
    duplicate(Id, DupProps, [], Context).

-spec duplicate(m_rsc:resource(), m_rsc:props_all(), m_rsc:duplicate_options(), z:context()) -> {ok, m_rsc:resource_id()} | {error, term()}.
duplicate(Id, DupProps, DupOpts, Context) when is_list(DupProps) ->
    {ok, DupMap} = z_props:from_list(DupProps),
    duplicate(Id, DupMap, DupOpts, Context);
duplicate(Id, DupProps, DupOpts, Context) when is_integer(Id) ->
    case z_acl:rsc_visible(Id, Context) of
        true ->
            case m_rsc:get_raw(Id, Context) of
                {ok, RawProps} ->
                    RscUpd = #rscupd{
                        id = insert_rsc,
                        is_escape_texts = false
                    },
                    FilteredProps = props_filter_protected(RawProps, RscUpd),
                    SafeDupProps = escape_props(true, DupProps, Context),
                    InsProps = maps:fold(
                        fun(Key, Value, Acc) ->
                            Acc#{ Key => Value }
                        end,
                        FilteredProps,
                        SafeDupProps#{
                            <<"name">> => undefined,
                            <<"uri">> => undefined,
                            <<"page_path">> => undefined,
                            <<"is_authoritative">> => true,
                            <<"is_protected">> => false
                        }),
                    case insert(InsProps, [{is_escape_texts, false}], Context) of
                        {ok, NewId} ->
                            case proplists:get_value(edges, DupOpts, true) of
                                true ->
                                    m_edge:duplicate(Id, NewId, Context);
                                _ ->
                                    ok
                            end,
                            case proplists:get_value(medium, DupOpts, true) of
                                true ->
                                    m_media:duplicate(Id, NewId, Context);
                                _ ->
                                    ok
                            end,
                            {ok, NewId};
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        false ->
            {error, eacces}
    end;
duplicate(undefined, _DupProps, _DupOpts, _Context) ->
    {error, enoent};
duplicate(Id, DupProps, DupOpts, Context) ->
    duplicate(m_rsc:rid(Id, Context), DupProps, DupOpts, Context).

%% @doc Update a resource
-spec update(
        m_rsc:resource() | insert_rsc,
        m_rsc:props_all() | m_rsc:update_function(),
        z:context()
    ) ->
    {ok, m_rsc:resource_id()} | {error, term()}.
update(Id, Props, Context) ->
    update(Id, Props, [], Context).

%% @doc Update a resource.
%% Options flags:
%%      is_escape_texts (default: true}
%%      is_acl_check    (default: true)
%%      no_touch        (default: false)
%%      is_import       (default: false)
%% Other options:
%%      tz - timezone for date conversions
%%      expected - list with property value pairs that
%%                 are expected, fail if the properties
%%                 are different.
%%
%% {is_escape_texts, false} checks if the texts are escaped, and if not then it
%% will escape. This prevents "double-escaping" of texts.
-spec update(
        m_rsc:resource() | insert_rsc,
        m_rsc:props_all() | m_rsc:update_function(),
        list() | boolean(),
        z:context()
    ) ->
    {ok, m_rsc:resource_id()} | {error, term()}.
update(Id, Props, false, Context) ->
    update(Id, Props, [{is_escape_texts, false}], Context);
update(Id, Props, true, Context) ->
    update(Id, Props, [{is_escape_texts, true}], Context);
update(Name, PropsOrFun, Options, Context) when not is_integer(Name), Name =/= insert_rsc ->
    case m_rsc:name_to_id(Name, Context) of
        {ok, Id} ->
            update(Id, PropsOrFun, Options, Context);
        {error, _} ->
            case m_rsc:rid(Name, Context) of
                undefined -> {error, enoent};
                Id -> update(Id, PropsOrFun, Options, Context)
            end
    end;
update(Id, Props, Options, Context) when is_list(Props) ->
    {ok, Props1} = z_props:from_list(Props),
    OptionsTz = case proplists:lookup(tz, Options) of
        {tz, _} ->
            % Timezone set in the update options
            Options;
        none ->
            case maps:find(<<"tz">>, Props1) of
                {ok, Tz} ->
                    % Timezone specified in the update
                    [ {tz, Tz} | Options ];
                error ->
                    case Props of
                        [ {K, _} | _ ] when is_binary(K); is_list(K) ->
                            % On a form post input we use the timezone of
                            % of the request context.
                            [ {tz, z_context:tz(Context)} | Options ];
                        _ ->
                            % Assume UTC
                            Options
                    end
            end
    end,
    update_1(Id, Props1, OptionsTz, Context);
update(Id, PropsOrFun0, Options, Context) when is_integer(Id); Id =:= insert_rsc ->
    PropsOrFun = binary_keys(PropsOrFun0),
    update_1(Id, PropsOrFun, Options, Context).

update_1(Id, PropsOrFun, Options, Context) when is_integer(Id); Id =:= insert_rsc ->
    IsImport = proplists:get_value(is_import, Options, false),
    Tz0 = case is_map(PropsOrFun) of
        true when not IsImport ->
            % Timezone in the update props is leading over the timezone in the options
            case maps:get(<<"tz">>, PropsOrFun, undefined) of
                undefined -> proplists:get_value(tz, Options, <<"UTC">>);
                <<>> -> proplists:get_value(tz, Options, <<"UTC">>);
                PropTz -> PropTz
            end;
        _ ->
            proplists:get_value(tz, Options, <<"UTC">>)
    end,
    % Sanity fallback for 'undefined' tz in the options
    Tz = case Tz0 of
        undefined -> <<"UTC">>;
        <<>> -> <<"UTC">>;
        _ -> Tz0
    end,
    % Also accept the old non "is_.." options
    RscUpd = #rscupd{
        id = Id,
        is_escape_texts = proplists:get_value(is_escape_texts, Options,
                                proplists:get_value(escape_texts, Options, true)),
        is_acl_check = proplists:get_value(is_acl_check, Options,
                                proplists:get_value(acl_check, Options, true)),
        is_import = proplists:get_value(is_import, Options, false),
        is_no_touch = proplists:get_value(no_touch, Options, false)
                      andalso z_acl:is_admin(Context),
        tz = Tz,
        expected = proplists:get_value(expected, Options, [])
    },
    update_imported_check(RscUpd, PropsOrFun, Context).

binary_keys(Map) when is_map(Map) ->
    z_props:from_map(Map);
binary_keys(Fun) ->
    Fun.

update_imported_check(#rscupd{is_import = true, id = Id} = RscUpd, PropsOrFun, Context) when is_integer(Id) ->
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
    update_editable_check(RscUpd, PropsOrFun, Context);
update_imported_check(RscUpd, PropsOrFun, Context) ->
    update_editable_check(RscUpd, PropsOrFun, Context).


update_editable_check(#rscupd{id = Id, is_acl_check = true } = RscUpd, PropsOrFun, Context) when is_integer(Id) ->
    case z_acl:rsc_editable(Id, Context) of
        true ->
            update_normalize_props(RscUpd, PropsOrFun, Context);
        false ->
            {error, eacces}
    end;
update_editable_check(RscUpd, PropsOrFun, Context) ->
    update_normalize_props(RscUpd, PropsOrFun, Context).

update_normalize_props(#rscupd{id = Id, tz = Tz} = RscUpd, Props, Context) when is_map(Props) ->
    % Convert the dates in the properties to Erlang DateTime and optionally convert
    % them to UTC as well.
    IsAllDay = is_all_day(Id, Props, Context),
    PropsDates = z_props:normalize_dates(Props, IsAllDay, Tz),
    % The 'blocks' must be a list (of maps), if anything else then
    % the blocks are set to 'undefined' and removed from the rsc
    PropsBlocks = case maps:find(<<"blocks">>, PropsDates) of
        {ok, L} when is_list(L) ->
            L1 = lists:filter(
                fun
                    (B) when is_map(B) -> maps:is_key(<<"type">>, B);
                    (_) -> false
                end,
                L),
            PropsDates#{ <<"blocks">> => L1 };
        {ok, _} ->
            PropsDates#{ <<"blocks">> => undefined };
        error ->
            PropsDates
    end,
    update_transaction(RscUpd, fun(_, _, _) -> {ok, PropsBlocks} end, Context);
update_normalize_props(RscUpd, Func, Context) when is_function(Func) ->
    update_transaction(RscUpd, Func, Context).


%% If the "all day" flag is set then the date_start and date_end are specified
%% in UTC. This is used for dates that are fixed across timezones. An example
%% is Christmas, which is always on the 25th of december, local time.
%% All other dates are not affected by the flag and are subject to the timezone
%% specified in the update options or resource properties.
is_all_day(_Id, #{ <<"date_is_all_day">> := IsAllDay }, _Context) ->
    z_convert:to_bool(IsAllDay);
is_all_day(Id, _Props, Context) when is_integer(Id) ->
    z_convert:to_bool(m_rsc:p_no_acl(Id, date_is_all_day, Context));
is_all_day(_Id, _Props, _Context) ->
    false.


update_transaction(RscUpd, Func, Context) ->
    Result = z_db:transaction(
        fun(Ctx) ->
            update_transaction_fun_props(RscUpd, Func, Ctx)
        end,
        Context),
    update_result(Result, RscUpd, Context).

update_result({ok, NewId, notchanged}, _RscUpd, _Context) ->
    z_acl:flush(NewId),
    {ok, NewId};
update_result({ok, NewId, {OldProps, NewProps, OldCatList, IsCatInsert}}, #rscupd{id = Id}, Context) ->
    % Flush some low level caches
    case maps:get(<<"name">>, NewProps, undefined) of
        undefined -> nop;
        Name -> z_depcache:flush({rsc_name, z_string:to_name(Name)}, Context)
    end,
    case maps:get(<<"uri">>, NewProps, undefined) of
        undefined -> nop;
        Uri -> z_depcache:flush({rsc_uri, z_convert:to_binary(Uri)}, Context)
    end,
    z_acl:flush(NewId),

    % Flush category caches if a category is inserted.
    case IsCatInsert of
        true -> m_category:flush(Context);
        false -> nop
    end,

    % Flush all cached content that is depending on one of the updated categories
    z_depcache:flush(NewId, Context),
    NewCatList = m_rsc:is_a(NewId, Context),
    Cats = lists:usort(NewCatList ++ OldCatList),
    [z_depcache:flush(Cat, Context) || Cat <- Cats],

    % Notify that a new resource has been inserted, or that an existing one is updated
    Note = #rsc_update_done{
        action = case Id of insert_rsc -> insert; _ -> update end,
        id = NewId,
        pre_is_a = OldCatList,
        post_is_a = NewCatList,
        pre_props = OldProps,
        post_props = NewProps
    },
    z_notifier:notify_sync(Note, Context),
    Topic = [
        <<"model">>, <<"rsc">>, <<"event">>, NewId,
        case Id of
            insert_rsc -> <<"insert">>;
            _ -> <<"update">>
        end
    ],
    z_mqtt:publish(
        Topic,
        #{
            id => NewId,
            pre_is_a => OldCatList,
            post_is_a => NewCatList
        },
        Context),

    % Return the updated or inserted id
    {ok, NewId};
update_result({rollback, {error, _} = Er}, _RscUpd, _Context) ->
    Er;
update_result({rollback, {_Why, _} = Er}, _RscUpd, _Context) ->
    {error, Er};
update_result({error, {expected, _, _}} = Error, #rscupd{ id = Id }, Context) ->
    % We might have an out-of-date cached value, flush caches to force
    % an update from the database.
    z_depcache:flush_process_dict(),
    flush(Id, Context),
    Error;
update_result({error, _} = Error, _RscUpd, _Context) ->
    Error.


%% @doc This is running inside the rsc update db transaction
update_transaction_fun_props(#rscupd{id = Id} = RscUpd, Func, Context) ->
    case get_raw_lock(Id, Context) of
        {ok, Raw} ->
            update_transaction_fun_props_1(RscUpd, Raw, Func, Context);
        {error, _} = Error ->
            {rollback, Error}
    end.

update_transaction_fun_props_1(#rscupd{id = Id} = RscUpd, Raw, Func, Context) ->
    case Func(Id, Raw, Context) of
        {ok, UpdateProps} ->
            update_transaction_filter_props(RscUpd, UpdateProps, Raw, Context);
        {error, _} = Error ->
            {rollback, Error}
    end.

update_transaction_filter_props(#rscupd{id = Id} = RscUpd, UpdateProps, Raw, Context) ->
    {Edges, UpdateProps1} = split_edges(UpdateProps),
    EditableProps = props_filter_protected( props_filter( props_trim(UpdateProps1), Context), RscUpd),
    SafeProps = escape_props(RscUpd#rscupd.is_escape_texts, EditableProps, Context),
    SafeSlugProps = generate_slug(Id, SafeProps, Context),
    case preflight_check(Id, SafeSlugProps, Context) of
        ok ->
            try
                throw_if_category_not_allowed(Id, SafeSlugProps, RscUpd#rscupd.is_acl_check, Context),
                Result = update_transaction_fun_insert(RscUpd, SafeSlugProps, Raw, UpdateProps, Context),
                insert_edges(Result, Edges, Context)
            catch
                throw:{error, _} = Error -> {rollback, Error}
            end;
        {error, _} = Error ->
            {rollback, Error}
    end.

split_edges(Props) ->
    maps:fold(
        fun
            (<<"s.", Pred/binary>>, E, {Es, Ps}) ->
                Es1 = [ {subject, Pred, E}  | Es ],
                {Es1, Ps};
            (<<"o.", Pred/binary>>, E, {Es, Ps}) ->
                Es1 = [ {object, Pred, E}  | Es ],
                {Es1, Ps};
            (<<"o">>, Map, {Es, Ps}) when is_map(Map) ->
                Es1 = split_edges_map(object, Map, Es),
                {Es1, Ps};
            (<<"s">>, Map, {Es, Ps}) when is_map(Map) ->
                Es1 = split_edges_map(subject, Map, Es),
                {Es1, Ps};
            (K, V, {Es, Ps}) ->
                {Es, Ps#{ K => V }}
        end,
        {[], #{}},
        Props).

split_edges_map(What, Map, Acc) ->
    maps:fold(
        fun
            (Pred, IdOrIds, PAcc) ->
                [ {What, Pred, IdOrIds} | PAcc ]
        end,
        Acc,
        Map).

insert_edges({ok, _Id, _Res} = Result, [], _Context) ->
    Result;
insert_edges({ok, Id, _Res} = Result, Edges, Context) ->
    case z_acl:is_sudo(Context) of
        true ->
            ?LOG_ERROR(#{
                text => <<"Not allowed to insert edges during rsc update with sudo">>,
                result => error,
                error => eacces,
                in => zotonic_core,
                rsc_id => Id,
                edges => Edges
            }),
            % ignore edge insertion error
            Result;
        false ->
            lists:foreach(
                fun
                    ({_ ,<<>>, _}) ->
                        ok;
                    ({_ ,undefined, _}) ->
                        ok;
                    ({_ ,_, undefined}) ->
                        ok;
                    ({object, Pred, Es}) when is_list(Es) ->
                        Es1 = lists:filtermap(
                            fun(EId) ->
                                case m_rsc:rid(EId, Context) of
                                    undefined -> false;
                                    Rid -> {true, Rid}
                                end
                            end,
                            Es),
                        m_edge:replace(Id, Pred, Es1, Context);
                    ({object, Pred, E}) ->
                        m_edge:insert(Id, Pred, E, Context);
                    ({subject, Pred, Es}) when is_list(Es) ->
                        lists:map(
                            fun(E) -> m_edge:insert(E, Pred, Id, Context) end,
                            Es);
                    ({subject, Pred, E}) ->
                        m_edge:insert(E, Pred, Id, Context)
                end,
                Edges),
            Result
    end;
insert_edges({error, _} = Error , _, _Context) ->
    Error.

escape_props(true, Props, Context) ->
    z_sanitize:escape_props(Props, Context);
escape_props(false, Props, Context) ->
    z_sanitize:escape_props_check(Props, Context).

%% @doc Fetch the complete resource from the database, lock for subsequent updates.
%%      The resource is not filtered by the ACL as this is the basis for the update.
get_raw_lock(insert_rsc, _Context) ->
    {ok, #{}};
get_raw_lock(Id, Context) ->
    m_rsc:get_raw_lock(Id, Context).

update_transaction_fun_insert(#rscupd{id = insert_rsc} = RscUpd, Props, _Raw, UpdateProps, Context) ->
    % Allow the initial insertion props to be modified.
    CategoryId = z_convert:to_integer(maps:get(<<"category_id">>, Props)),
    InitProps0 = #{
        <<"version">> => 0,
        <<"category_id">> => CategoryId,
        <<"content_group_id">> => maps:get(<<"content_group_id">>, Props, undefined),
        <<"is_published">> => false,
        <<"publication_start">> => undefined
    },
    InitProps = case z_convert:to_integer(maps:get(<<"visible_for">>, Props, undefined)) of
        undefined -> InitProps0;
        VisFor -> InitProps0#{ <<"visible_for">> => VisFor }
    end,
    InsProps = z_notifier:foldr(#rsc_insert{ props = Props }, InitProps, Context),

    % Create dummy resource with correct creator, category and content group.
    % This resource will be updated with the other properties.
    InsertId = case maps:get(<<"creator_id">>, UpdateProps, undefined) of
                   Self when Self =:= <<"self">>; Self =:= self ->
                        {ok, InsId} = z_db:insert(
                            rsc,
                            InsProps#{ <<"creator_id">> => undefined },
                            Context),
                        1 = z_db:q("update rsc set creator_id = id where id = $1", [InsId], Context),
                        InsId;
                   CreatorId when is_integer(CreatorId) ->
                        {ok, InsId} = case z_acl:is_admin(Context) of
                            true ->
                                z_db:insert(
                                    rsc,
                                    InsProps#{ <<"creator_id">> => CreatorId },
                                    Context);
                            false ->
                                z_db:insert(
                                    rsc,
                                    InsProps#{ <<"creator_id">> => z_acl:user(Context) },
                                    Context)
                        end,
                        InsId;
                   undefined ->
                       {ok, InsId} = z_db:insert(
                            rsc,
                            InsProps#{ <<"creator_id">> => z_acl:user(Context) },
                            Context),
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
     % Place the inserted properties over the update properties.
     Props1 = maps:fold(
        fun
            (<<"version">>, _V, Acc) -> Acc;
            (<<"creator_id">>, _V, Acc) -> Acc;
            (<<"is_published">>, _V, Acc) -> Acc;
            (<<"publication_start">>, _V, Acc) -> Acc;
            (P, V, Acc) when is_binary(P) ->
                Acc#{ P => V }
        end,
        Props,
        InsProps),
    update_transaction_fun_db(RscUpd, InsertId, Props1, InsProps, [], IsCatInsert, Context);
update_transaction_fun_insert(#rscupd{id = Id} = RscUpd, Props, Raw, UpdateProps, Context) ->
    Props1 = maps:remove(<<"creator_id">>, Props),
    Props2 = case z_acl:is_admin(Context) of
                 true ->
                     case maps:get(<<"creator_id">>, UpdateProps, undefined) of
                         Self when Self =:= <<"self">>; Self =:= self ->
                             Props#{ <<"creator_id">> => Id };
                         CreatorId when is_integer(CreatorId) ->
                             Props#{ <<"creator_id">> => CreatorId };
                         undefined ->
                             Props1
                     end;
                 false ->
                     Props1
             end,
    IsA = m_rsc:is_a(Id, Context),
    update_transaction_fun_expected(RscUpd, Id, Props2, Raw, IsA, false, Context).

update_transaction_fun_expected(#rscupd{expected = Expected} = RscUpd, Id, Props1, Raw, IsA, false,
    Context) ->
    case check_expected(Raw, Expected, Context) of
        ok ->
            update_transaction_fun_db(RscUpd, Id, Props1, Raw, IsA, false, Context);
        {error, _} = Error ->
            Error
    end.


check_expected(_Raw, [], _Context) ->
    ok;
check_expected(Raw, [{Key, F} | Es], Context) when is_function(F) ->
    case F(z_convert:to_binary(Key), Raw, Context) of
        true -> check_expected(Raw, Es, Context);
        false -> {error, {expected, Key, maps:get(Key, Raw, undefined)}}
    end;
check_expected(Raw, [{Key, Value} | Es], Context) ->
    case maps:get(z_convert:to_binary(Key), Raw, undefined) of
        Value -> check_expected(Raw, Es, Context);
        Other -> {error, {expected, Key, Other}}
    end.


update_transaction_fun_db(RscUpd, Id, Props, Raw, IsABefore, IsCatInsert, Context) ->
    {ok, Version} = maps:find(<<"version">>, Raw),
    UpdateProps = Props#{
        <<"version">> => Version + 1
    },
    IsInsert = (RscUpd#rscupd.id =:= insert_rsc),
    UpdateProps1 = set_if_normal_update(RscUpd, <<"modified">>, erlang:universaltime(), UpdateProps),
    UpdateProps2 = set_if_normal_update(RscUpd, <<"modifier_id">>, z_acl:user(Context), UpdateProps1),
    UpdResult = z_notifier:foldr(
        #rsc_update{
            action = case IsInsert of
                        true -> insert;
                        false -> update
                     end,
            id = Id,
            props = Raw
        },
        {ok, UpdateProps2},
        Context),
    update_transaction_fun_db_1(UpdResult, Id, RscUpd, Raw, IsABefore, IsCatInsert, Context).

update_transaction_fun_db_1({error, _} = Error, _Id, _RscUpd, _Raw, _IsABefore, _IsCatInsert, _Context) ->
    Error;
update_transaction_fun_db_1({ok, UpdatePropsN}, Id, RscUpd, Raw, IsABefore, IsCatInsert, Context) ->
    % Pre-pivot of the category-id to the category sequence nr.
    UpdatePropsN1 = case maps:get(<<"category_id">>, UpdatePropsN, undefined) of
                        undefined ->
                            UpdatePropsN;
                        CatId ->
                            CatNr = z_db:q1("
                                    select nr
                                    from hierarchy
                                    where id = $1
                                      and name = '$category'",
                                [CatId],
                                Context),
                            UpdatePropsN#{ <<"pivot_category_nr">> => CatNr }
                    end,

    % 1. Merge UpdatePropsN into Raw for complete view
    NewProps = maps:merge(Raw, UpdatePropsN1),

    % 2. Ensure language tag
    Langs = case maps:get(<<"language">>, NewProps, []) of
        [ _ | _ ] = Lngs -> Lngs;
        _ -> z_props:extract_languages(NewProps)
    end,
    Langs1 = case Langs of
        [] ->
            % If no language, but medium_language is defined, then assume
            % content is also in the medium language
            case maps:get(<<"medium_language">>, NewProps, undefined) of
                undefined -> detect_language(NewProps, Context);
                <<>> -> detect_language(NewProps, Context);
                MLang -> [ MLang ]
            end;
        _ ->
            Langs
    end,
    % Only editable languages
    Langs2 = lists:filtermap(
        fun(Lang) ->
            case z_language:to_language_atom(Lang) of
                {ok, Iso} ->
                    case z_language:is_language_editable(Iso, Context) of
                        false ->
                            ?LOG_INFO(#{
                                text => <<"Dropping non editable language from resource">>,
                                in => zotonic_core,
                                language => Iso,
                                rsc_id => Id
                            }),
                            false;
                        true ->
                            {true, Iso}
                    end;
                {error, not_a_language} ->
                    false
            end
        end,
        Langs1),
    % Ensure there is always a language
    Langs3 = case Langs2 of
        [] -> [ z_context:language(Context ) ];
        _ -> Langs2
    end,
    NewPropsLang = maybe_set_langs(NewProps, Langs3),

    % 4. Prune languages
    NewPropsLangPruned = z_props:prune_languages(NewPropsLang, maps:get(<<"language">>, NewPropsLang)),

    % 5. Diff the update
    NewPropsLangPruned1 = set_forced_props(Id, clear_empty(NewPropsLangPruned, Context)),
    NewPropsDiff = diff(NewPropsLangPruned1, Raw),

    % 6. Ensure that there is a Timezone set in the saved resource
    PropsTz = maps:get(<<"tz">>, NewPropsDiff, undefined),
    MapsTz = maps:get(<<"tz">>, Raw, undefined),
    NewPropsDiffTz = case {MapsTz, PropsTz} of
        {undefined, undefined} ->
            NewPropsDiff#{ <<"tz">> => RscUpd#rscupd.tz };
        _ ->
            NewPropsDiff
    end,

    % 7. Ensure that the publication_start is set if the is_published flag is set
    IsPublished = case NewPropsDiffTz of
        #{ <<"is_published">> := IsPub } ->
            IsPub;
        _ ->
            case Raw of
                #{ <<"is_published">> := IsPub } -> IsPub;
                _ -> false
            end
    end,
    HasPubStart = case NewPropsDiffTz of
        #{ <<"publication_start">> := {_, _} } ->
            true;
        #{ <<"publication_start">> := undefined } ->
            false;
        _ ->
            case Raw of
                #{ <<"publication_start">> := {_, _} } -> true;
                _ -> false
            end
    end,
    NewPropsDiffPub = if
        IsPublished and not HasPubStart ->
            NewPropsDiffTz#{
                <<"publication_start">> => calendar:universal_time()
            };
        true ->
            NewPropsDiffTz
    end,

    % 8. Perform optional update, check diff
    IsInsert = (RscUpd#rscupd.id =:= insert_rsc),
    case RscUpd#rscupd.is_acl_check =:= false
        orelse is_update_allowed(IsInsert, Id, NewPropsLangPruned, Context)
    of
        true ->
            case (IsInsert orelse is_changed(Raw, NewPropsDiffPub)) of
                true ->
                    UpdatePropsPrePivoted = z_pivot_rsc:pivot_resource_update(Id, NewPropsDiffPub, Raw, Context),
                    case z_db:update(rsc, Id, UpdatePropsPrePivoted, Context) of
                        {ok, 1} ->
                            ok = update_page_path_log(Id, Raw, NewPropsDiffPub, Context),
                            NewPropsFinal = maps:merge(NewPropsLangPruned, UpdatePropsPrePivoted),
                            {ok, Id, {Raw, NewPropsFinal, IsABefore, IsCatInsert}};
                        {error, _} = Error ->
                            Error
                    end;
                false ->
                    {ok, Id, notchanged}
            end;
        false ->
            {error, eacces}
    end.

detect_language(NewProps, Context) ->
    Text = z_html:unescape(z_html:strip(extract_text(NewProps))),
    case z_string:trim(Text) of
        Text1 when size(Text1) > ?LANGUAGE_DETECT_THRESHOLD ->
            case z_notifier:first(#language_detect{ text = Text1 }, Context) of
                undefined ->
                    [ z_context:language(Context) ];
                Language ->
                    [ Language ]
            end;
        _ ->
            [ z_context:language(Context) ]
    end.

extract_text(NewProps) ->
    Texts = [
        extract_text_prop(<<"title">>, NewProps),
        extract_text_prop(<<"chapeau">>, NewProps),
        extract_text_prop(<<"subtitle">>, NewProps),
        extract_text_prop(<<"summary">>, NewProps),
        extract_text_prop(<<"body">>, NewProps)
    ],
    Texts1 = [ T || T <- Texts, T =/= <<>> ],
    iolist_to_binary(lists:join(32, Texts1)).

extract_text_prop(K, Props) ->
    case maps:get(K, Props, undefined) of
        T when is_binary(T) -> T;
        _ -> <<>>
    end.

%% @doc Some forced props, depending on the resource being updated.
set_forced_props(1, Props) ->
    Props#{
        <<"is_protected">> => true
    };
set_forced_props(_Id, Props) ->
    Props.

%% Set all non-column fields with empty values to 'undefined'.
%% This makes the props blob smaller by removing all empty address (etc) fields.
clear_empty(Props, Context) ->
    Cols = z_db:column_names_bin(rsc, Context),
    maps:fold(
        fun
            (K, <<>>, Acc) ->
                case lists:member(K, Cols) of
                    true -> Acc#{ K => <<>> };
                    false -> Acc#{ K => undefined }
                end;
            (K, #trans{} = V, Acc) ->
                case z_utils:is_empty(V) of
                    true -> Acc#{ K => undefined };
                    false -> Acc#{ K => V }
                end;
            (K, V, Acc) ->
                Acc#{ K => V }
        end,
        #{},
        Props).


is_update_allowed(true, Id, NewProps, Context) ->
    z_acl:is_allowed(insert, #acl_rsc{ id = Id, props = NewProps }, Context);
is_update_allowed(false, Id, NewProps, Context) ->
    z_acl:is_allowed(update, #acl_rsc{ id = Id, props = NewProps }, Context).


maybe_set_langs(#{ <<"language">> := Langs } = Props, NewLangs) when is_list(Langs) ->
    case Langs of
        NewLangs -> Props;
        _ -> Props#{ <<"language">> => NewLangs }
    end;
maybe_set_langs(Props, NewLangs) ->
    Props#{ <<"language">> => NewLangs }.

%% @doc Remove everything from New that has the same value in Old.
diff(New, Old) ->
    maps:fold(
        fun(K, V, Acc) ->
            case maps:find(K, Old) of
                {ok, V} -> maps:remove(K, Acc);
                _ -> Acc
            end
        end,
        New,
        New).

set_if_normal_update(#rscupd{} = RscUpd, K, V, Props) ->
    set_if_normal_update_1(
        is_normal_update(RscUpd),
        K, V, Props).

set_if_normal_update_1(false, _K, _V, Props) ->
    Props;
set_if_normal_update_1(true, K, V, Props) ->
    Props#{ K => V }.

is_normal_update(#rscupd{is_import = true}) -> false;
is_normal_update(#rscupd{is_no_touch = true}) -> false;
is_normal_update(#rscupd{}) -> true.


%% @doc Check if the update will change the data in the database
-spec is_changed( m_rsc:props(), m_rsc:props() ) -> boolean().
is_changed(Current, Props) ->
    maps:fold(
        fun
            (_K, _V, true) ->
                true;
            (K, V, false) ->
                is_prop_changed(K, V, Current)
        end,
        false,
        Props).

is_prop_changed(<<"version">>, _V, _Current) -> false;
is_prop_changed(<<"modifier_id">>, _V, _Current) -> false;
is_prop_changed(<<"modified">>, _V, _Current) -> false;
is_prop_changed(<<"pivot_category_nr">>, _V, _Current) -> false;
is_prop_changed(Prop, Value, Current) ->
    not is_equal(Value, maps:get(Prop, Current, undefined)).

is_equal(A, A) -> true;
is_equal(_, undefined) -> false;
is_equal(undefined, _) -> false;
is_equal(A, B) -> z_utils:are_equal(A, B).


%% @doc Check if all props are acceptable. Examples are unique name, uri etc.
-spec preflight_check( insert_rsc | m_rsc:resource_id(), map(), z:context()) ->
              ok
            | {error, duplicate_name | duplicate_page_path | duplicate_uri | invalid_query}.
preflight_check(insert_rsc, Props, Context) ->
    preflight_check(-1, Props, Context);
preflight_check(Id, Props, Context) when is_integer(Id) ->
    lists:foldl(
        fun
            (_F, {error, _} = Error) ->
                Error;
            (F, ok) ->
                F(Id, Props, Context)
        end,
        ok,
        [
            fun preflight_check_name/3,
            fun preflight_check_page_path/3,
            fun preflight_check_uri/3,
            fun preflight_check_query/3
        ]).

preflight_check_name(Id, #{ <<"name">> := Name }, Context) when Name =/= undefined ->
    case z_db:q1("select count(*) from rsc where name = $1 and id <> $2", [Name, Id], Context) of
        0 ->
            ok;
        _N ->
            ?LOG_WARNING(#{
                text => <<"Trying to insert duplicate name">>,
                in => zotonic_core,
                name => Name,
                rsc_id => Id,
                result => error,
                reason => duplicate_name
            }),
            {error, duplicate_name}
    end;
preflight_check_name(_Id, _Props, _Context) ->
    ok.


preflight_check_page_path(Id, #{ <<"page_path">> := Path }, Context) when Path =/= undefined ->
    case z_db:q1("select count(*) from rsc where page_path = $1 and id <> $2", [Path, Id], Context) of
        0 ->
            ok;
        _N ->
            ?LOG_WARNING(#{
                text => <<"Trying to insert duplicate page_path">>,
                in => zotonic_core,
                result => error,
                reason => duplicate_page_path,
                rsc_id => Id,
                page_path => Path
            }),
            {error, duplicate_page_path}
    end;
preflight_check_page_path(_Id, _Props, _Context) ->
    ok.

preflight_check_uri(Id, #{ <<"uri">> := Uri }, Context) when Uri =/= undefined ->
    case z_db:q1("select count(*) from rsc where uri = $1 and id <> $2", [Uri, Id], Context) of
        0 ->
            ok;
        _N ->
            ?LOG_WARNING(#{
                text => <<"Trying to insert duplicate uri">>,
                in => zotonic_core,
                result => error,
                reason => duplicate_uri,
                rsc_id => Id,
                uri => Uri
            }),
            {error, duplicate_uri}
    end;
preflight_check_uri(_Id, _Props, _Context) ->
    ok.

preflight_check_query(Id, #{ <<"query">> := Query }, Context) when Query =/= undefined ->
    try
        SearchContext = z_context:new( Context ),
        search_query:search(z_search_props:from_text(z_html:unescape(Query)), SearchContext),
        ok
    catch
        _:Reason:Stack ->
            ?LOG_WARNING(#{
                in => zotonic_core,
                text => <<"Error in preflight test of query text">>,
                rsc_id => Id,
                result => error,
                reason => Reason,
                stack => Stack,
                query => Query
            }),
            {error, invalid_query}
    end;
preflight_check_query(_Id, _Props, _Context) ->
    ok.


throw_if_category_not_allowed(_Id, _SafeProps, false, _Context) ->
    ok;
throw_if_category_not_allowed(insert_rsc, SafeProps, _True, Context) ->
    case maps:get(<<"category_id">>, SafeProps, undefined) of
        CatId when ?is_empty(CatId) ->
            throw({error, nocategory});
        CatId ->
            throw_if_category_not_allowed_1(undefined, SafeProps, CatId, Context)
    end;
throw_if_category_not_allowed(Id, SafeProps, _True, Context) ->
    case maps:get(<<"category_id">>, SafeProps, undefined) of
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
    maps:map(
        fun(K, V) ->
            case is_trimmable(K, V) of
                true -> z_string:trim(V);
                false -> V
            end
        end,
        Props).


%% @doc Remove properties the user is not allowed to change and convert some other to the correct data type
props_filter(Props, Context) ->
    maps:fold(
        fun(K, V, Acc) ->
            props_filter(K, V, Acc, Context)
        end,
        #{},
        Props).

props_filter(<<"uri">>, Uri, Acc, _Context) when ?is_empty(Uri) ->
    Acc#{ <<"uri">> => undefined };
props_filter(<<"uri">>, Uri, Acc, _Context) ->
    case z_sanitize:uri(Uri) of
        <<"#script-removed">> ->
            Acc#{ <<"uri">> => undefined };
        CleanUri ->
            Acc#{ <<"uri">> => CleanUri }
    end;
props_filter(<<"name">>, Name, Acc, Context) ->
    case z_acl:is_allowed(use, mod_admin, Context) of
        true ->
            case z_utils:is_empty(Name) of
                true ->
                    Acc#{ <<"name">> => undefined };
                false ->
                    Name1 = case z_string:to_name(Name) of
                        <<"_">> -> undefined;
                        N -> N
                    end,
                    Acc#{ <<"name">> => Name1 }
            end;
        false ->
            Acc
    end;
props_filter(<<"page_path">>, Path, Acc, Context) ->
    case z_acl:is_allowed(use, mod_admin, Context) of
        true when ?is_empty(Path) ->
            Acc#{ <<"page_path">> => undefined };
        true ->
            P = iolist_to_binary([
                $/, z_string:trim(z_url:url_path_encode(Path), $/)
            ]),
            Acc#{ <<"page_path">> => P };
        false ->
            Acc
    end;
props_filter(<<"title_slug">>, Slug, Acc, _Context) when ?is_empty(Slug) ->
    Acc;
props_filter(<<"title_slug">>, Slug, Acc, Context) ->
    Slug1 = to_slug(Slug),
    SlugNoTr = z_trans:lookup_fallback(Slug1, en, Context),
    Acc#{
        <<"slug">> => z_convert:to_binary(SlugNoTr),
        <<"title_slug">> => Slug1
    };
props_filter(<<"slug">>, Slug, Acc, _Context) ->
    case maps:is_key(<<"slug">>, Acc) of
        true ->
            Acc;
        false ->
            Acc#{
                <<"slug">> => to_slug(Slug)
            }
    end;
props_filter(<<"is_", _/binary>> = B, P, Acc, _Context) ->
    Acc#{ B => z_convert:to_bool(P) };
props_filter(<<"visible_for">> = B, P, Acc, _Context) ->
    Acc#{ B => z_convert:to_integer(P) };
props_filter(<<"custom_slug">> = B, P, Acc, _Context) ->
    Acc#{ B => z_convert:to_bool(P) };
props_filter(<<"date_is_all_day">> = B, P, Acc, _Context) ->
    Acc#{ B => z_convert:to_bool(P) };
props_filter(<<"seo_noindex">> = B, P, Acc, _Context) ->
    Acc#{ B => z_convert:to_bool(P) };
props_filter(P, DT, Acc, _Context)
    when P =:= <<"created">>;           P =:= <<"modified">>;
         P =:= <<"date_start">>;        P =:= <<"date_end">>;
         P =:= <<"publication_start">>; P =:= <<"publication_end">>  ->
    DateTime = case z_datetime:to_datetime(DT) of
        undefined when P =:= <<"publication_end">> ->
            ?ST_JUTTEMIS;
        DT1 ->
            DT1
    end,
    Acc#{
        P => DateTime
    };
props_filter(P, Id, Acc, Context)
    when P =:= <<"creator_id">>;
         P =:= <<"modifier_id">> ->
    case m_rsc:rid(Id, Context) of
        undefined ->
            Acc;
        RId ->
            Acc#{ P => RId }
    end;
props_filter(<<"category">>, CatName, Acc, Context) ->
    {ok, CategoryId} = m_category:name_to_id(CatName, Context),
    Acc#{ <<"category_id">> => CategoryId };
props_filter(<<"category_id">>, CatId, Acc, Context) ->
    CatId1 = m_rsc:rid(CatId, Context),
    case m_rsc:is_a(CatId1, category, Context) of
        true ->
            Acc#{ <<"category_id">> => CatId1 };
        false ->
            ?LOG_WARNING(#{
                text => <<"Ignoring unknown category in update, using 'other' instead.">>,
                in => zotonic_core,
                category_id => CatId
            }),
            {ok, OtherId} = m_category:name_to_id(other, Context),
            Acc#{ <<"category_id">> => OtherId }
    end;
props_filter(<<"content_group">>, CG, Acc, _Context) when ?is_empty(CG) ->
    Acc#{ <<"content_group_id">> => undefined };
props_filter(<<"content_group">>, CgName, Acc, Context) ->
    props_filter(<<"content_group_id">>, CgName, Acc, Context);
props_filter(<<"content_group_id">>, CgId, Acc, _Context) when ?is_empty(CgId) ->
    Acc#{ <<"content_group_id">> => undefined };
props_filter(<<"content_group_id">>, CgId, Acc, Context) ->
    CgId1 = m_rsc:rid(CgId, Context),
    case m_rsc:is_a(CgId1, content_group, Context)
        orelse m_rsc:is_a(CgId1, acl_collaboration_group, Context)
    of
        true ->
            Acc#{ <<"content_group_id">> => CgId1 };
        false ->
            ?LOG_WARNING(#{
                text => <<"Ignoring unknown content group">>,
                in => zotonic_core,
                content_group_id => CgId
            }),
            Acc
    end;
props_filter(Location, P, Acc, _Context)
    when Location =:= <<"location_lat">>;
         Location =:= <<"location_lng">> ->
    X = try
            z_convert:to_float(P)
        catch
            _:_ -> undefined
        end,
    Acc#{ Location => X };
props_filter(<<"pref_language">>, Lang, Acc, _Context) ->
    Lang1 = case z_language:to_language_atom(Lang) of
        {ok, LangAtom} -> LangAtom;
        {error, not_a_language} -> undefined
    end,
    Acc#{ <<"pref_language">> => Lang1 };
props_filter(<<"medium_language">>, Lang, Acc, _Context) ->
    Lang1 = case z_language:to_language_atom(Lang) of
        {ok, LangAtom} -> LangAtom;
        {error, not_a_language} -> undefined
    end,
    Acc#{ <<"medium_language">> => Lang1 };
props_filter(<<"language">>, Langs, Acc, _Context) ->
    Acc#{ <<"language">> => filter_languages(Langs) };
props_filter(<<"crop_center">>, CropCenter, Acc, _Context) when ?is_empty(CropCenter) ->
    Acc#{ <<"crop_center">> => undefined };
props_filter(<<"crop_center">>, CropCenter, Acc, _Context) ->
    CropCenter1 = case z_string:trim(CropCenter) of
        <<>> -> undefined;
        Trimmed ->
            case re:run(Trimmed, "^\\+[0-9]+\\+[0-9]+$") of
                nomatch -> undefined;
                {match, _} -> Trimmed
            end
    end,
    Acc#{ <<"crop_center">> => CropCenter1 };
props_filter(<<"privacy">>, Privacy, Acc, _Context) when ?is_empty(Privacy) ->
    Acc#{ <<"privacy">> => undefined };
props_filter(<<"privacy">>, Privacy, Acc, _Context) ->
    P = try
            z_convert:to_integer(Privacy)
        catch
            _:_ -> undefined
        end,
    Acc#{ <<"privacy">> => P };
props_filter(P, V, Acc, _Context) ->
    Acc#{ P => V }.


%% Filter all given languages, drop unknown languages.
%% Ensure that the languages are a list of atoms.
filter_languages([]) -> [];
filter_languages(<<>>) -> [];
filter_languages(Lang) when is_binary(Lang); is_atom(Lang) ->
    filter_languages([Lang]);
filter_languages([C | _] = Lang) when is_integer(C) ->
    filter_languages([Lang]);
filter_languages([L | _] = Langs) when is_list(L); is_binary(L); is_atom(L) ->
    Langs1 = lists:foldl(
        fun(Lang, Acc) ->
            case z_language:to_language_atom(Lang) of
                {ok, LangAtom} -> [LangAtom | Acc];
                {error, not_a_language} -> Acc
            end
        end,
        [],
        Langs),
    lists:sort(Langs1).

%% @doc If title is updating, check the rsc 'custom_slug' field to see if we need to update the slug or not.
generate_slug(Id, Props, Context) ->
    case maps:find(<<"title">>, Props) of
         {ok, Title} ->
            case {maps:get(<<"custom_slug">>, Props, false), m_rsc:p(Id, <<"custom_slug">>, Context)} of
                 {true, _} -> Props;
                 {_, true} -> Props;
                 _X ->
                    %% Determine the slug from the title.
                    Slug = to_slug(Title),
                    SlugNoTr = z_trans:lookup_fallback(Slug, en, Context),
                    Props#{
                        <<"slug">> => z_convert:to_binary(SlugNoTr),
                        <<"title_slug">> => Slug
                    }
            end;
         error ->
            Props
    end.


%% @doc Fill in some defaults for empty props on insert.
props_defaults(Props, Context) ->
    % Generate slug from the title (when there is a title)
    Props1 = case maps:find(<<"title_slug">>, Props) of
        error ->
            case maps:find(<<"title">>, Props) of
                {ok, Title} ->
                    Slug = to_slug(Title),
                    SlugNoTr = z_trans:lookup_fallback(Slug, en, Context),
                    Props#{
                        <<"slug">> => z_convert:to_binary(SlugNoTr),
                        <<"title_slug">> => Slug
                    };
                error ->
                    Props
            end;
        {ok, undefined} ->
            Props#{
                <<"title_slug">> => <<"-">>
            };
        {ok, _} ->
            Props
    end,
    % Assume content is authoritative, unless stated otherwise
    case maps:get(<<"is_authoritative">>, Props1, undefined) of
        undefined -> Props1#{ <<"is_authoritative">> => true };
        _ -> Props
    end.

props_filter_protected(Props, RscUpd) ->
    IsNormalUpdate = is_normal_update(RscUpd),
    maps:filter(
        fun (K, _) -> not is_protected(K, IsNormalUpdate) end,
        Props).


to_slug(undefined) ->
    <<>>;
to_slug(#trans{ tr = Tr }) ->
    Tr1 = lists:map(
        fun({Lang, V}) -> {Lang, to_slug(V)} end,
        Tr),
    #trans{ tr = Tr1 };
to_slug(B) when is_binary(B) ->
    B1 = z_string:to_lower(z_html:unescape(B)),
    truncate_slug(slugify(B1, false, <<>>));
to_slug(X) ->
    to_slug(z_convert:to_binary(X)).

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


%% @doc Properties that can't be updated with m_rsc_update:update/3 or m_rsc_update:insert/2
is_protected(<<"id">>, _IsNormal) -> true;
is_protected(<<"created">>, true) -> true;
is_protected(<<"creator_id">>, true) -> true;
is_protected(<<"modified">>, true) -> true;
is_protected(<<"modifier_id">>, true) -> true;
is_protected(<<"props">>, _IsNormal) -> true;
is_protected(<<"version">>, _IsNormal) -> true;
is_protected(<<"short_url">>, _IsNormal) -> true;
is_protected(<<"page_url">>, _IsNormal) -> true;
is_protected(<<"page_url_abs">>, _IsNormal) -> true;
is_protected(<<"alternate_page_url">>, _IsNormal) -> true;
is_protected(<<"alternate_page_url_abs">>, _IsNormal) -> true;
is_protected(<<"medium">>, _IsNormal) -> true;
is_protected(<<"pivot_", _binary>>, _IsNormal) -> true;
is_protected(<<"computed_", _/binary>>, _IsNormal) -> true;
is_protected(<<"*", _/binary>>, _IsNormal) -> true;
is_protected(_, _IsNormal) -> false.

is_trimmable(_, V) when not is_binary(V) -> false;
is_trimmable(<<"title">>, _) -> true;
is_trimmable(<<"short_title">>, _) -> true;
is_trimmable(<<"summary">>, _) -> true;
is_trimmable(<<"chapeau">>, _) -> true;
is_trimmable(<<"subtitle">>, _) -> true;
is_trimmable(<<"email">>, _) -> true;
is_trimmable(<<"uri">>, _) -> true;
is_trimmable(<<"website">>, _) -> true;
is_trimmable(<<"page_path">>, _) -> true;
is_trimmable(<<"name">>, _) -> true;
is_trimmable(<<"slug">>, _) -> true;
is_trimmable(<<"category">>, _) -> true;
is_trimmable(<<"rsc_id">>, _) -> true;
is_trimmable(_, _) -> false.


update_page_path_log(RscId, OldProps, NewProps, Context) ->
    Old = maps:get(<<"page_path">>, OldProps, undefined),
    New = maps:get(<<"page_path">>, NewProps, not_updated),
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
            z_db:q("DELETE FROM rsc_page_path_log WHERE page_path = $1 OR page_path = $2", [New, Old],
                Context),
            z_db:q("INSERT INTO rsc_page_path_log(id, page_path) VALUES ($1, $2)", [RscId, Old], Context),
            ok
    end.
