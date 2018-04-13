%% @copyright 2015-2017 Arjan Scherpenisse
%% @doc Adds content groups to enable access-control rules on resources.

%% Copyright 2015-2017 Arjan Scherpenisse
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

-module(mod_acl_user_groups).

-mod_title("ACL User Groups").
-mod_description("Organize users into hierarchical groups").
-mod_prio(400).
-mod_schema(9).
-mod_depends([menu, mod_content_groups]).
-mod_provides([acl]).

-behaviour(gen_server).

-include_lib("zotonic_core/include/zotonic.hrl").
-include("support/acl_user_groups.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").

% API
-export([
    status/1,
    table/1,
    table/2,
    await_table/1,
    await_table/2,
    await_table/3,
    lookup/2,
    await_lookup/2,
    rebuild/2,
    observe_admin_menu/3,
    observe_rsc_update_done/2,
    observe_rsc_delete/2,
    observe_rsc_insert/3,
    observe_rsc_get/3,
    name/1,
    manage_schema/2,
    manage_data/2
]).

% Access control hooks
-export([
    event/2,

    observe_acl_is_owner/2,
    observe_acl_is_allowed/2,
    observe_acl_is_allowed_prop/2,
    observe_acl_logon/2,
    observe_acl_logoff/2,
    observe_acl_context_authenticated/2,
    observe_acl_add_sql_check/2,

    observe_hierarchy_updated/2
]).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% gen_server state record
-record(state, {
            site,
            is_rebuild_publish=true,
            is_rebuild_edit=true,
            rebuilder_pid,
            rebuilder_mref,
            rebuilding,
            table_edit = [],
            table_publish = []
        }).


event(#submit{message={delete_move, Args}}, Context) ->
    ToUGId = z_convert:to_integer(z_context:get_q_validated(<<"acl_user_group_id">>, Context)),
    {id, Id} = proplists:lookup(id, Args),
    Ids = [ Id | m_hierarchy:children('acl_user_group', Id, Context) ],
    case deletable(Ids, Context) andalso z_acl:rsc_editable(ToUGId, Context) of
        true ->
            Context1 = z_context:prune_for_async(Context),
            spawn(fun() ->
                    ug_move_and_delete(Ids, ToUGId, Context1)
                  end),
            z_render:wire({dialog_close, []}, Context);
        false ->
            z_render:growl(?__("Sorry, you are not allowed to delete this.", Context), Context)
    end;
event(#postback{message={delete_all, Args}}, Context) ->
    {id, Id} = proplists:lookup(id, Args),
    IfEmpty = proplists:get_value(if_empty, Args, false),
    Ids = [ Id | m_hierarchy:children('acl_user_group', Id, Context) ],
    case not IfEmpty orelse not m_acl_user_group:is_used(Id, Context) of
        true ->
            case deletable(Ids, Context)  of
                true ->
                    Context1 = z_context:prune_for_async(Context),
                    spawn(fun() ->
                            ug_delete(Ids, Context1)
                          end),
                    z_render:wire({dialog_close, []}, Context);
                false ->
                    z_render:growl(?__("Sorry, you are not allowed to delete this.", Context), Context)
            end;
        false ->
            z_render:wire(
                {alert, [
                    {message, ?__("Delete is canceled, there are users in the user groups.", Context)}
                ]},
                Context
            )
    end.

-spec ug_delete(list(m_rsc:resource_id()), z:context()) -> any().
ug_delete(Ids, Context) ->
    z_session_page:add_script(
        z_render:wire({mask, [{message, ?__("Deleting...", Context)}]}, Context)
    ),
    UGUserIds = in_user_groups(Ids, Context),
    Total = lists:sum([length(UIds) || {_, UIds} <- UGUserIds]),
    case unlink_all(UGUserIds, 0, Total, Context) of
        ok ->
            lists:foreach(fun(Id) ->
                             m_rsc:delete(Id, Context)
                          end,
                          Ids),
            z_session_page:add_script(z_render:wire({unmask, []}, Context));
        {error, _} ->
            Context1 = z_render:wire([
                    {unmask, []},
                    {alert, [{message, ?__("Not all user groups could be deleted.", Context)}]}
                ],
                Context),
            z_session_page:add_script(Context1)

    end.

-spec ug_move_and_delete([pos_integer()], m_rsc:resource_id(), #context{}) -> ok.
ug_move_and_delete(Ids, ToGroupId, Context) ->
    z_session_page:add_script(z_render:wire({mask, [{message, ?__("Deleting...", Context)}]}, Context)),
    UGUserIds = in_user_groups(Ids, Context),
    Total = lists:sum([length(UIds) || {_, UIds} <- UGUserIds]),
    ok = move_all(UGUserIds, ToGroupId, 0, Total+Total, Context),
    lists:foreach(fun(Id) ->
                     m_rsc:delete(Id, Context)
                  end,
                  Ids),
    z_session_page:add_script(z_render:wire({unmask, []}, Context)),
    ok.

in_user_groups(Ids, Context) ->
    lists:flatten([ {Id, m_edge:subjects(Id, hasusergroup, Context)} || Id <- Ids ]).

unlink_all([], _N, _Total, _Context) ->
    ok;
unlink_all([{UGId, UserIds}|Ids], N, Total, Context) ->
    case unlink_users(UGId, UserIds, N, Total, Context) of
        {ok, N1} ->
            unlink_all(Ids, N1, Total, Context);
        Error ->
            {error, Error}
    end.

unlink_users(_UGId, [], N, _Total, _Context) ->
    {ok, N};
unlink_users(UGId, [UserId|UserIds], N, Total, Context) ->
    case m_edge:delete(UserId, hasusergroup, UGId, [], Context) of
        ok ->
            maybe_progress(N, N+1, Total, Context),
            unlink_users(UGId, UserIds, N+1, Total, Context);
        {error, _} = Error ->
            Error
    end.

move_all([], _ToUGId, _N, _Total, _Context) ->
    ok;
move_all([{UGId, UserIds}|Ids], ToUGId, N, Total, Context) ->
    case move_link_users(UGId, ToUGId, UserIds, N, Total, Context) of
        {ok, N1} ->
            move_all(Ids, ToUGId, N1, Total, Context);
        {error, _} = Error ->
            Error
    end.

move_link_users(_OldUGId, _UGId, [], N, _Total, _Context) ->
    {ok, N};
move_link_users(OldUGId, UGId, [UserId|UserIds], N, Total, Context) ->
    case m_edge:insert(UserId, hasusergroup, UGId, [], Context) of
        {ok, _} ->
            case m_edge:delete(UserId, hasusergroup, OldUGId, [], Context) of
                ok ->
                    maybe_progress(N, N+1, Total, Context),
                    move_link_users(OldUGId, UGId, UserIds, N+1, Total, Context);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

maybe_progress(_N1, _N2, 0, _Context) ->
    ok;
maybe_progress(N1, N2, Total, Context) ->
    z_pivot_rsc:pivot_delay(Context),
    PerStep = Total / 100,
    S1 = round(N1 / PerStep),
    S2 = round(N2 / PerStep),
    case S1 of
        S2 -> ok;
        _ -> z_session_page:add_script(z_render:wire({mask_progress, [{percent,S2}]}, Context))
    end.

deletable(Ids, Context) ->
    lists:all(fun(Id) -> z_acl:rsc_deletable(Id, Context) end, Ids).


% @doc Per default users own their person record and creators own the created content.
observe_acl_is_owner(#acl_is_owner{id=Id, user_id=Id}, _Context) -> true;
observe_acl_is_owner(#acl_is_owner{user_id=UserId, creator_id=UserId}, _Context) -> true;
observe_acl_is_owner(#acl_is_owner{}, _Context) -> undefined.

observe_acl_is_allowed(AclIsAllowed, Context) ->
    acl_user_groups_checks:acl_is_allowed(AclIsAllowed, Context).

observe_acl_is_allowed_prop(#acl_is_allowed_prop{action=view, object=undefined}, _Context) ->
    true;
observe_acl_is_allowed_prop(#acl_is_allowed_prop{action=view, object=Id, prop=Property}, #context{} = Context) when is_integer(Id) ->
    acl_user_groups_checks:acl_is_allowed_prop(Id, Property, Context);
observe_acl_is_allowed_prop(#acl_is_allowed_prop{}, #context{user_id=undefined}) ->
    undefined.

observe_acl_logon(AclLogon, Context) ->
    acl_user_groups_checks:acl_logon(AclLogon, Context).

observe_acl_logoff(AclLogoff, Context) ->
    acl_user_groups_checks:acl_logoff(AclLogoff, Context).

observe_acl_context_authenticated(_AclAuthenticated, Context) ->
    acl_user_groups_checks:acl_context_authenticated(Context).

observe_acl_add_sql_check(AclAddSQLCheck, Context) ->
    acl_user_groups_checks:acl_add_sql_check(AclAddSQLCheck, Context).

observe_hierarchy_updated(#hierarchy_updated{root_id= <<"$category">>, predicate=undefined}, Context) ->
    rebuild(Context);
observe_hierarchy_updated(#hierarchy_updated{root_id= <<"content_group">>, predicate=undefined}, Context) ->
    rebuild(Context);
observe_hierarchy_updated(#hierarchy_updated{root_id= <<"acl_user_group">>, predicate=undefined}, Context) ->
    rebuild(Context);
observe_hierarchy_updated(#hierarchy_updated{}, _Context) ->
    ok.

%% @doc Add default content group when resource is inserted without one
-spec observe_rsc_insert(#rsc_insert{}, list(), #context{}) -> list().
observe_rsc_insert(#rsc_insert{props=RscProps}, InsertProps, Context) ->
    case proplists:get_value(content_group_id, RscProps,
            proplists:get_value(content_group_id, InsertProps))
    of
        undefined ->
            CategoryId = proplists:get_value(category_id, InsertProps),
            ContentGroupId = acl_user_groups_checks:default_content_group(CategoryId, Context),
            [{content_group_id, ContentGroupId} | InsertProps];
        _ ->
            InsertProps
    end.

observe_rsc_update_done(#rsc_update_done{id=Id, pre_is_a=PreIsA, post_is_a=PostIsA}=M, Context) ->
    check_hasusergroup(Id, M#rsc_update_done.post_props, Context),
    case  lists:member('acl_user_group', PreIsA)
        orelse lists:member('acl_user_group', PostIsA)
    of
        true -> m_hierarchy:ensure('acl_user_group', Context);
        false -> ok
    end.

%% @doc Do now allow the deletion of a acl_user_group if that group is still used.
observe_rsc_delete(#rsc_delete{id=Id, is_a=IsA}, Context) ->
    case lists:member('acl_user_group', IsA) of
        true ->
            case m_acl_user_group:is_used(Id, Context) of
                true -> throw({error, is_used});
                false -> ok
            end;
        false ->
            ok
    end.

%% @doc Ensure that the privacy property is set.
observe_rsc_get(#rsc_get{}, [], _Context) ->
    [];
observe_rsc_get(#rsc_get{}, Props, Context) ->
    case proplists:get_value(privacy, Props) of
        undefined ->
            [
                {privacy,
                    case m_category:is_a_prim(proplists:get_value(category_id, Props), person, Context) of
                        true -> ?ACL_PRIVACY_COLLAB_MEMBER;
                        false -> ?ACL_PRIVACY_PUBLIC
                    end}
                | proplists:delete(privacy, Props)
            ];
        _ ->
            Props
    end.


status(Context) ->
    gen_server:call(name(Context), status).

rebuild(Context) ->
    rebuild(publish, Context),
    rebuild(edit, Context).

rebuild(edit, Context) ->
    gen_server:cast(name(Context), rebuild_edit);
rebuild(publish, Context) ->
    gen_server:cast(name(Context), rebuild_publish).

-spec table(#context{}) -> ets:tab() | undefined.
table(Context) ->
    table(acl_user_groups_checks:state(Context), Context).

-spec await_table(#context{}) -> ets:tab() | undefined.
await_table(Context) ->
    await_table(acl_user_groups_checks:state(Context), Context).


-spec table(edit|publish, #context{}) -> ets:tab() | undefined.
table(State, Context) when State =:= edit; State =:= publish ->
    try
        gproc:get_value_shared({p,l,{z_context:site(Context), ?MODULE, State}})
    catch
        error:badarg ->
            undefined
    end.

-spec await_table(edit|publish, #context{}) -> ets:tab() | undefined.
await_table(State, Context) ->
    await_table(State, infinity, Context).

-spec await_table(edit|publish, integer()|infinity, #context{}) -> ets:tab() | undefined.
await_table(State, infinity, Context) ->
    case table(State, Context) of
        undefined ->
            timer:sleep(100),
            await_table(State, infinity, Context);
        TId ->
            TId
    end;
await_table(State, Timeout, Context) when Timeout > 0 ->
    case table(State, Context) of
        undefined ->
            timer:sleep(10),
            await_table(State, Timeout-10, Context);
        TId ->
            TId
    end.

lookup(Key, Context) ->
    lookup1(table(Context), Key).

await_lookup(Key, Context) ->
    lookup1(await_table(Context), Key).

lookup1(undefined, _Key) ->
    undefined;
lookup1(TId, Key) ->
    case ets:lookup(TId, Key) of
        [] -> undefined;
        [{_,V}|_] -> V
    end.


observe_admin_menu(#admin_menu{}, Acc, Context) ->
    [
     #menu_item{id=admin_acl_user_groups,
                parent=admin_auth,
                label=?__("User groups", Context),
                url={admin_menu_hierarchy, [{name, "acl_user_group"}]},
                visiblecheck={acl, use, mod_acl_user_groups}},
     #menu_item{id=admin_collaboration_groups,
                parent=admin_auth,
                label=?__("Collaboration groups", Context),
                url={admin_overview_rsc, [{qcat, "acl_collaboration_group"}]}},
     #menu_item{id=admin_content_groups,
                parent=admin_auth,
                label=?__("Access control rules", Context),
                url={admin_acl_rules_rsc, []},
                visiblecheck={acl, use, mod_acl_user_groups}}
     |Acc].


name(Context) ->
    z_utils:name_for_site(?MODULE, Context).

%%====================================================================
%% API
%%====================================================================
%% @spec start_link(Args) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(Args) when is_list(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    gen_server:start_link({local, name(Context)}, ?MODULE, Args, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Args) ->
    process_flag(trap_exit, true),
    {context, Context} = proplists:lookup(context, Args),
    Site = z_context:site(Context),
    lager:md([
        {site, Site},
        {module, ?MODULE}
      ]),
    timer:send_after(10, rebuild),
    {ok, #state{ site=Site, is_rebuild_publish=true, is_rebuild_edit=true}}.

handle_call(status, _From, State) ->
    Reply = {ok, [
        {is_rebuilding, is_pid(State#state.rebuilder_pid)},
        {rebuilding, State#state.rebuilding},
        {is_rebuild_publish, State#state.is_rebuild_publish},
        {is_rebuild_edit, State#state.is_rebuild_edit}
    ]},
    {reply, Reply, State};
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

handle_cast(rebuild_publish, State) ->
    timer:send_after(100, rebuild),
    {noreply, State#state{is_rebuild_publish=true}};
handle_cast(rebuild_edit, State) ->
    timer:send_after(750, rebuild),
    {noreply, State#state{is_rebuild_edit=true}};
handle_cast(rebuild, State) ->
    handle_info(rebuild, State);
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.

handle_info(rebuild, #state{rebuilder_pid=undefined} = State) ->
    State1 = maybe_rebuild(State),
    {noreply, State1};
handle_info(rebuild, #state{rebuilder_pid=Pid} = State) when is_pid(Pid) ->
    {noreply, State};

handle_info({'DOWN', MRef, process, _Pid, normal}, #state{rebuilder_mref = MRef, site = Site} = State) ->
    lager:debug("[mod_acl_user_groups] rebuilder for ~p finished.",
                [State#state.rebuilding]),
    State1 = State#state{
                    rebuilding=undefined,
                    rebuilder_pid=undefined,
                    rebuilder_mref=undefined
                },
    State2 = maybe_rebuild(State1),
    Context = z_context:new(Site),
    z_mqtt:publish(
        <<"acl-rules/", (z_convert:to_binary(State#state.rebuilding))/binary, "-rebuild">>,
        true,
        z_acl:sudo(Context)
    ),
    {noreply, State2};

handle_info({'DOWN', MRef, process, _Pid, Reason}, #state{rebuilder_mref=MRef} = State) ->
    lager:error("[mod_acl_user_groups] rebuilder for ~p down with reason ~p",
                [State#state.rebuilding, Reason]),
    State1 = case State#state.rebuilding of
                publish -> State#state{is_rebuild_publish=true};
                edit -> State#state{is_rebuild_edit=true}
             end,
    timer:send_after(500, rebuild),
    {noreply, State1#state{
                    rebuilding=undefined,
                    rebuilder_pid=undefined,
                    rebuilder_mref=undefined
                }};

handle_info({'ETS-TRANSFER', TId, _FromPid, publish}, State) ->
    lager:debug("[mod_acl_user_groups] 'ETS-TRANSFER' for 'publish' (~p)", [TId]),
    gproc_new_ets(TId, publish, State#state.site),
    State1 = store_new_ets(TId, publish, State),
    z_mqtt:publish(<<"acl-rules/publish">>, true, z_context:new(State#state.site)),
    {noreply, State1};
handle_info({'ETS-TRANSFER', TId, _FromPid, edit}, State) ->
    lager:debug("[mod_acl_user_groups] 'ETS-TRANSFER' for 'edit' (~p)", [TId]),
    gproc_new_ets(TId, edit, State#state.site),
    State1 = store_new_ets(TId, edit, State),
    z_mqtt:publish(<<"acl-rules/edit">>, true, z_context:new(State#state.site)),
    {noreply, State1};

handle_info({'EXIT', _Pid, normal}, State) ->
    {noreply, State};

handle_info(Info, State) ->
    lager:warning("[mod_acl_user_groups] unknown info message ~p", [Info]),
    {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, _State) ->
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Check if we need to start a rebuild process
maybe_rebuild(#state{is_rebuild_publish=true} = State) ->
    {Pid, MRef} = start_rebuilder(publish, State#state.site),
    State#state{
        is_rebuild_publish=false,
        rebuilder_pid=Pid,
        rebuilding=publish,
        rebuilder_mref=MRef
    };
maybe_rebuild(#state{is_rebuild_edit=true} = State) ->
    {Pid, MRef} = start_rebuilder(edit, State#state.site),
    State#state{
        is_rebuild_edit=false,
        rebuilder_pid=Pid,
        rebuilding=edit,
        rebuilder_mref=MRef
    };
maybe_rebuild(#state{} = State) ->
    State.


start_rebuilder(EditState, Site) ->
    Self = self(),
    Pid = erlang:spawn_link(fun() ->
                                Context = z_acl:sudo(z_context:new(Site)),
                                acl_user_group_rebuilder:rebuild(Self, EditState, Context)
                            end),
    MRef = erlang:monitor(process, Pid),
    {Pid, MRef}.

gproc_new_ets(TId, EditState, Site) ->
    Key = {Site, ?MODULE, EditState},
    try
        gproc:unreg_shared({p,l,Key})
    catch
        error:badarg -> ok
    end,
    true = gproc:reg_shared({p,l,Key}, TId).

store_new_ets(TId, publish, #state{table_publish=Ts} = State) ->
    Ts1 = drop_old_ets(Ts),
    State#state{table_publish=[TId|Ts1]};
store_new_ets(TId, edit, #state{table_edit=Ts} = State) ->
    Ts1 = drop_old_ets(Ts),
    State#state{table_edit=[TId|Ts1]}.

drop_old_ets([A|Rest]) ->
    lists:foreach(fun(TId) ->
                    ets:delete(TId)
                  end,
                  Rest),
    [A];
drop_old_ets([]) ->
    [].

%%====================================================================
%% Manage Schema
%%====================================================================

manage_schema(Version, Context) ->
    m_acl_rule:manage_schema(Version, Context),
    #datamodel{
        categories = [
            {acl_user_group, meta, [
                    {title, {trans, [{en, <<"User Group">>}, {nl, <<"Gebruikersgroep">>}]}}
                ]},
            {acl_collaboration_group, meta, [
                    {title, {trans, [{en, <<"Collaboration Group">>}, {nl, <<"Samenwerkingsgroep">>}]}}
                ]}
        ],
        resources = [
            {acl_user_group_anonymous,
                acl_user_group,
                [{title, {trans, [{en, <<"Anonymous">>}, {nl, <<"Anoniem">>}]}}]},
            {acl_user_group_members,
                acl_user_group,
                [{title, {trans, [{en, <<"Members">>}, {nl, <<"Gebruikers">>}]}}]},
            {acl_user_group_editors,
                acl_user_group,
                [{title, {trans, [{en, <<"Editors">>}, {nl, <<"Redactie">>}]}}]},
            {acl_user_group_managers,
                acl_user_group,
                [{title, {trans, [{en, <<"Managers">>}, {nl, <<"Beheerders">>}]}}]}
        ],
        predicates = [
            {hasusergroup,
                [{title, {trans, [{en, <<"In User Group">>},{nl, <<"In gebruikersgroep">>}]}}],
                [{person, acl_user_group}]
            },
            {hascollabmember,
                [{title, {trans, [{en, <<"Member">>},{nl, <<"Lid">>}]}}],
                [{acl_collaboration_group, person}]
            },
            {hascollabmanager,
                [{title, {trans, [{en, <<"Manager">>},{nl, <<"Beheerder">>}]}}],
                [{acl_collaboration_group, person}]
            }
        ]
    }.

manage_data(install, Context) ->
    case m_hierarchy:tree(acl_user_group, Context) of
        [] ->
            R = fun(N) -> m_rsc:rid(N, Context) end,
            NewTree = [
                {R(acl_user_group_anonymous), [
                    {R(acl_user_group_members), [
                        {R(acl_user_group_editors), [
                            {R(acl_user_group_managers), []}
                        ]}
                    ]}
                ]}
            ],
            m_hierarchy:save(acl_user_group, NewTree, Context),
            Rules = acl_default_rules:get_default_rules(),
            m_acl_rule:replace_managed(Rules, ?MODULE, Context),
            ok;
        _ ->
            ok
    end;
manage_data(_Version, _Context) ->
    ok.


check_hasusergroup(UserId, P, Context) ->
    HasUserGroup = proplists:get_all_values(hasusergroup, P),
    case length(HasUserGroup) of
        0 ->
            %% not submitted, do nothing
            ok;
        _ ->
            GroupIds = lists:map(
                fun z_convert:to_integer/1,
                lists:filter(fun(<<>>) -> false; (_) -> true end, HasUserGroup)),
            {ok, PredId} = m_predicate:name_to_id(hasusergroup, Context),
            m_edge:replace(UserId, PredId, GroupIds, Context)
    end.

