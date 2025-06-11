%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2023 Marc Worrell
%% @doc Menu module. Supports menu trees in Zotonic. Adds admin interface to define the menu.
%% @end

%% Copyright 2009-2023 Marc Worrell
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

-module(mod_menu).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Menus").
-mod_description("Menus in Zotonic, adds admin interface to define menus and other hierarchical lists.").
-mod_schema(4).
-mod_depends([]).
-mod_provides([menu]).

%% interface functions
-export([
    manage_schema/2,
    manage_data/2,

    event/2,

    observe_menu_get_rsc_ids/2,
    observe_menu_save/2,
    observe_rsc_get/3,
    observe_admin_menu/3,
    observe_rsc_update/3,
    observe_rsc_update_done/2,

    get_menu/1,
    get_menu/2,
    set_menu/3,
    menu_flat/2,
    menu_flat/3,
    menu_subtree/3,
    menu_subtree/4,
    remove_invisible/2,

    ensure_hasmenupart/1,
    update_hasmenupart/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").


event(#postback_notify{message= <<"menuedit">>, trigger=TriggerId}, Context) ->
    {Kind, RootId, Predicate} = get_kind_root(TriggerId),
    Tree = unpack(z_context:get_q(<<"tree">>, Context)),
    {Tree1, Context1} = create_new(Tree, Context),
    case Kind of
        category ->
            % This is the category hierarchy.
            z_notifier:notify_sync(#category_hierarchy_save{tree=Tree1}, Context1),
            Context1;
        menu ->
            % A menu hierarchy, give it to the menu routines
            z_notifier:notify_sync(#menu_save{id=m_rsc:rid(RootId, Context1), tree=Tree1}, Context1),
            Context1;
        hierarchy ->
            _ = m_hierarchy:save(RootId, Tree1, Context),
            Context1;
        edge ->
            % Hierarchy using edges between resources
            hierarchy_edge(m_rsc:rid(RootId, Context1), Predicate, Tree1, Context1)
    end;
event(#z_msg_v1{data=Data}, Context) ->
    handle_cmd(proplists:get_value(<<"cmd">>, Data), Data, Context);
event(#postback{ message = {ensure_hasmenupart, []} }, Context) ->
    case z_acl:is_admin(Context) of
        true ->
            z_pivot_rsc:insert_task(?MODULE, ensure_hasmenupart, <<>>, Context),
            z_render:growl(?__("Checking all menu resources in the backgroud", Context), Context);
        false ->
            z_render:growl_error(?__("Sorry, only an admin is allowed to do this", Context), Context)
    end.


%% @doc Notifier handler to get all menu ids for the default menu.
observe_menu_get_rsc_ids(menu_get_rsc_ids, Context) ->
    Menu = get_menu(Context),
    {ok, menu_ids(Menu, [])}.

%% @doc Observer the 'menu_save' notification
observe_menu_save(#menu_save{id=MenuId, tree=Menu}, Context) ->
    set_menu(MenuId, Menu, Context).

observe_admin_menu(#admin_menu{}, Acc, Context) ->
    case m_rsc:name_to_id(main_menu, Context) of
        {ok, Id} ->
            [
             #menu_item{id=admin_menu,
                        parent=admin_content,
                        label=?__("Menu", Context),
                        url={admin_edit_rsc, [{id, Id}]},
                        visiblecheck={acl, use, mod_menu}}
             |Acc];
        _ ->
            Acc
    end.

%% @doc Ensure that menus in the old tuple format are translated to #rsc_tree
observe_rsc_get(#rsc_get{}, #{ <<"menu">> := [] } = R, _Context) ->
    R;
observe_rsc_get(#rsc_get{}, #{ <<"menu">> := [ #rsc_tree{} | _ ] } = R, _Context) ->
    R;
observe_rsc_get(#rsc_get{}, #{ <<"menu">> := Menu } = R, _Context) ->
    R#{
        <<"menu">> => validate(Menu, [])
    };
observe_rsc_get(#rsc_get{}, R, _Context) ->
    R.


observe_rsc_update(#rsc_update{ action = Action }, {ok, #{ <<"menu">> := Menu } = R}, _Context)
    when Action =:= update;
         Action =:= insert ->
    Menu1 = validate(Menu, []),
    {ok, R#{ <<"menu">> => Menu1 }};
observe_rsc_update(#rsc_update{}, Result, _Context) ->
    Result.


%% @doc Set the 'hasmenupart' edges to keep track which resourcees are used in a menu.
%% This is needed for the automatic cleanup of 'dependent' resources and the
%% breadcrumb paths of mod_seo.
observe_rsc_update_done(#rsc_update_done{ action = Action, id = Id, post_is_a = IsA }, Context)
    when Action =:= insert;
         Action =:= update ->
    case lists:member(menu, IsA) of
        true ->
            update_hasmenupart(Id, Context);
        false ->
            ok
    end;
observe_rsc_update_done(#rsc_update_done{}, _Context) ->
    ok.

%% @doc Ensure that all menu resources have the correct 'hasmenupart' edges.
-spec ensure_hasmenupart(Context) -> ok | {error, Reason} when
    Context :: z:context(),
    Reason :: term().
ensure_hasmenupart(Context) ->
    ?LOG_INFO(#{
        in => mod_menu,
        text => <<"Checking all menu resource for hasmenupart connections">>
    }),
    ContextSudo = z_acl:sudo(Context),
    m_category:foreach(
        menu,
        fun(Id, Ctx) ->
            update_hasmenupart(Id, Ctx)
        end,
        ContextSudo).

%% @doc Ensure that all hasmenupart connections of the resource are correct.
-spec update_hasmenupart(Id, Context) -> ok when
    Id :: m_rsc:resource_id(),
    Context :: z:context().
update_hasmenupart(Id, Context) ->
    ContextSudo = z_acl:sudo(Context),
    NewMenuIds = filter_menu_ids:menu_ids(Id, ContextSudo),
    OldMenuIds = m_edge:objects(Id, hasmenupart, ContextSudo),
    New = NewMenuIds -- OldMenuIds,
    Del = OldMenuIds -- NewMenuIds,
    NewExists = lists:filter(fun(ObjId) -> m_rsc:exists(ObjId, ContextSudo) end, New),
    [ m_edge:insert(Id, hasmenupart, ObjId, [no_touch], ContextSudo) || ObjId <- NewExists ],
    [ m_edge:delete(Id, hasmenupart, ObjId, [no_touch], ContextSudo) || ObjId <- Del ],
    ok.

% Event handler command handling.
handle_cmd(<<"copy">>, Data, Context) ->
    FromId = z_convert:to_integer(proplists:get_value(<<"id">>, Data)),
    case m_rsc:is_visible(FromId, Context) of
        true ->
            NewTitle = make_copy_title(m_rsc:p(FromId, title, Context), Context),
            NewProps = #{
                <<"title">> => z_html:unescape(NewTitle)
            },
            case m_rsc:duplicate(FromId, NewProps, Context) of
                {ok, NewId} ->
                    Template = case proplists:get_value(<<"template">>, Data) of
                        undefined -> "_menu_edit_item.tpl";
                        <<>> -> "_menu_edit_item.tpl";
                        Tpl -> z_convert:to_list(Tpl)
                    end,
                    {Html, Context1} = z_template:render_to_iolist({cat, Template}, [{id, NewId}, {editable, true}], Context),
                    z_render:wire({script, [{script, [
                                    <<"window.zMenuInsertAfter(\"">>,
                                            integer_to_list(FromId), $",$,,
                                            $",z_utils:js_escape(Html,Context1),$",
                                    $),$;
                                ]}]}, Context1);
                {error, _Reason} ->
                    z_render:growl_error(?__("Sorry, can’t copy that page.", Context), Context)
            end;
        false ->
            Context
    end;
handle_cmd(<<"menu-item-render">>, Data, Context) ->
    Id = z_convert:to_integer(proplists:get_value(<<"id">>, Data)),
    Callback = proplists:get_value(<<"callback">>, Data),
    Template = case proplists:get_value(<<"template">>, Data) of
        undefined -> "_menu_edit_item.tpl";
        <<>> -> "_menu_edit_item.tpl";
        Tpl -> z_convert:to_list(Tpl)
    end,
    {Html, Context2} = z_template:render_to_iolist({cat, Template}, [{id, Id}, {editable, true}], Context),
    z_render:wire({script, [{script, [
                    Callback, $(,
                        $", integer_to_list(Id), $",$,,
                        $",z_utils:js_escape(Html,Context2),$",
                    $),$;
                ]}]}, Context2);
handle_cmd(<<"delete">>, Data, Context) ->
    Id = z_convert:to_integer(proplists:get_value(<<"id">>, Data)),
    case m_rsc:is_deletable(Id, Context) of
        true ->
            z_render:wire({dialog_delete_rsc, [{id,Id}]}, Context);
        false ->
            z_render:wire(
                {alert, [{text, ?__("Sorry, you are not allowed to delete this.", Context)}]},
                Context)
    end.


make_copy_title(undefined, _Context) ->
    undefined;
make_copy_title(Title, Context) when is_binary(Title) ->
    iolist_to_binary([?__("Copy of:", Context), " ", Title]);
make_copy_title(#trans{ tr = Ts }, Context) ->
    #trans{ tr = [
        {Lang, make_copy_title(Title, z_context:set_language(Lang, Context))}
        || {Lang,Title} <- Ts
    ]}.



% @doc Sync a hierarchy based on edges (silently ignore ACL errors)
hierarchy_edge(RootId, Predicate, Tree, Context) ->
    Predicate = z_context:get_q(<<"z_predicate">>, Context, Predicate),
    {ok, PredId} = m_predicate:name_to_id(Predicate, Context),
    {ok, PredName} = m_predicate:id_to_name(PredId, Context),
    move_edges(RootId, Tree, PredName, Context),
    z_notifier:notify(#hierarchy_updated{root_id=RootId, predicate=PredName}, Context),
    Context.

move_edges(RootId, Tree, Pred, Context) ->
    Wanted = [ Id || {Id,_SubTree} <- Tree ],
    case z_acl:rsc_editable(RootId, Context) of
        true -> ok = m_edge:replace(RootId, Pred, Wanted, Context);
        false -> ignore
    end,
    [ move_edges(Id, SubTree, Pred, Context) || {Id, SubTree} <- Tree ].



%% @doc The id of the root ul should be one of:
%%      category
%%      menu-ID
%%      hierarchy-ROOTID
%%      edge-ROOTID
%%      edge-ROOTID-PREDICATE
%%      collection-ROOTID
get_kind_root(Root) when is_binary(Root) ->
    get_kind_root(z_convert:to_list(Root));
get_kind_root("category") ->
    {category, undefined, undefined};
get_kind_root("menu-"++MenuId) ->
    {menu, MenuId, undefined};
get_kind_root(TriggerId) ->
    case string:tokens(TriggerId, "-") of
        ["hierarchy" | Name] ->
            {hierarchy, string:join(Name, "-"), undefined};
        ["edge", RootId, Predicate] ->
            {edge, RootId, Predicate};
        ["collection", RootId] ->
            {edge, RootId, haspart};
        _ ->
            {edge, TriggerId, haspart}
    end.

unpack(S) when is_binary(S) ->
    unpack(binary_to_list(S));
unpack(S) ->
    {[], Tree} = unpack(S, []),
    Tree.

unpack([$[|S], Acc) ->
    unpack(S, Acc);
unpack([$]|S], Acc) ->
    {S, lists:reverse(Acc)};
unpack([$,|S], Acc) ->
    unpack(S, Acc);
unpack(S, Acc) ->
    {Id,S1} = lists:splitwith(fun(C) when C==$[; C==$]; C==$, -> false; (_) -> true end, S),
    {Rest,Sub} = case S1 of
                    [$[|Xs] -> unpack(Xs, []);
                    _ -> {S1,[]}
                 end,
    Acc1 = [{map_id(Id),Sub}|Acc],
    unpack(Rest,Acc1).

map_id(Id) when is_list(Id) ->
    Id1 = lists:last(string:tokens(Id, "-")),
    case z_utils:only_digits(Id1) of
        true -> z_convert:to_integer(Id1);
        false -> Id
    end.


create_new([], Context) ->
    {[], Context};
create_new(List, Context) ->
    create_new(List, [], Context).


create_new([], Acc, Context) ->
    {lists:reverse(Acc), Context};
create_new([{Id,Sub}|Rest], Acc, Context) ->
    {Id1, Context1} = create_new_if_needed(Id, Context),
    {Sub1, Context2} = create_new(Sub, Context1),
    create_new(Rest, [{Id1,Sub1}|Acc], Context2).


create_new_if_needed(Id, Context) when is_integer(Id) ->
    {Id, Context};
create_new_if_needed(Id, Context) ->
    C = lists:last( string:tokens(Id, "-") ),
    Category = case m_category:name_to_id(C, Context) of
        {error, _} -> text;
        {ok, C1} -> C1
    end,
    Props = #{
        <<"is_published">> => false,
        <<"category_id">> => Category,
        <<"title">> => ?__("New page", Context)
    },
    {ok, RscId} = m_rsc:insert(Props, Context),
    OuterId = iolist_to_binary(["outernew-",integer_to_list(RscId)]),
    InnerId = iolist_to_binary(["innernew-",integer_to_list(RscId)]),
    Context1 = z_render:wire(
        [
            {script, [{script, ["$('#",Id,"').attr('id','",OuterId,"');"]}]},
            {dialog_edit_basics, [
                        {id, RscId},
                        {target, InnerId},
                        {template, "_menu_edit_item.tpl"}
                ]},
            {update, [
                        {target, OuterId},
                        {menu_id, InnerId},
                        {id, RscId},
                        {template, "_menu_edit_item.tpl"}
                ]}
        ], Context),
    {RscId, Context1}.

menu_ids([], Acc) ->
    Acc;
menu_ids([ #rsc_tree{ id = Id, tree = SubMenu } | T ], Acc) ->
    Acc1 = menu_ids(SubMenu, [Id|Acc]),
    menu_ids(T, Acc1).

%% @doc Fetch the default menu. Performs validation/visibility checking on the menu items.
get_menu(Context) ->
    get_menu(m_rsc:rid(main_menu, Context), Context).

%% @doc Fetch a menu structure from a rsc. Performs validation/visibility checking on the menu items.
get_menu(Id, Context) ->
    case m_rsc:p(Id, menu, Context) of
        undefined -> [];
        <<>> -> [];
        Menu -> remove_invisible(validate(Menu, []), [], Context)
    end.

validate(undefined, Acc) ->
    lists:reverse(Acc);
validate([], Acc) ->
	lists:reverse(Acc);
validate(<<>>, Acc) ->
    lists:reverse(Acc);
validate([ #rsc_tree{ id = Id, tree = Sub } = M | Ms ], Acc)
    when is_integer(Id);
         is_atom(Id);
         is_binary(Id) ->
    Sub1 = validate(Sub, []),
    validate(Ms, [ M#rsc_tree{ tree = Sub1 } | Acc ]);
validate([ #rsc_tree{} | Ms ], Acc) ->
    % Drop entries with invalid ids
    validate(Ms, Acc);
validate([ {Id,L} | Ms ], Acc) ->
	validate(Ms, [ #rsc_tree{ id = Id, tree = validate(L, []) } | Acc ]);
validate([ M | Ms ], Acc) ->
	validate(Ms, [ #rsc_tree{ id = M, tree = [] } | Acc ]).


remove_invisible(Menu, Context) ->
    remove_invisible(Menu, [], Context).

%% Remove invisible menu items
remove_invisible(undefined, Acc, _Context) ->
    lists:reverse(Acc);
remove_invisible(<<>>, Acc, _Context) ->
    lists:reverse(Acc);
remove_invisible([], Acc, _Context) ->
    lists:reverse(Acc);
remove_invisible([ #rsc_tree{ id = Id, tree = Sub } | Rest ], Acc, Context) ->
    case m_rsc:is_visible(Id, Context) andalso m_rsc:exists(Id, Context) of
        true ->  remove_invisible(Rest, [ #rsc_tree{ id = Id, tree = remove_invisible(Sub, [], Context)} | Acc ], Context);
        false -> remove_invisible(Rest, Acc, Context)
    end;
% Old notations
remove_invisible([{Id,Sub}|Rest], Acc, Context) ->
    remove_invisible([ #rsc_tree{ id = Id, tree = Sub } | Rest ], Acc, Context);
remove_invisible([Id|Rest], Acc, Context) ->
    remove_invisible([ #rsc_tree{ id = Id, tree = [] } | Rest ], Acc, Context).


%% @doc Save the current menu to a resource.
set_menu(Id, Menu, Context) ->
    Menu1 = validate(Menu, []),
    m_rsc:update(Id, #{ <<"menu">> => Menu1 }, Context).


%% @doc Flatten the menu structure in a list, used for display purposes in templates
-spec menu_flat(list() | undefined | <<>>, z:context()) -> [ {integer()|undefined, LevelIndex::list(integer()) | undefined, up|down|undefined} ].
menu_flat(Menu, Context) ->
    menu_flat(Menu, 999, Context).

-spec menu_flat(list() | undefined | <<>>, integer(), z:context()) -> [ {integer()|undefined, LevelIndex::list(integer()) | undefined, up|down|undefined} ].
menu_flat(Menu, _MaxDepth, _Context) when not is_list(Menu) ->
    [];
menu_flat([], _MaxDepth, _Context) ->
    [];
menu_flat(X, MaxDepth, Context) ->
    menu_flat(X, MaxDepth, [1], [], Context).

menu_flat([], _MaxDepth, _Path, Acc, _Context) ->
    lists:reverse(Acc);
menu_flat([ #rsc_tree{ id = MenuId, tree = []} | Rest], MaxDepth, [Idx|PR], Acc, Context ) ->
    [ {m_rsc:rid(MenuId, Context), [Idx|PR], undefined} ]
        ++ menu_flat(Rest, MaxDepth, [Idx+1|PR], [], Context)
        ++  Acc;
menu_flat([ #rsc_tree{ id = MenuId } | Rest], 1, [Idx|PR], Acc, Context ) ->
    [ {m_rsc:rid(MenuId, Context), [Idx|PR], undefined} ]
        ++ menu_flat(Rest, 1, [Idx+1|PR], [], Context)
        ++  Acc;
menu_flat([ #rsc_tree{ id = MenuId, tree = Children } | Rest], MaxDepth, [Idx|PR], Acc, Context ) ->
    [ {m_rsc:rid(MenuId, Context), [Idx|PR], down} ]
        ++ menu_flat(Children, MaxDepth-1, [1,Idx|PR], [], Context)
        ++ [{undefined, undefined, up}]
        ++ menu_flat(Rest, MaxDepth, [Idx+1|PR], [], Context)
        ++  Acc;
% Old notations
menu_flat([ {MenuId, Sub} | Rest ], MaxDepth, P, A, C) when is_list(Sub) ->
    menu_flat([ #rsc_tree{ id = MenuId, tree = Sub } | Rest ], MaxDepth, P, A, C);
menu_flat([ MenuId | Rest ], MaxDepth, P, A, C) ->
    menu_flat([ #rsc_tree{ id = MenuId, tree = [] } | Rest ], MaxDepth, P, A, C).


%% @doc Find the subtree below a resource id. 'undefined' when not found.
-spec menu_subtree(Menu::list(), PageId :: term(), #context{}) -> list() | undefined.
menu_subtree(Menu, PageId, Context) ->
    menu_subtree(Menu, PageId, false, Context).

-spec menu_subtree(Menu::list(), PageId :: term(), AddSiblings :: boolean(), #context{}) -> list() | undefined.
menu_subtree([], _BelowId, _AddSiblings, _Context) ->
    undefined;
menu_subtree(Menu, _BelowId, _AddSiblings, _Context) when not is_list(Menu) ->
    undefined;
menu_subtree(_Menu, undefined, _AddSiblings, _Context) ->
    undefined;
menu_subtree(Menu, BelowId, AddSiblings, Context) when not is_integer(BelowId) ->
    menu_subtree(Menu, m_rsc:rid(BelowId, Context), AddSiblings, Context);
menu_subtree(Menu, BelowId, AddSiblings, Context) ->
    menu_subtree_1(Menu, BelowId, AddSiblings, Menu, Context).

menu_subtree_1([], _BelowId, _AddSiblings, _CurrMenu, _Context) ->
    undefined;
menu_subtree_1([ #rsc_tree{ id = BelowId, tree = Menu } | _Rest ], BelowId, false, _CurrMenu, _Context) ->
    Menu;
menu_subtree_1([ #rsc_tree{ id = Id, tree = Menu } | Rest ], BelowId, AddSiblings, CurrMenu, Context) when not is_integer(Id), Id =/= undefined ->
    RscId = m_rsc:rid(Id, Context),
    menu_subtree_1([ #rsc_tree{ id = RscId, tree = Menu } | Rest ], BelowId, AddSiblings, CurrMenu, Context);
menu_subtree_1([ #rsc_tree{ id = BelowId } | _Rest ], BelowId, true, CurrMenu, _Context)  ->
    [
        case MenuId of
            BelowId -> #rsc_tree{ id = BelowId, tree = Menu };
            _ -> #rsc_tree{ id = MenuId, tree = [] }
        end
        || #rsc_tree{ id = MenuId, tree = Menu } <- CurrMenu
    ];
menu_subtree_1([ #rsc_tree{ tree = Menu } | Rest ], BelowId, AddSiblings, CurrMenu, Context) ->
    case menu_subtree_1(Menu, BelowId, AddSiblings, Menu, Context) of
        undefined -> menu_subtree_1(Rest, BelowId, AddSiblings, CurrMenu, Context);
        M -> M
    end;
% Old notations
menu_subtree_1([{Id, Sub}|Rest], BelowId, AddSiblings, CurrMenu, Context) ->
    menu_subtree_1([ #rsc_tree{ id = Id, tree = Sub } | Rest ], BelowId, AddSiblings, CurrMenu, Context);
menu_subtree_1([Id|Rest], BelowId, AddSiblings, CurrMenu, Context) when is_integer(Id); is_atom(Id) ->
    menu_subtree_1([ #rsc_tree{ id = Id, tree = [] } | Rest ], BelowId, AddSiblings, CurrMenu, Context);
% Look into next subtree
menu_subtree_1([_|Rest], BelowId, AddSiblings, CurrMenu, Context) ->
    menu_subtree_1(Rest, BelowId, AddSiblings, CurrMenu, Context).


%% @doc Return datamodel for the menu routines.
manage_schema(_Version, Context) ->
    datamodel(Context).

%% @doc Modify the menu data on module upgrade.
manage_data({upgrade, 4}, Context) ->
    % Check all menus to add the 'hasmenupart' edges
    z_pivot_rsc:insert_task_after(5, ?MODULE, ensure_hasmenupart, <<>>, [], Context),
    ok;
manage_data(_Version, _Context) ->
    ok.

datamodel(Context) ->
    #datamodel{
        categories = [
              {menu, categorization,
                    #{
                        <<"title">> => <<"Page Menu">>
                    }
              }
        ],
        predicates = [
            {hasmenu, #{
                <<"title">> => <<"Has Menu">>,
                <<"summary">> => <<"Define preferred menu context for page, used in SEO.">>,
                <<"is_object_noindex">> => true
            }, [
                {undefined, menu}
            ]},
            {hasmenupart, #{
                <<"title">> => <<"Menu Contains">>,
                <<"summary">> => <<"Automatically updated after menu changes.">>,
                <<"is_object_noindex">> => true
            }, [
                {menu, undefined}
            ]}
        ],
        resources = [
            {main_menu,
                menu,
                #{
                    <<"title">> => <<"Main Menu">>,
                    <<"menu">> => validate(default_menu(Context), [])
                }
            }
        ]
    }.

default_menu(Context) ->
    case m_site:get(install_menu, Context) of
        Menu when is_list(Menu) -> Menu;
        _ -> []
    end.


% %% @doc test function
% %%  111  [1]
% %%  - 44   [1,1]
% %%  - - 555  [1,1,1]
% %%  - - 666  [1,1,2]
% %%  222  [2]
% %%  - 333  [2,1]
% test() ->

%     [
%      {111, [1], down },
%      {444, [1,1], down},
%      {555, [1,1,1], undefined},
%      {666, [2,1,1], undefined},
%      {undefined, undefined, up},
%      {undefined, undefined, up},
%      {222, [2], down},
%      {333, [1,2], undefined},
%      {undefined, undefined, up}
%     ]
%         = menu_flat([{111, [{444, [{555, []}, {666, []} ]}]}, {222, [{333, []}]}], x).
