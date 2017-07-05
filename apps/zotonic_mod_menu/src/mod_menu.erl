%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2015 Marc Worrell
%% @doc Menu module.  Supports menus in Zotonic. Adds admin interface to define the menu.

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

-module(mod_menu).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Menus").
-mod_description("Menus in Zotonic, adds admin interface to define menus and other hierarchical lists.").
-mod_schema(2).
-mod_depends([]).
-mod_provides([menu]).

%% interface functions
-export([
    manage_schema/2,
    init/1,
    event/2,
    observe_menu_get_rsc_ids/2,
    observe_menu_save/2,
    observe_admin_menu/3,
    get_menu/1,
    get_menu/2,
    set_menu/3,
    menu_flat/2,
    menu_flat/3,
    menu_subtree/3,
    menu_subtree/4,
    remove_invisible/2,
    test/0
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").


%% @doc Initializes the module (after the datamodel is installed).
init(Context) ->
    case m_config:get(menu, menu_default, Context) of
        undefined -> ok;
        Props ->
            %% upgrade from previous menu
            OldMenu = proplists:get_value(menu, Props, []),
            lager:info("Upgrading old menu structure"),
            set_menu(OldMenu, Context),
            m_config:delete(menu, menu_default, Context)
    end.


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
    handle_cmd(proplists:get_value(<<"cmd">>, Data), Data, Context).


handle_cmd(<<"copy">>, Data, Context) ->
    FromId = z_convert:to_integer(proplists:get_value(<<"id">>, Data)),
    case m_rsc:is_visible(FromId, Context) of
        true ->
            NewTitle = make_copy_title(m_rsc:p(FromId, title, Context), Context),
            case m_rsc:duplicate(FromId, [{title, NewTitle}], Context) of
                {ok, NewId} ->
                    {Html, Context1} = z_template:render_to_iolist({cat, "_menu_edit_item.tpl"}, [{id, NewId}, {editable, true}], Context),
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
    {Html, Context2} = z_template:render_to_iolist({cat, "_menu_edit_item.tpl"}, [{id, Id}, {editable, true}], Context),
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



make_copy_title(Title, Context) when is_binary(Title) ->
    iolist_to_binary([?__("COPY", Context), " ", Title]);
make_copy_title({trans, Ts}, Context) ->
    {trans, [
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
        Category = case lists:last(string:tokens(Id, "-")) of
                        [] ->
                            text;
                        C ->
                            case m_category:name_to_id(C, Context) of
                                {error, _} -> text;
                                {ok, C1} -> C1
                            end
                   end,
        Props = [
            {is_published, false},
            {category, Category},
            {title, ?__("New page", Context)}
        ],
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



%% @doc Notifier handler to get all menu ids for the default menu.
observe_menu_get_rsc_ids(menu_get_rsc_ids, Context) ->
    Menu = get_menu(Context),
    {ok, menu_ids(Menu, [])}.

    menu_ids([], Acc) ->
        Acc;
    menu_ids([{Id,SubMenu}|T], Acc) ->
        Acc1 = menu_ids(SubMenu, [Id|Acc]),
        menu_ids(T, Acc1);
    menu_ids([H|T], Acc) ->
        menu_ids(T, [H|Acc]).


%% @doc Observer the 'menu_save' notification
observe_menu_save(#menu_save{id=MenuId, tree=Menu}, Context) ->
    set_menu(MenuId, Menu, Context).


%% @doc Fetch the default menu. Performs validation/visibility checking on the menu items.
%% @spec get_menu(Context) -> list()
get_menu(Context) ->
    get_menu(m_rsc:rid(main_menu, Context), Context).

%% @doc Fetch a menu structure from a rsc. Performs validation/visibility checking on the menu items.
%% @spec get_menu(Id, Context) -> list()
get_menu(Id, Context) ->
    case m_rsc:p(Id, menu, Context) of
        undefined -> [];
        <<>> -> [];
        Menu -> remove_invisible(validate(Menu, []), [], Context)
    end.

	validate([], Acc) ->
		lists:reverse(Acc);
	validate([{_M,_L} = M|Ms], Acc) ->
		validate(Ms, [M|Acc]);
	validate([M|Ms], Acc) ->
		validate(Ms, [{M,[]}|Acc]).


remove_invisible(Menu, Context) ->
    remove_invisible(Menu, [], Context).

%% Remove invisible menu items
remove_invisible(undefined, Acc, _Context) ->
    lists:reverse(Acc);
remove_invisible(<<>>, Acc, _Context) ->
    lists:reverse(Acc);
remove_invisible([], Acc, _Context) ->
    lists:reverse(Acc);
remove_invisible([{Id,Sub}|Rest], Acc, Context) ->
    case m_rsc:is_visible(Id, Context) andalso m_rsc:exists(Id, Context) of
        true ->  remove_invisible(Rest, [{Id,remove_invisible(Sub, [], Context)} | Acc], Context);
        false -> remove_invisible(Rest, Acc, Context)
    end;
remove_invisible([Id|Rest], Acc, Context) ->
    case m_rsc:is_visible(Id, Context) andalso m_rsc:exists(Id, Context) of
        true ->  remove_invisible(Rest, [Id | Acc], Context);
        false -> remove_invisible(Rest, Acc, Context)
    end.


%% @doc Save the default menu, ensure that the resource 'main_menu' is defined.
set_menu(Menu, Context) ->
    case m_rsc:rid(main_menu, Context) of
        undefined ->
            Props = [
                {category, menu},
                {is_published, true},
                {title, <<"Main menu">>},
                {menu, Menu}
            ],
            {ok, _Id} = m_rsc:insert(Props, z_acl:sudo(Context)),
            ok;
        Id ->
            set_menu(Id, Menu, Context)
    end.



%% @doc Save the current menu.
set_menu(Id, Menu, Context) ->
    m_rsc:update(Id, [{menu, Menu}], Context).


%% @doc Flatten the menu structure in a list, used for display purposes in templates
-spec menu_flat(list() | undefined | <<>>, #context{}) -> [ {integer()|undefined, LevelIndex::list(integer()) | undefined, up|down|undefined} ].
menu_flat(Menu, Context) ->
    menu_flat(Menu, 999, Context).

-spec menu_flat(list() | undefined | <<>>, integer(), #context{}) -> [ {integer()|undefined, LevelIndex::list(integer()) | undefined, up|down|undefined} ].
menu_flat(Menu, _MaxDepth, _Context) when not is_list(Menu) ->
    [];
menu_flat([], _MaxDepth, _Context) ->
    [];
menu_flat(X, MaxDepth, Context) ->
    menu_flat(X, MaxDepth, [1], [], Context).

menu_flat([], _MaxDepth, _Path, Acc, _Context) ->
    lists:reverse(Acc);
menu_flat([ {MenuId, []} | Rest], MaxDepth, [Idx|PR], Acc, Context ) ->
    [ {m_rsc:rid(MenuId, Context), [Idx|PR], undefined} ]
        ++ menu_flat(Rest, MaxDepth, [Idx+1|PR], [], Context)
        ++  Acc;
menu_flat([ {MenuId, _Children} | Rest], 1, [Idx|PR], Acc, Context ) ->
    [ {m_rsc:rid(MenuId, Context), [Idx|PR], undefined} ]
        ++ menu_flat(Rest, 1, [Idx+1|PR], [], Context)
        ++  Acc;
menu_flat([ {MenuId, Children} | Rest], MaxDepth, [Idx|PR], Acc, Context ) ->
    [ {m_rsc:rid(MenuId, Context), [Idx|PR], down} ]
        ++ menu_flat(Children, MaxDepth-1, [1,Idx|PR], [], Context)
        ++ [{undefined, undefined, up}]
        ++ menu_flat(Rest, MaxDepth, [Idx+1|PR], [], Context)
        ++  Acc;
menu_flat([ MenuId | Rest ], MaxDepth, P, A, C) when is_integer(MenuId) ->
    %% oldschool notation fallback
    menu_flat([{MenuId, []} | Rest], MaxDepth, P, A, C).


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
    menu_subtree_1([{BelowId, Menu}|_Rest], BelowId, false, _CurrMenu, _Context) ->
        Menu;
    menu_subtree_1([{Id, Menu}|Rest], BelowId, AddSiblings, CurrMenu, Context) when not is_integer(Id) ->
        menu_subtree_1([{m_rsc:rid(Id, Context), Menu}|Rest], BelowId, AddSiblings, CurrMenu, Context);
    menu_subtree_1([{BelowId, _}|_Rest], BelowId, true, CurrMenu, _Context)  ->
        [
            case MenuId of
                BelowId -> {BelowId, Menu};
                _ -> {MenuId, []}
            end
            || {MenuId,Menu} <- CurrMenu
        ];
    menu_subtree_1([{_Id, Menu}|Rest], BelowId, AddSiblings, CurrMenu, Context) ->
        case menu_subtree_1(Menu, BelowId, AddSiblings, Menu, Context) of
            undefined -> menu_subtree_1(Rest, BelowId, AddSiblings, CurrMenu, Context);
            M -> M
        end;
    % Old notation
    menu_subtree_1([BelowId|_Rest], BelowId, false, _CurrMenu, _Context) ->
        [];
    menu_subtree_1([BelowId|_Rest], BelowId, true, CurrMenu, _Context) ->
        [
          case Id of
            {MenuId,_} -> {MenuId, []};
            _ -> {Id,[]}
          end
          || Id <- CurrMenu
        ];
    menu_subtree_1([_|Rest], BelowId, AddSiblings, CurrMenu, Context) ->
        menu_subtree_1(Rest, BelowId, AddSiblings, CurrMenu, Context).




%% @doc The datamodel for the menu routines.
manage_schema(install, Context) ->
    z_datamodel:manage(
      mod_menu,
      #datamodel{
        categories = [
              {menu, categorization,
               [{title, <<"Page menu">>}]
              }
        ],
        resources = [
              {main_menu,
                   menu,
                   [{title, <<"Main menu">>},
                    {menu, case z_install_defaultdata:default_menu(Context) of
                                undefined -> [];
                                Menu -> Menu
                           end}
                   ]
              }
        ]
      },
      Context);
manage_schema(_Version, _Context) ->
    ok.



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



%% @doc test function
%%  111  [1]
%%  - 44   [1,1]
%%  - - 555  [1,1,1]
%%  - - 666  [1,1,2]
%%  222  [2]
%%  - 333  [2,1]
test() ->

    [
     {111, [1], down },
     {444, [1,1], down},
     {555, [1,1,1], undefined},
     {666, [2,1,1], undefined},
     {undefined, undefined, up},
     {undefined, undefined, up},
     {222, [2], down},
     {333, [1,2], undefined},
     {undefined, undefined, up}
    ]
        = menu_flat([{111, [{444, [{555, []}, {666, []} ]}]}, {222, [{333, []}]}], x).
