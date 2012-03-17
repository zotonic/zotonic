%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-07-12
%% @doc Menu module.  Supports menus in Zotonic. Adds admin interface to define the menu.

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

-module(mod_menu).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Menus").
-mod_description("Menus in Zotonic, adds amdin interface to define the menu.").
-mod_schema(1).
-mod_depends([admin]).
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
    remove_invisible/2,
    test/0
]).

-include("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").


%% @doc Initializes the module (after the datamodel is installed).
init(Context) ->
    case m_config:get(menu, menu_default, Context) of
        undefined -> ok;
        Props -> 
            %% upgrade from previous menu
            OldMenu = proplists:get_value(menu, Props, []),
            ?zInfo("Upgrading old menu structure", Context),
            set_menu(OldMenu, Context),
            m_config:delete(menu, menu_default, Context)
    end.


event(#postback_notify{message="menuedit", trigger=TriggerId}, Context) ->
    {Kind, RootId, Predicate} = get_kind_root(TriggerId),
    Tree = unpack(z_context:get_q("tree", Context)),
    {Tree1, Context1} = create_new(Tree, Context),
    case z_context:get_q("kind", Context1, Kind) of
        category ->
            % This is the category hierarchy.
            z_notifier:notify(#category_hierarchy_save{tree=Tree1}, Context1),
            Context1;
        menu ->
            % A menu hierarchy, give it to the menu routines
            z_notifier:notify(#menu_save{id=m_rsc:rid(RootId, Context1), tree=Tree1}, Context1),
            Context1;
        edge ->
            % Hierarchy using edges between resources
            hierarchy_edge(m_rsc:rid(RootId, Context1), Predicate, Tree1, Context1)
    end.

% @doc Sync a hierarchy based on edges (silently ignore ACL errors)
hierarchy_edge(RootId, Predicate, Tree, Context) ->
    Predicate = z_context:get_q("z_predicate", Context, Predicate),
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
%%      edge-ROOTID
%%      edge-ROOTID-PREDICATE
%%      collection-ROOTID
get_kind_root("category") ->
    {category, undefined, undefined};
get_kind_root("menu-"++MenuId) ->
    {menu, MenuId, undefined};
get_kind_root(TriggerId) ->
    case string:tokens(TriggerId, "-") of
        ["edge", RootId, Predicate] ->
            {edge, RootId, Predicate};
        ["collection", RootId] ->
            {edge, RootId, haspart};
        _ ->
            {edge, TriggerId, haspart}
    end.

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

map_id(Id) ->
    Id1 = lists:last(string:tokens(Id, "-")),
    case z_utils:only_digits(Id1) of
        true -> list_to_integer(Id1);
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


%% @doc Save the default menu.
set_menu(Menu, Context) ->
    Id = m_rsc:rid(main_menu, Context),
    set_menu(Id, Menu, Context).


%% @doc Save the current menu.
set_menu(Id, Menu, Context) ->
    m_rsc:update(Id, [{menu, Menu}], Context).



menu_flat(undefined, _Context) ->
    [];
menu_flat(<<>>, _Context) ->
    [];
menu_flat(X, Context) ->
    menu_flat(X, [1], [], Context).

menu_flat([], _Path, Acc, _Context) ->
    lists:reverse(Acc);
menu_flat([ {MenuId, []} | Rest], [Idx|PR], Acc, Context ) ->

    [ {m_rsc:rid(MenuId, Context), [Idx|PR], undefined} ] 
        ++ menu_flat(Rest, [Idx+1|PR], [], Context)
        ++  Acc;
menu_flat([ {MenuId, Children} | Rest], [Idx|PR], Acc, Context ) ->

    [ {m_rsc:rid(MenuId, Context), [Idx|PR], down} ] 
        ++ menu_flat(Children, [1,Idx|PR], [], Context) 
        ++ [{undefined, undefined, up}]
        ++ menu_flat(Rest, [Idx+1|PR], [], Context)
        ++  Acc;
menu_flat([ MenuId | Rest ], P, A, C) when is_integer(MenuId) ->
    %% oldschool notation fallback
    menu_flat([{MenuId, []} | Rest], P, A, C).


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
                    {menu, case z_install_defaultdata:default_menu(m_site:get(skeleton, Context)) of
                                undefined -> [];
                                Menu -> Menu
                           end}
                   ]
              }
        ]
      },
      Context).



observe_admin_menu(admin_menu, Acc, Context) ->
    case m_rsc:name_to_id(main_menu, Context) of
        {ok, Id} ->
            ?DEBUG(Id),

            [
             #menu_item{id=admin_menu,
                        parent=admin_content,
                        label=?__("Menu", Context),
                        url={admin_admin_edit_rsc, [{id, Id}]},
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
