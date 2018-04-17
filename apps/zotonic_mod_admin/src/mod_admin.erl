%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2012 Marc Worrell
%% Date: 2009-06-09
%% @doc Administrative interface.  Aka backend.

%% Copyright 2009-2012 Marc Worrell
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

-module(mod_admin).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Admin module").
-mod_description("Provides administrative interface for editing pages, media, users etc.").
-mod_depends([ base, authentication, mod_search, mod_mqtt, mod_wires ]).
-mod_provides([ admin ]).
-mod_schema(1).
-mod_prio(1000).

-export([
     observe_sanitize_element/3,
     observe_admin_menu/3,
     observe_admin_edit_blocks/3,
     observe_module_ready/2,
     event/2,

     do_link/5,

     manage_schema/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("include/admin_menu.hrl").


%% @doc Fix tinymce images that are the result of copying
%% <img class="z-tinymce-media z-tinymce-media-align-block z-tinymce-media-size-small z-tinymce-media-crop- z-tinymce-media-link- "
%%      src="/admin/media/preview/41113"
%%      alt="" />
observe_sanitize_element(#sanitize_element{}, {<<"img">>, Attrs, _Enclosed} = Element, Context) ->
    case proplists:get_value(<<"src">>, Attrs) of
        <<"/admin/media/preview/", Number/binary>> ->
            NumberList = binary_to_list(Number),
            case m_rsc:rid(NumberList, Context) of
                undefined ->
                    {nop, []};
                ImgId ->
                    CommentText = [
                        <<" z-media ">>,
                        integer_to_list(ImgId),
                        32,
                        class_to_opts(proplists:get_value(<<"class">>, Attrs))
                    ],
                    {comment, iolist_to_binary(CommentText)}
            end;
        _OtherSrc ->
            Element
    end;
observe_sanitize_element(#sanitize_element{}, Element, _Context) ->
    Element.


class_to_opts(undefined) ->
    [];
class_to_opts(Class) ->
    case re:run(Class, "z-tinymce-media-([a-z]+)-([a-z]*)", [{capture, all_but_first, binary}, global]) of
        nomatch ->
            [];
        {match, Ms} ->
            [
                z_json:encode([{A,V} || [A,V] <- Ms]),
                32
            ]
    end.


observe_admin_menu(#admin_menu{}, Acc, Context) ->
    [
     #menu_item{id=admin_dashboard,
                label=?__("Dashboard", Context),
                url={admin} },

     %% CONTENT %%
     #menu_item{id=admin_content,
                label=?__("Content", Context)},

     #menu_item{id=admin_overview,
                parent=admin_content,
                label=?__("Pages", Context),
                url={admin_overview_rsc}},
     #menu_item{id=admin_media,
                parent=admin_content,
                label=?__("Media", Context),
                url={admin_media}}
    ]
    ++ admin_menu_content_queries(Context) ++
    [

     %% STRUCTURE %%
     #menu_item{id=admin_structure,
                label=?__("Structure", Context)},


     %% MODULES %%
     #menu_item{id=admin_modules,
                label=?__("Modules", Context)},


     %% AUTH %%
     #menu_item{id=admin_auth,
                label=?__("Auth", Context)},

     %% SYSTEM %%
     #menu_item{id=admin_system,
                label=?__("System", Context)},

     #menu_item{id=admin_status,
                parent=admin_system,
                visiblecheck={acl, use, mod_admin_config},
                label=?__("Status", Context),
                url={admin_status}}

     |Acc].


admin_menu_content_queries(Context) ->
    #search_result{result=Result} = z_search:search({all_bytitle, [{cat,admin_content_query}]}, Context),
    AdminOverviewQueryId = m_rsc:rid(admin_overview_query, Context),
    Result1 = lists:filter(
                    fun({_Title,Id}) ->
                        Id =/= AdminOverviewQueryId
                    end,
                    Result),
    lists:map(fun({Title, Id}) ->
                #menu_item{
                    id={admin_query, Id},
                    parent=admin_content,
                    label=Title,
                    url={admin_overview_rsc, [{qquery, Id}]}
                }
              end,
              Result1).


observe_admin_edit_blocks(#admin_edit_blocks{}, Menu, Context) ->
    [
        {1, ?__("Standard", Context), [
            {header, ?__("Header", Context)},
            {text, ?__("Text", Context)},
            {page, ?__("Embed page", Context)}
        ]}
        | Menu
    ].


observe_module_ready(module_ready, Context) ->
    z_depcache:flush(admin_menu, Context).


event(#postback_notify{message= <<"admin-insert-block">>}, Context) ->
    Language = case z_context:get_q("language", Context) of
                    undefined ->
                        [];
                    Ls ->
                        Ls1 = string:tokens(Ls, ","),
                        [ list_to_atom(L) || L <- lists:filter(fun z_language:is_valid/1, Ls1) ]
               end,
    EditLanguage = case z_context:get_q("edit_language", Context) of
                    undefined ->
                        z_context:language(Context);
                    EL ->
                        case z_language:is_valid(EL) of
                            true -> list_to_atom(EL);
                            false -> z_context:language(Context)
                        end
                   end,
    Type = z_string:to_name(z_context:get_q("type", Context)),
    RscId = z_convert:to_integer(z_context:get_q("rsc_id", Context)),
    Render = #render{
                template="_admin_edit_block_li.tpl",
                vars=[
                    {id, RscId},
                    {r_language, Language},
                    {edit_language, EditLanguage},
                    is_new,
                    {is_editable, z_acl:rsc_editable(RscId, Context)},
                    {blk, [{type, Type}]},
                    {blocks, lists:sort(z_notifier:foldl(#admin_edit_blocks{id=RscId}, [], Context))}
                ]
            },
    case z_html:escape(z_context:get_q("after", Context)) of
        undefined -> z_render:insert_top("edit-blocks", Render, Context);
        AfterId -> z_render:insert_after(AfterId, Render, Context)
    end;

event(#postback_notify{message= <<"feedback">>, trigger= <<"dialog-connect-find">>, target=TargetId}, Context) ->
    % Find pages matching the search criteria.
    SubjectId = z_convert:to_integer(z_context:get_q(subject_id, Context)),
    ObjectId = z_convert:to_integer(z_context:get_q(object_id, Context)),
    Category = z_context:get_q(<<"find_category">>, Context),
    Predicate = z_context:get_q(<<"predicate">>, Context, <<>>),
    Text = z_context:get_q(<<"find_text">>, Context),
    Cats = case Category of
                <<"p:", Predicate/binary>> -> feedback_categories(SubjectId, Predicate, ObjectId, Context);
                <<>> -> [];
                CatId -> [{m_rsc:rid(CatId, Context)}]
           end,
    Vars = [
        {subject_id, SubjectId},
        {cat, Cats},
        {predicate, Predicate},
        {text, Text}
    ]++ case z_context:get_q(find_cg, Context) of
        <<>> -> [];
        undefined -> [];
        <<"me">> -> [ {creator_id, z_acl:user(Context)} ];
        CgId -> [ {content_group, m_rsc:rid(CgId, Context)}]
    end,
    z_render:wire([
        {remove_class, [{target, TargetId}, {class, "loading"}]},
        {update, [{target, TargetId}, {template, "_action_dialog_connect_tab_find_results.tpl"} | Vars]}
    ], Context);

event(#postback{message={admin_connect_select, Args}}, Context) ->
    SelectId = z_context:get_q(<<"select_id">>, Context),
    IsConnected = z_convert:to_bool(z_context:get_q(<<"is_connected">>, Context)),
    SubjectId0 = proplists:get_value(subject_id, Args),
    ObjectId0 = proplists:get_value(object_id, Args),
    Predicate = proplists:get_value(predicate, Args),
    Callback = proplists:get_value(callback, Args),

    QAction = proplists:get_all_values(action, Args),
    QActions = proplists:get_value(actions, Args, []),
    QAction1 = case QAction of
        [undefined] -> [];
        _ -> QAction
    end,
    QActions1 = case QActions of
        undefined -> [];
        _ -> QActions
    end,
    Actions = QAction1 ++ QActions1,

    {SubjectId, ObjectId} =
        case z_utils:is_empty(ObjectId0) of
            true ->
                {z_convert:to_integer(SubjectId0),
                 z_convert:to_integer(SelectId)};
            false ->
                {z_convert:to_integer(SelectId),
                 z_convert:to_integer(ObjectId0)}
        end,

    case do_link_unlink(IsConnected, SubjectId, Predicate, ObjectId, Callback, Context) of
        {ok, Context1} ->
            Context2 = case z_convert:to_bool(proplists:get_value(autoclose, Args)) of
                            true -> z_render:dialog_close(Context1);
                            false -> Context1
                       end,
            z_render:wire(Actions, Context2);
        {error, Context1} ->
            Context1
    end;

%% Called when a block connection is done
event(#postback_notify{message= <<"update">>, target=TargetId}, Context) ->
    Id = z_convert:to_integer(z_context:get_q(<<"id">>, Context)),
    Predicate = get_predicate(z_context:get_q(<<"predicate">>, Context), Context),
    Vars = [
        {id, Id},
        {predicate, Predicate}
    ],
    Context1 = z_render:wire({unmask, [{target_id, TargetId}]}, Context),
    z_render:update(TargetId, #render{template={cat, "_rsc_block_item.tpl"}, vars=Vars}, Context1);

event(_E, Context) ->
    Context.


feedback_categories(SubjectId, Predicate, _ObjectId, Context) when is_integer(SubjectId) ->
    m_predicate:object_category(Predicate, Context);
feedback_categories(_SubjectId, Predicate, ObjectId, Context) when is_integer(ObjectId) ->
    m_predicate:subject_category(Predicate, Context).


get_predicate(undefined, _Context) ->
    undefined;
get_predicate([], _Context) ->
    undefined;
get_predicate(P, Context) when is_list(P) ->
    case z_utils:only_digits(P) of
        true ->
            RscId = m_rsc:rid(P, Context),
            z_convert:to_atom(m_rsc:p(RscId, name, Context));
        false ->
            list_to_existing_atom(P)
    end.


do_link(SubjectId, Predicate, ObjectId, Callback, Context) ->
    do_link_unlink(false, SubjectId, Predicate, ObjectId, Callback, Context).

do_link_unlink(_IsUnlink, _SubjectId, Predicate, ObjectId, Callback, Context)
    when Predicate =:= ""; Predicate =:= undefined ->
    ContextP = context_language(Context),
    Title = m_rsc:p(ObjectId, title, Context),
    Vars = [
            {subject_id, undefined},
            {predicate, undefined},
            {object_id, ObjectId},
            {url_language, m_rsc:page_url(ObjectId, ContextP)},
            {title_language, z_trans:lookup_fallback(Title, ContextP)},
            {title, z_trans:lookup_fallback(Title, Context)}
           ],
    case Callback of
        undefined ->
            {ok, Context};
        {CB, Args} ->
            {ok, z_render:wire({script, [{script, [
                    z_convert:to_binary(CB), $(,
                        z_utils:js_object(Vars++Args,Context),
                    $),$;
                ]}]}, Context)};
        _ ->
            {ok, z_render:wire({script, [{script, [
                    z_convert:to_binary(Callback), $(,
                        z_utils:js_object(Vars,Context),
                    $),$;
                ]}]}, Context)}
    end;
do_link_unlink(IsUnlink, SubjectId, Predicate, ObjectId, Callback, Context) ->
    case z_acl:rsc_linkable(SubjectId, Context) of
        true ->
            case m_edge:get_id(SubjectId, Predicate, ObjectId, Context) of
                undefined when IsUnlink ->
                    do_link_unlink_feedback(
                                    false, false, undefined,
                                    SubjectId, Predicate, ObjectId,
                                    Callback, Context);
                undefined when not IsUnlink ->
                    case m_edge:insert(SubjectId, Predicate, ObjectId, Context) of
                        {ok, EdgeId} ->
                            do_link_unlink_feedback(
                                            true, false, EdgeId,
                                            SubjectId, Predicate, ObjectId,
                                            Callback, Context);
                        {error, _} ->
                            do_link_unlink_error(IsUnlink, Context)
                    end;
                EdgeId when IsUnlink ->
                    case m_edge:delete(SubjectId, Predicate, ObjectId, Context) of
                        ok ->
                            do_link_unlink_feedback(
                                            false, true, EdgeId,
                                            SubjectId, Predicate, ObjectId,
                                            Callback, Context);
                        {error, _} ->
                            do_link_unlink_error(IsUnlink, Context)
                    end;
                EdgeId when not IsUnlink->
                    do_link_unlink_feedback(
                                    false, false, EdgeId,
                                    SubjectId, Predicate, ObjectId,
                                    Callback, Context)
            end;
        false ->
            do_link_unlink_error(IsUnlink, Context)
    end.

do_link_unlink_error(false = _IsUnlink, Context) ->
    {error, z_render:growl_error(?__("Sorry, you have no permission to add the connection.", Context), Context)};
do_link_unlink_error(true, Context) ->
    {error, z_render:growl_error(?__("Sorry, you have no permission to delete the connection.", Context), Context)}.


do_link_unlink_feedback(IsNew, IsDelete, EdgeId, SubjectId, Predicate, ObjectId, Callback, Context) ->
    ContextP = context_language(Context),
    Title = m_rsc:p(ObjectId, title, Context),
    Context1 = case {IsNew, IsDelete} of
        {true,false} ->
            z_render:growl([
                        ?__("Added the connection to", ContextP),<<" \"">>, Title, <<"\".">>
                    ], Context);
        {false,true} ->
            z_render:growl([
                        ?__("Removed the connection to", ContextP),<<" \"">>, Title, <<"\".">>
                    ], Context);
        {false,false} ->
            Context
    end,
    case Callback of
        undefined ->
            {ok, Context1};
        _ ->
            Vars = [
                    {is_new, IsNew},
                    {is_delete, IsDelete},
                    {subject_id, SubjectId},
                    {predicate, Predicate},
                    {object_id, ObjectId},
                    {edge_id, EdgeId},
                    {url_language, m_rsc:page_url(ObjectId, ContextP)},
                    {title_language, z_trans:lookup_fallback(Title, ContextP)},
                    {title, z_trans:lookup_fallback(Title, Context)}
                   ],
            case Callback of
                    {CB, Args} ->
                        {ok, z_render:wire({script, [{script, [
                                z_convert:to_binary(CB), $(,
                                    z_utils:js_object(Vars++Args,ContextP),
                                $),$;
                            ]}]}, Context1)};
                    _ ->
                        {ok, z_render:wire({script, [{script, [
                                z_convert:to_binary(Callback), $(,
                                    z_utils:js_object(Vars,ContextP),
                                $),$;
                            ]}]}, Context1)}
            end
    end.

context_language(Context) ->
    case z_context:get_q("language", Context) of
        undefined -> Context;
        [] -> Context;
        Lang ->
            case z_language:to_language_atom(Lang) of
                {ok, LanguageCode} -> z_context:set_language(LanguageCode, Context);
                _ -> Context
            end
    end.


manage_schema(_Version, _Context) ->
    #datamodel{
        categories=[
            {admin_content_query,
             'query',
             [
                {title, {trans, [
                            {en, <<"Admin content query">>},
                            {nl, <<"Admin inhoud zoekopdracht">>}
                    ]}}
             ]}
        ]
    }.
