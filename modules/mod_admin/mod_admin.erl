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
-mod_depends([base, authentication]).
-mod_provides([admin]).
-mod_prio(1000).

-export([
         observe_sanitize_element/3,
         observe_admin_menu/3,
         observe_admin_edit_blocks/3,
         event/2,

         do_link/5
]).

-include_lib("zotonic.hrl").
-include_lib("include/admin_menu.hrl").


%% @doc Fix tinymce images that are the result of copying
%% <img class="z-tinymce-media z-tinymce-media-align-block z-tinymce-media-size-small z-tinymce-media-crop- z-tinymce-media-link- " 
%%      src="/admin/media/preview/41113" 
%%      alt="" />
observe_sanitize_element(sanitize_element, {<<"img">>, Attrs, _Enclosed} = Element, Context) ->
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
                    ?DEBUG({comment, iolist_to_binary(CommentText)})
            end;
        _OtherSrc ->
            Element
    end;
observe_sanitize_element(sanitize_element, Element, _Context) ->
    Element.


class_to_opts(undefined) ->
    [];
class_to_opts(Class) ->
    case re:run(Class, "z-tinymce-media-([a-z]+)-([a-z]*)", [{capture, all_but_first, binary}, global]) of
        nomatch ->
            [];
        {match, Ms} ->
            [
                mochijson:encode({struct, [{A,V} || [A,V] <- Ms]}),
                32
            ]
    end.

    
observe_admin_menu(admin_menu, Acc, Context) ->
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
                url={admin_media}},

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
                label=?__("Status", Context),
                url={admin_status}}

     |Acc].


observe_admin_edit_blocks(#admin_edit_blocks{}, Menu, Context) ->
    [
        {1, ?__("Texts", Context), [
            {header, ?__("Header", Context)},
            {text, ?__("Text", Context)}
        ]}
        | Menu
    ].


event(#postback_notify{message="admin-insert-block"}, Context) ->
    Language = case z_context:get_q("language", Context) of
                    undefined -> 
                        [];
                    Ls -> 
                        Ls1 = string:tokens(Ls, ","),
                        [ list_to_atom(L) || L <- lists:filter(fun z_trans:is_language/1, Ls1) ]
               end,
    EditLanguage = case z_context:get_q("edit_language", Context) of
                    undefined -> 
                        z_context:language(Context);
                    EL ->
                        case z_trans:is_language(EL) of
                            true -> list_to_atom(EL);
                            false -> z_context:language(Context)
                        end
                   end,
    Type = z_string:to_name(z_context:get_q("type", Context)),
    RscId = list_to_integer(z_context:get_q("rsc_id", Context)),
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

event(#postback_notify{message="feedback", trigger="dialog-connect-find", target=TargetId}, Context) ->
    % Find pages matching the search criteria.
    SubjectId = z_convert:to_integer(z_context:get_q(subject_id, Context)),
    Category = z_context:get_q(find_category, Context),
    Text=z_context:get_q(find_text, Context),
    Cats = case Category of
                "p:"++Predicate -> m_predicate:object_category(Predicate, Context);
                "" -> [];
                CatId -> [{z_convert:to_integer(CatId)}]
           end,
    Vars = [
        {subject_id, SubjectId},
        {cat, Cats},
        {text, Text}
    ],
    z_render:wire([
        {remove_class, [{target, TargetId}, {class, "loading"}]},
        {update, [{target, TargetId}, {template, "_action_dialog_connect_tab_find_results.tpl"} | Vars]}
    ], Context);

event(#postback_notify{message="admin-connect-select"}, Context) ->
    SubjectId = z_convert:to_integer(z_context:get_q("subject_id", Context)),
    Predicate = z_context:get_q("predicate", Context),
    ObjectId = z_convert:to_integer(z_context:get_q("select_id", Context)),
    Callback = z_context:get_q("callback", Context),
    case do_link(SubjectId, Predicate, ObjectId, Callback, Context) of
        {ok, Context1} ->
            z_render:dialog_close(Context1);
        {error, Context1} ->
            Context1
    end;

event(#postback_notify{message="update", target=TargetId}, Context) ->
    Template = filename:basename(z_context:get_q("template", Context)),
    Id = z_convert:to_integer(z_context:get_q("id", Context)),
    Predicate = list_to_existing_atom(z_context:get_q("predicate", Context)),
    Vars = [
        {id, Id},
        {predicate, Predicate}
    ],
    Context1 = z_render:wire({unmask, []}, Context),
    z_render:update(TargetId, #render{template=Template, vars=Vars}, Context1);

event(_E, Context) ->
    Context.



do_link(SubjectId, Predicate, ObjectId, Callback, Context) ->
    case z_acl:rsc_editable(SubjectId, Context) of
        true ->
            {EdgeId,IsNew} = case m_edge:get_id(SubjectId, Predicate, ObjectId, Context) of
                undefined ->
                    {ok, EdgId} = m_edge:insert(SubjectId, Predicate, ObjectId, Context),
                    {EdgId, true};
                EdgId ->
                    {EdgId, false}
            end,

            ContextP = context_language(Context),
            Title = m_rsc:p(ObjectId, title, Context),
            Vars = [
                    {is_new, IsNew},
                    {subject_id, SubjectId},
                    {predicate, Predicate},
                    {object_id, ObjectId},
                    {edge_id, EdgeId},
                    {url_language, m_rsc:page_url(ObjectId, ContextP)},
                    {title_language, z_trans:lookup_fallback(Title, ContextP)},
                    {title, z_trans:lookup_fallback(Title, Context)}
                   ],
            Context1 = case Callback of
                    undefined -> 
                        Context;
                    _ ->
                        z_render:wire({script, [{script, [
                                Callback, $(,
                                    z_utils:js_object(Vars,Context),
                                $),$;
                            ]}]}, Context)
                end,

            case IsNew of
                true -> {ok, z_render:growl([?__("Added the connection to", Context1),"“",Title,"”."], Context1)};
                false -> {ok, Context1}
            end;
        false ->
            {error, z_render:growl_error(?__("Sorry, you have no permission to add the connection.",Context), Context)}
    end.

context_language(Context) ->
    case z_context:get_q("language", Context) of
        undefined -> Context;
        [] -> Context;
        Lang -> 
            case z_trans:to_language_atom(Lang) of
                {ok, IsoCode} -> z_context:set_language(IsoCode, Context);
                _ -> Context
            end
    end.

