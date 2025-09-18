%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2025 Marc Worrell
%% @doc Administrative interface.  Aka backend.
%% @enddoc

%% Copyright 2009-2025 Marc Worrell
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
-moduledoc("
Todo

Finish documentation



Extending the admin menu
------------------------

See [m\\_admin\\_menu](/id/doc_model_model_admin_menu) on how to extend the admin menu.



Extending the admin edit page
-----------------------------

There are several special templates names that will be automatically included into the /admin/edit/xxx page from when
you create these specially named templates.

`_admin_edit_basics.tpl`

Will be automatically included into main (left) div (at top).

`_admin_edit_content.tpl`

Will be automatically included into main (left) div (at bottom).

`_admin_edit_sidebar.tpl`

Will be automatically included into right sidebar (near middle/bottom).

These templates are included using the [all catinclude](/id/doc_template_tag_tag_all_catinclude) tag; so if you need
something in the sidebar just for persons, create a `_admin_edit_sidebar.person.tpl` file in your project.



### Overriding TinyMCE options

If you need to override TinyMCE options; adding plugins, or setting other settings; you can create an
`_admin_tinymce_overrides_js.tpl` file which can contain extra settings for the TinyMCE editors in the admin.

The template must contain JavasSript which modifies the tinyInit variable just before the editor is started. For
example, to tweak the “paste” options you can put the following in the template:


```javascript
tinyInit.theme_advanced_blockformats = \"p,h1,h2\"
tinyInit.paste_auto_cleanup_on_paste = true;
tinyInit.paste_remove_styles = true;
tinyInit.paste_remove_styles_if_webkit = true;
tinyInit.paste_strip_class_attributes = true;
tinyInit.paste_text_sticky = true;
tinyInit.paste_text_sticky_default = true;
```



### TinyMCE Zotonic options

Zotonic provides extra init options:

`z_insert_dialog_enabled`

Set this to false to prevent the insert media dialog from showing. Default `true`.

`z_properties_dialog_enabled`

Set this to false to prevent the media properties dialog from showing. Default `true`.



Writing admin widget templates
------------------------------

This section contains examples of templates to create widgets for the /admin. Each of these examples extends basic
several widget templates from mod\\_admin. To write your own you need to drop example content and fill holes in these
example widgets.

You can use them as basis for your site admin-related tasks.

`_admin_dashboard_example.tpl`

Very simple example widget for admin dashboard. Contains blocks for title and body. Look at /admin to see several
dashboard widgets (latest events, pages, media, etc).

`_admin_widget_std.tpl`

Sligthly more complex widget example. Same templates are used into /admin/edit/N for main content and sidebar widgets.
These widgets do not provide any localization abilities. Also note that there are several special widget names:

`_admin_widget_i18n.tpl`

Complex widget example. Is used to edit localized rsc properties. It will be rendered as tabs. See /admin/edit/N top
left to see the tabs. If mod\\_translation disabled, then i18n-widgets are displayed same as \\_admin\\_widget\\_std.tpl.



Making an admin widget conditionally visible
--------------------------------------------

See also

[admin\\_edit\\_widget\\_i18n.tpl](../templates/template_admin_edit_widget_i18n.html#template-admin-edit-widget-i18n), [inherit](/id/doc_template_tag_tag_inherit)

To make an entire admin widget visible or not, depending on some condition that you want to calculate inside the
widget’s code, you can use the widget\\_wrapper block (which sits around the entire widget) in combination with the
[inherit](/id/doc_template_tag_tag_inherit) tag, wrapping that with a condition.

For instance, [mod\\_backup](/id/doc_module_mod_backup) uses this technique to display the import/export sidebar widget.
Excerpt from mod\\_backup’s \\_admin\\_edit\\_sidebar.tpl:


```django
{# Make the widget conditional, based on the config value mod_backup.admin_panel #}
{% block widget_wrapper %}
    {% if m.config.mod_backup.admin_panel.value %}
        {% inherit %}
    {% endif %}
{% endblock %}
```

In this example, when the condition is true, the wrapper is rendered normally (content is inherited from the extended
template); when false, the wrapper block is overridden by new (but void) content.



Extending the admin overview page
---------------------------------

The overview page at `admin/overview` shows a table with links to all pages. It can be filtered and sorted. This
extension deals with the view when a category filter has been selected.

The goal is to add more specific information that helps to distinguish pages.

The Category column, second from the left, can be extended to carry category-specific information. As an example, pages
of category Event show the start date for each event, grayed out when the event is in the past. Something like this:

| Title       | Starts              | Created on         |
| ----------- | ------------------- | ------------------ |
| Great event | 2016-08-27 00:00:00 | 25 Aug 2015, 22:19 |
| Past event  | 2014-01-02 00:00:00 | 01 Jan 2014, 13:10 |

Instead of dates, the information can be anything - from color coding labels to location data, the number of comments or
the completeness state of product descriptions.



### Setting up templates

To make it work we are using 3 templates (where category\\_name is the lowercase name of your category):

`_admin_overview_list.category_name.tpl`

Overrides the overview with a `field` variable for our custom sort. If we are using [an existing resource
property](/id/doc_model_model_rsc) such as `date_start`, we write:


```django
{% include \"_admin_overview_list.tpl\"
  field=\"pivot.date_start\"
%}
```

`_admin_sort_header.category_name.tpl`

The sort header caption. For a non-sortable header, just write the caption as text. For a sortable header, include the
sort functionality in `_admin_sort_header.tpl` and pass the caption as variable:


```django
{% include \"_admin_sort_header.tpl\"
    caption=\"My caption\"
    type=\"date\"
%}
```

`type=\"date\"` indicates that sorting should start descending, from new to old.

`_admin_overview_list_data.category_name.tpl`

Contains the category-specific information. This is a freeform Zotonic template. The Events example checks if the end
date is in the past:


```django
{% if id.date_end|in_past %}
    <span class=\"text-muted\">{{ id.date_start|timesince }}</span>
{% else %}
    <span>{{ id.date_start|timesince }}</span>
{% endif %}
```



### Custom sort properties

To sort on values that are not stored in the default Zotonic resources, you will need to create a [custom
pivot](/id/doc_cookbook_custom_pivot#cookbook-custom-pivots). This will create an additional database table with the
values to sort on.

Let’s take the (unlikely) example where we want to display the summary of each page (and sort on it as well). The
summary data is not stored in an easily accessible way (at least for sorting), so we need to add 2 pivot methods to our
Erlang module:


```django
-module(mymodule).

-export([
    init/1,
    observe_custom_pivot/2
]).

init(Context) ->
    z_pivot_rsc:define_custom_pivot(?MODULE, [{summary, \"text\"}], Context),
    ok.

observe_custom_pivot({custom_pivot, Id}, Context) ->
    case z_trans:lookup_fallback(m_rsc:p(Id, summary, Context), Context) of
        undefined ->
            none;
        Summary ->
            {?MODULE, [{summary, Summary}]}
    end.
```

For this example we are just grabbing the default language text.

The `field` name in `_admin_overview_list.category_name.tpl` now just needs to contain the pivot column name:


```django
{% include \"_admin_overview_list.tpl\"
    field=\"pivot.mymodule.summary\"
%}
```

And the sort header template `_admin_sort_header.category_name.tpl` adds the custom pivot variable:


```django
{% include \"_admin_sort_header.tpl\"
    caption=\"Summary\"
%}
```



Resource meta features
----------------------

Resources in the meta category can have ‘features’: certain resource properties (usually in the form of checkboxes)
that decide what to show or hide on certain pages in the admin. To use this, create a `_admin_features.category.tpl` in
your module.

For instance, the module mod\\_geomap defines the following `_admin_features.category.tpl` to create an extra checkbox
so that per category can be defined whether or not the geodata box should be shown:


```django
<div class=\"controls\">
        <label class=\"checkbox\">
        <input value=\"1\" type=\"checkbox\"
               name=\"feature_show_geodata\"
               {% if id.is_feature_show_geodata|if_undefined:true %}checked{% endif %}
               />
        {_ Show geo data on edit page _}
    </label>
</div>
```

And on the edit page there is this check to conditionally include the geodata box:


```django
{% if id.category_id.is_feature_show_geodata|if_undefined:true %}
```

The filter [if\\_undefined](/id/doc_template_filter_filter_if_undefined) is used so that the default value can be true
when the checkbox has never been touched.



Configuration keys
------------------

For the admin there are the following configuration keys:

*   `mod_admin.rsc_dialog_tabs`
*   `mod_admin.rsc_dialog_is_published`
*   `mod_admin.rsc_dialog_is_dependent`
*   `mod_admin.rsc_dialog_hide_dependent`
*   `mod_admin.connect_created_me`

The `mod_admin.rsc_dialog_tabs` key defines which tabs are shown in the new resource, media-upload, and image-link
dialogs. Per defauls these dialogs show all the possible tabs, with this configurarion key it is possible to change that.

Available tabs are: `find,new,upload,url,embed,oembed,depiction`. More can be defined by modules.

Tab `depiction` is used for the TinyMCE image-link dialog; it shows all media connected using the `depiction` predicate.

The configuration `mod_admin.rsc_dialog_is_published` defines the default *is\\_published* state for new resources.
Setting this key to 1 will check the *is\\_published* checkbox.

The configuration `mod_admin.edge_list_max_length` defines the maximum number of connections shown per predicate in the
connection list sidebar. If there are more connections then the list truncated, and the message \\_Too many connections,
only the first and last are shown.\\_ is displayed. The default is 100 connections.

The configuration `mod_admin.rsc_dialog_is_dependent` defines the default *is\\_dependent* state for new resources.
Setting this key to 1 will check the *is\\_dependent* checkbox.

With the configuration `mod_admin.rsc_dialog_hide_dependent` the *dependent* checkbox can be hidden for non admin users.

If the `mod_admin.connect_created_me` is set then the connect dialogs will per default filter for content made by the
current user. The current setting is stored in the sessionStorage with the key `dialog_connect_created_me`.
").
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Admin module").
-mod_description("Provides administrative interface for editing pages, media, users etc.").
-mod_depends([ base, authentication, mod_search, mod_mqtt, mod_wires ]).
-mod_provides([ admin ]).
-mod_schema(3).
-mod_prio(1000).
-mod_config([
        #{
            key => is_notrack_refers,
            type => boolean,
            default => false,
            description => "If true, the admin module will not track refers connections between resources."
        },
        #{
            key => connect_created_me,
            type => boolean,
            default => true,
            description => "If true, the connect dialog will set per default the 'created by me' filter."
        },
        #{
            key => edge_list_max_length,
            type => integer,
            default => 100,
            description => "Maximum number of edges to show in the edge list on the resource edit pages."
        },
        #{
            key => rsc_dialog_is_published,
            type => boolean,
            default => false,
            description => "If true, the resource create dialog will check the 'is published' checkbox."
        },
        #{
            key => rsc_dialog_is_dependent,
            type => boolean,
            default => false,
            description => "If true, the resource create dialog will check the 'is dependent' checkbox."
        },
        #{
            key => rsc_dialog_hide_dependent,
            type => boolean,
            default => false,
            description => "If true, the resource create dialog will hide the dependent checkbox."
        }
    ]).

-export([
     observe_sanitize_element/3,
     observe_admin_menu/3,
     observe_admin_edit_blocks/3,
     observe_module_ready/2,
     observe_rsc_update_done/2,
     event/2,

     do_link/5,

     manage_schema/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include("../include/admin_menu.hrl").


%% @doc Fix tinymce images that are the result of copying
%% <img class="z-tinymce-media z-tinymce-media-align-block z-tinymce-media-size-small z-tinymce-media-crop- z-tinymce-media-link- "
%%      src="/admin/media/preview/41113"
%%      alt="" />
-spec observe_sanitize_element(#sanitize_element{}, Acc, z:context()) -> Result when
    Acc :: Element,
    Result :: Element,
    Element :: {binary(), list( {binary(), binary()} ), list()}.
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


-spec observe_admin_menu(#admin_menu{}, Acc, z:context()) -> Result when
    Acc :: MenuItems,
    Result :: MenuItems,
    MenuItems :: [ #menu_item{} ].
observe_admin_menu(#admin_menu{}, Acc, Context) ->
    [
     #menu_item{id = admin_dashboard,
                label = ?__("Dashboard", Context),
                url = admin },

     %% CONTENT %%
     #menu_item{id = admin_content,
                label = ?__("Content", Context)},

     #menu_item{id = admin_overview,
                parent = admin_content,
                label = ?__("Pages", Context),
                url = admin_overview_rsc,
                sort = 1},
     #menu_item{id = admin_media,
                parent = admin_content,
                label = ?__("Media", Context),
                url = admin_media,
                sort = 2},
     #menu_separator{
                parent=admin_content,
                sort = 10}
    ]
    ++ admin_menu_content_queries(Context) ++
    [

     %% STRUCTURE %%
     #menu_item{id = admin_structure,
                label = ?__("Structure", Context)},


     %% MODULES %%
     #menu_item{id = admin_modules,
                label = ?__("Modules", Context)},


     %% AUTH %%
     #menu_item{id = admin_auth,
                label = ?__("Auth", Context)},

     %% SYSTEM %%
     #menu_item{id = admin_system,
                label = ?__("System", Context)},

     #menu_item{id = admin_status,
                parent = admin_system,
                visiblecheck = {acl, use, mod_admin_config},
                label = ?__("Status", Context),
                url = admin_status}

     |Acc].


admin_menu_content_queries(Context) ->
    #search_result{result=Result} = z_search:search(
            <<"all_bytitle">>, #{ <<"cat">> => admin_content_query },
            1, 100, Context),
    AdminOverviewQueryId = m_rsc:rid(admin_overview_query, Context),
    Result1 = lists:filter(
        fun({_Title,Id}) ->
            z_acl:rsc_visible(Id, Context)
            andalso Id =/= AdminOverviewQueryId
        end,
        Result),
    CQ = lists:map(
        fun({Title, Id}) ->
            #menu_item{
                id = {admin_query, Id},
                parent = admin_content,
                label = Title,
                url = {admin_overview_rsc, [{qquery_id, Id}]},
                sort = 10
            }
        end,
        Result1),
    case CQ of
        [] ->
            [];
        _ ->
            CQ ++ [ #menu_separator{ parent = admin_content, sort = 10 } ]
    end.


-spec observe_admin_edit_blocks(#admin_edit_blocks{}, Acc, z:context()) -> Result when
    Acc :: BlockGroups,
    Result :: BlockGroups,
    BlockGroups :: [ {Prio, SectionTitle, BlockTypes} ],
    Prio :: integer(),
    SectionTitle :: binary() | string() | z:trans(),
    BlockTypes :: [ {atom(), binary() | string() | z:trans()}].
observe_admin_edit_blocks(#admin_edit_blocks{}, Menu, Context) ->
    [
        {1, ?__("Standard page block types", Context), [
            {header, ?__("Header | a big header", Context)},
            {text, ?__("Text | a (rich) text block", Context)},
            {page, ?__("Embed page | a citation, popup, or link to another page", Context)}
        ]}
        | Menu
    ].


-spec observe_module_ready(module_ready, z:context()) -> any().
observe_module_ready(module_ready, Context) ->
    z_depcache:flush(admin_menu, Context).

-spec observe_rsc_update_done(#rsc_update_done{}, z:context()) -> any().
observe_rsc_update_done(#rsc_update_done{ action = Action, id = Id }, Context) when
    Action =:= insert;
    Action =:= update ->
    case m_config:get_boolean(?MODULE, is_notrack_refers, Context) of
        true ->
            ok;
        false ->
            z_admin_refers:ensure_refers(Id, z_acl:sudo(Context))
    end;
observe_rsc_update_done(#rsc_update_done{}, _Context) ->
    ok.


event(#postback_notify{message= <<"admin-insert-block">>}, Context) ->
    Language = language_list(z_context:get_q(<<"language">>, Context)),
    EditLanguage = case z_context:get_q(<<"edit_language">>, Context) of
                    undefined ->
                        z_context:language(Context);
                    EL ->
                        case z_language:to_language_atom(EL) of
                            {ok, ELA} -> ELA;
                            {error, _} -> z_context:language(Context)
                        end
                   end,
    Type = z_string:to_name(z_context:get_q(<<"type">>, Context)),
    RscId = z_convert:to_integer(z_context:get_q(<<"rsc_id">>, Context)),
    Render = #render{
                template="_admin_edit_block_li.tpl",
                vars=[
                    {id, RscId},
                    {r_language, Language},
                    {edit_language, EditLanguage},
                    is_new,
                    {is_editable, z_acl:rsc_editable(RscId, Context)},
                    {blk, #{
                        <<"type">> => Type
                    }},
                    {blocks, lists:sort(z_notifier:foldl(#admin_edit_blocks{id=RscId}, [], Context))}
                ]
            },
    case z_html:escape(z_context:get_q(<<"after">>, Context)) of
        undefined -> z_render:insert_top("edit-blocks", Render, Context);
        AfterId -> z_render:insert_after(AfterId, Render, Context)
    end;

event(#postback_notify{message = <<"feedback">>, trigger = Trigger, target=TargetId}, Context)
    when Trigger =:= <<"dialog-new-rsc-tab">>; Trigger =:= <<"dialog-connect-find">> ->
    % Find pages matching the search criteria.
    CreatorId = case z_convert:to_integer(z_context:get_q(<<"find_creator_id">>, Context)) of
        undefined ->
            case z_context:get_q(<<"find_cg">>, Context) of
                <<"me">> -> z_acl:user(Context);
                _ -> undefined
            end;
        CrId ->
            CrId
    end,
    SubjectId = m_rsc:rid(z_context:get_q(<<"subject_id">>, Context), Context),
    ObjectId = m_rsc:rid(z_context:get_q(<<"object_id">>, Context), Context),
    Predicate = z_convert:to_binary(z_context:get_q(<<"predicate">>, Context, <<>>)),
    PredicateId = m_rsc:rid(Predicate, Context),
    TextL = lists:foldl(
        fun(Q, Acc) ->
            case z_context:get_q(Q, Context) of
                <<>> -> Acc;
                undefined -> Acc;
                V -> case Acc of
                        [] -> V;
                        _ -> [ V, " ", Acc ]
                     end
            end
        end,
        [],
        [
            <<"find_text">>, <<"title">>, <<"new_rsc_title">>,
            <<"name_first">>, <<"name_surname">>, <<"email">>
        ]),
    Text = iolist_to_binary(TextL),
    Category = case z_context:get_q(<<"find_category">>, Context) of
        undefined -> z_context:get_q(<<"category_id">>, Context);
        <<>> -> z_context:get_q(<<"category_id">>, Context);
        Cat -> Cat
    end,
    Cats = case z_convert:to_binary(Category) of
                <<"p:", PredicateName/binary>> ->
                    feedback_categories(SubjectId, PredicateName, ObjectId, Context);
                <<>> when PredicateId =/= undefined ->
                    feedback_categories(SubjectId, Predicate, ObjectId, Context);
                <<>> -> [];
                <<"*">> -> [];
                CIds ->
                    CatIds = binary:split(CIds, <<",">>, [ global ]),
                    [ m_rsc:rid(CatId, Context) || CatId <- CatIds, CatId =/= <<>> ]
           end,
    Vars = [
        {intent, z_context:get_q(<<"intent">>, Context)},
        {creator_id, CreatorId},
        {subject_id, SubjectId},
        {cat, Cats},
        {cat_exclude, z_context:get_q(<<"cat_exclude">>, Context)},
        {predicate, Predicate},
        {text, Text},
        {is_multi_cat, length(Cats) > 1},
        {category_id, case Cats of
            [CId] -> CId;
            _ -> undefined
        end},
        {is_zlink, z_convert:to_bool( z_context:get_q(<<"is_zlink">>, Context) )}
    ] ++ case z_context:get_q(<<"find_cg">>, Context) of
        undefined -> [];
        <<>> -> [];
        <<"me">> -> [];
        CgId -> [ {content_group, m_rsc:rid(CgId, Context)}]
    end,
    case Trigger of
        <<"dialog-connect-find">> ->
            z_render:wire([
                {remove_class, [{target, TargetId}, {class, "loading"}]},
                {update, [{target, TargetId}, {template, "_action_dialog_connect_tab_find_results.tpl"} | Vars]}
            ], Context);
        <<"dialog-new-rsc-tab">> ->
            z_render:wire([
                {remove_class, [{target, TargetId}, {class, "loading"}]},
                {update, [{target, TargetId}, {template, "_action_dialog_new_rsc_tab_find_results.tpl"} | Vars]}
            ], Context)
    end;

event(#postback{message={admin_connect_select, Args}}, Context) ->
    IsConnected = z_convert:to_bool(z_context:get_q(<<"is_connected">>, Context)),
    SelectId0 = z_context:get_q(<<"select_id">>, Context),
    SubjectId0 = proplists:get_value(subject_id, Args),
    ObjectId0 = proplists:get_value(object_id, Args),
    Predicate = proplists:get_value(predicate, Args),
    Callback = proplists:get_value(callback, Args),
    Intent = proplists:get_value(intent, Args),
    IsConnectToggle = z_convert:to_bool( proplists:get_value(is_connect_toggle, Args) ),

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

    SelectId = m_rsc:rid(SelectId0, Context),
    {SubjectId, ObjectId} =
        case z_utils:is_empty(ObjectId0) of
            true ->
                {m_rsc:rid(SubjectId0, Context),
                 SelectId};
            false ->
                {SelectId,
                 m_rsc:rid(ObjectId0, Context)}
        end,

    % Only disconnect if connection is not made from tinymce
    IsUnlink = case IsConnectToggle of
        false -> false;
        true -> IsConnected
    end,
    OptPredicate = case Intent of
        <<"select">> when is_integer(SubjectId) -> refers;
        <<"select">> -> undefined;
        _ -> Predicate
    end,
    case do_link_unlink(IsUnlink, SubjectId, OptPredicate, ObjectId, Callback, Context) of
        {ok, Context1} ->
            Context2 = case z_convert:to_bool(proplists:get_value(autoclose, Args)) of
                            true -> z_render:dialog_close(Context1);
                            false -> Context1
                       end,
            Actions1 = lists:map(
                fun(Action) ->
                    z_render:action_with_args(Action, [ {select_id, ObjectId} ])
                end,
                [Actions]),
            z_render:wire(Actions1, Context2);
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

event(#postback{message = {admin_rsc_redirect, Args}}, Context) ->
    SelectId = m_rsc:rid(z_context:get_q(<<"select_id">>, Context), Context),
    Dispatch = case proplists:get_value(redirect, Args) of
        true -> admin_edit_rsc;
        false -> admin_edit_rsc;
        undefined -> admin_edit_rsc;
        D when is_atom(D) -> D
    end,
    z_render:wire({redirect, [ {dispatch, Dispatch}, {id, SelectId} ]}, Context);

event(#submit{ message={admin_note_update_rsc, Args} }, Context) ->
    {id, Id} = proplists:lookup(id, Args),
    Note = z_context:get_q(<<"note">>, Context),
    case m_admin_note:update_rsc(Id, Note, Context) of
        ok ->
            OnSuccess = proplists:get_all_values(on_success, Args),
            z_render:wire(OnSuccess, Context);
        {error, enoent} ->
            z_render:growl_error(?__("Sorry, that page is unknown.", Context), Context);
        {error, eacces} ->
            z_render:growl_error(?__("Sorry, you are not allowed to edit notes.", Context), Context)
    end;
event(#postback{ message={admin_note_delete_rsc, Args} }, Context) ->
    {id, Id} = proplists:lookup(id, Args),
    case m_admin_note:delete_rsc(Id, Context) of
        ok ->
            OnSuccess = proplists:get_all_values(on_success, Args),
            z_render:wire(OnSuccess, Context);
        {error, enoent} ->
            z_render:growl_error(?__("Sorry, that page is unknown.", Context), Context);
        {error, eacces} ->
            z_render:growl_error(?__("Sorry, you are not allowed to edit notes.", Context), Context)
    end;

event(#submit{ message = {dropbox_upload, _Args} }, Context) ->
    case z_acl:is_admin(Context) orelse z_acl:is_allowed(use, mod_mailinglist, Context) of
        true ->
            #upload{
                tmpfile = TmpFile,
                filename = Filename
            } = z_context:get_q(<<"file">>, Context),
            case sanitize_filename(Filename, <<>>) of
                <<>> ->
                    z_render:growl_error(?__("Sorry, could not handle this filename.", Context), Context);
                Filename1 ->
                    Size = filelib:file_size(TmpFile),
                    Path = z_path:files_subdir_ensure("dropbox", Context),
                    Filepath = filename:join(Path, Filename1),
                    Result = case file:rename(TmpFile, Filepath) of
                        ok ->
                            ok;
                        {error, exdev} ->
                            case file:copy(TmpFile, Filepath) of
                                {ok, _} -> ok;
                                {error, _} = Error -> Error
                            end;
                        {error, _} = Error ->
                            Error
                    end,
                    case Result of
                        ok ->
                            ?LOG_INFO(#{
                                in => zotonic_mod_admin,
                                text => <<"DFile upload accepted">>,
                                result => ok,
                                filename => Filename,
                                dropbox_filename => Filename1,
                                path => Filepath,
                                size => Size
                            }),
                            z_render:growl(?__("File uploaded to the drop folder, will be handled shortly.", Context), Context);
                        {error, Reason} ->
                            ?LOG_INFO(#{
                                in => zotonic_mod_admin,
                                text => <<"File upload error">>,
                                result => error,
                                reason => Reason,
                                filename => Filename,
                                dropbox_filename => Filename1,
                                path => Filepath,
                                size => Size
                            }),
                            z_render:growl_error(?__("Sorry, could not move the file to the drop folder.", Context), Context)
                    end
            end;
        false ->
            z_render:growl_error(?__("Sorry, you are not allowed to upload drop folder files.", Context), Context)
    end;

event(#submit{ message = {delete_all, Args}}, Context) ->
    case proplists:get_value(ids, Args) of
        Ids when is_list(Ids) ->
            lists:foreach(
                fun(Id) ->
                    case m_rsc:delete(Id, Context) of
                        ok ->
                            ok;
                        {error, Reason} ->
                            ?LOG_WARNING(#{
                                in => zotonic_mod_admin,
                                text => <<"Error during bulk delete of resources">>,
                                id => Id,
                                result => error,
                                reason => Reason
                            })
                    end
                end,
                Ids),
            z_render:wire(proplists:get_all_values(on_success, Args), Context);
        _ ->
            Context
    end;

event(#submit{ message = {update_all, Args}}, Context) ->
    case proplists:get_value(ids, Args) of
        Ids when is_list(Ids) ->
            Props = z_context:get_q_all_noz(Context),
            Update = lists:foldl(
                fun
                    ({_P, <<>>}, Acc) ->
                        Acc;
                    ({P, V}, Acc) ->
                        Acc#{
                            P => V
                        }
                end,
                #{},
                Props),
            lists:foreach(
                fun(Id) ->
                    case m_rsc:update(Id, Update, Context) of
                        {ok, _} ->
                            ok;
                        {error, Reason} ->
                            ?LOG_WARNING(#{
                                in => zotonic_mod_admin,
                                text => <<"Error during bulk update of resources">>,
                                id => Id,
                                update => Update,
                                result => error,
                                reason => Reason
                            })
                    end
                end,
                Ids),
            z_render:wire(proplists:get_all_values(on_success, Args), Context);
        _ ->
            Context
    end;

event(#postback{ message = {ensure_refers, _} }, Context) ->
    case z_acl:is_admin(Context) of
        true ->
            z_admin_refers:insert_ensure_refers_all_task(Context),
            z_render:growl(?__("Scheduled a background task to check all refers connections.", Context), Context);
        false ->
            z_render:growl_error(?__("Sorry, only an admin is allowed to do this", Context), Context)
    end;

event(#postback{ message = {delete_tasks, Args} }, Context) ->
    case z_acl:is_admin(Context) of
        true ->
            {module, Module} = proplists:lookup(module, Args),
            {function, Function} = proplists:lookup(function, Args),
            z_pivot_rsc:delete_task(Module, Function, Context),
            z_render:growl(?__("Deleted tasks.", Context), Context);
        false ->
            z_render:growl_error(?__("Sorry, only an admin is allowed to do this", Context), Context)
    end;

event(_E, Context) ->
    ?DEBUG(_E),
    Context.

language_list(undefined) ->
    [];
language_list(<<>>) ->
    [];
language_list(B) when is_binary(B) ->
    language_list(binary:split(B, <<",">>, [global, trim_all]));
language_list(L) ->
    lists:filtermap(
        fun(Lang) ->
            case z_language:to_language_atom(Lang) of
                {ok, Code} -> {true, Code};
                {error, _} -> false
            end
        end,
        L).

sanitize_filename(<<>>, Acc) ->
    Acc;
sanitize_filename(<<C/utf8, Rest/binary>>, Acc) ->
    case filechar_ok(C) of
        true -> sanitize_filename(Rest, <<Acc/binary, C/utf8>>);
        false -> sanitize_filename(Rest, <<Acc/binary, $->>)
    end.

filechar_ok($.) -> true;
filechar_ok($-) -> true;
filechar_ok($+) -> true;
filechar_ok($_) -> true;
filechar_ok($=) -> true;
filechar_ok($() -> true;
filechar_ok($)) -> true;
filechar_ok($#) -> true;
filechar_ok($@) -> true;
filechar_ok($<) -> true;
filechar_ok($>) -> true;
filechar_ok($|) -> true;
filechar_ok($^) -> true;
filechar_ok($\ ) -> true;
filechar_ok(C) when C >= $0, C =< $9 -> true;
filechar_ok(C) when C >= $a, C =< $z -> true;
filechar_ok(C) when C >= $A, C =< $Z -> true;
filechar_ok(C) when C > 128 -> true;
filechar_ok(_) -> false.


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

do_link_unlink(_IsUnlink, SubjectId, Predicate, ObjectId, Callback, Context)
    when Predicate =:= "";
         Predicate =:= <<>>;
         Predicate =:= undefined;
         SubjectId =:= undefined;
         ObjectId =:= undefined ->
     ContextP = context_language(Context),
    Title = m_rsc:p(ObjectId, title, Context),
    Vars = [
            {subject_id, undefined},
            {predicate, undefined},
            {object_id, ObjectId},
            {url_language, m_rsc:p(ObjectId, page_url, ContextP)},
            {title_language, z_trans:lookup_fallback(Title, ContextP)},
            {title, z_trans:lookup_fallback(Title, Context)},
            {is_media, m_rsc:is_a(ObjectId, media, Context)},
            {is_document, m_rsc:is_a(ObjectId, document, Context)},
            {is_image, m_rsc:is_a(ObjectId, image, Context)}
           ],
    case Callback of
        undefined -> {ok, Context};
        "" -> {ok, Context};
        <<>> -> {ok, Context};
        {CB, Args} ->
            {ok, z_render:wire({script, [{script, [
                    z_sanitize:ensure_safe_js_callback(CB), $(,
                        z_utils:js_object(Vars++Args,Context),
                    $),$;
                ]}]}, Context)};
        CB ->
            {ok, z_render:wire({script, [{script, [
                    z_sanitize:ensure_safe_js_callback(CB), $(,
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
    Title = case m_rsc:p(ObjectId, title, Context) of
        undefined -> ?__("Untitled", Context);
        T -> T
    end,
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
        undefined -> {ok, Context1};
        "" -> {ok, Context1};
        <<>> -> {ok, Context1};
        _ ->
            Vars = [
                    {is_new, IsNew},
                    {is_delete, IsDelete},
                    {subject_id, SubjectId},
                    {predicate, Predicate},
                    {object_id, ObjectId},
                    {edge_id, EdgeId},
                    {url_language, m_rsc:p(ObjectId, page_url, ContextP)},
                    {title_language, z_trans:lookup_fallback(Title, ContextP)},
                    {title, z_trans:lookup_fallback(Title, Context)}
                   ],
            case Callback of
                    {CB, Args} ->
                        {ok, z_render:wire({script, [{script, [
                                 z_sanitize:ensure_safe_js_callback(CB), $(,
                                    z_utils:js_object(Vars++Args,ContextP),
                                $),$;
                            ]}]}, Context1)};
                    CB ->
                        {ok, z_render:wire({script, [{script, [
                                 z_sanitize:ensure_safe_js_callback(CB), $(,
                                    z_utils:js_object(Vars,ContextP),
                                $),$;
                            ]}]}, Context1)}
            end
    end.

context_language(Context) ->
    case z_context:get_q(<<"language">>, Context) of
        undefined -> Context;
        <<>> -> Context;
        Lang ->
            case z_language:to_language_atom(Lang) of
                {ok, LanguageCode} -> z_context:set_language(LanguageCode, Context);
                {error, _} -> Context
            end
    end.

manage_schema(_Version, Context) ->
    m_admin_note:install(Context),
    #datamodel{
        categories = [
            {admin_content_query, 'query', #{
                <<"title">> => #trans{ tr = [
                    {en, <<"Admin content query">>},
                    {nl, <<"Admin inhoud zoekopdracht">>}
                ]}
            }}
        ],
        predicates = [
            {refers, #{
                <<"title">> => #trans{
                    tr = [
                        {en, <<"Refers">>},
                        {nl, <<"Refereert">>}
                    ]
                },
                <<"is_object_noindex">> => true,
                <<"is_connections_hide">> => true
            }, []}
        ]
    }.
