
.. include:: meta-mod_admin.rst

.. todo:: Finish documentation
.. role:: gray

Extending the admin menu
------------------------

See :ref:`model-admin_menu` on how to extend the admin menu.


Extending the admin edit page
-----------------------------

There are several special templates names that will be automatically
included into the `/admin/edit/xxx` page from when you create these
specially named templates.

``_admin_edit_basics.tpl``
  Will be automatically included into main (left) div (at top).

``_admin_edit_content.tpl``
  Will be automatically included into main (left) div (at bottom).

``_admin_edit_sidebar.tpl``
  Will be automatically included into right sidebar (near middle/bottom).

These templates are included using the :ref:`tag-all-catinclude` tag; so
if you need something in the sidebar just for persons, create a
``_admin_edit_sidebar.person.tpl`` file in your project.


Overriding TinyMCE options
``````````````````````````

If you need to override TinyMCE options; adding plugins, or setting
other settings; you can create an ``_admin_tinymce_overrides_js.tpl``
file which can contain extra settings for the TinyMCE editors in the
admin.

.. highlight:: javascript

The template must contain JavasSript which modifies the `tinyInit`
variable just before the editor is started. For example, to tweak the
"paste" options you can put the following in the template::

  tinyInit.theme_advanced_blockformats = "p,h1,h2"
  tinyInit.paste_auto_cleanup_on_paste = true;
  tinyInit.paste_remove_styles = true;
  tinyInit.paste_remove_styles_if_webkit = true;
  tinyInit.paste_strip_class_attributes = true;
  tinyInit.paste_text_sticky = true;
  tinyInit.paste_text_sticky_default = true;


TinyMCE Zotonic options
```````````````````````

Zotonic provides extra init options:

``z_insert_dialog_enabled``
  Set this to false to prevent the insert media dialog from showing. Default `true`.

``z_properties_dialog_enabled``
  Set this to false to prevent the media properties dialog from showing. Default `true`.


Writing admin widget templates
------------------------------

This section contains examples of templates to create widgets for the
/admin. Each of these examples extends basic several widget templates
from mod_admin. To write your own you need to drop example content and
fill holes in these example widgets.

You can use them as basis for your site admin-related tasks.

``_admin_dashboard_example.tpl``
  Very simple example widget for admin dashboard. Contains blocks for title and body.
  Look at /admin to see several dashboard widgets (latest events, pages, media, etc).

``_admin_widget_std.tpl``
  Sligthly more complex widget example. Same templates are used into /admin/edit/N for
  main content and sidebar widgets. These widgets do not provide any localization
  abilities. Also note that there are several special widget names:

``_admin_widget_i18n.tpl``
  Complex widget example. Is used to edit localized rsc properties. It will be rendered
  as tabs. See /admin/edit/N top left to see the tabs. If mod_translation disabled, then
  i18n-widgets are displayed same as _admin_widget_std.tpl.


Making an admin widget conditionally visible
--------------------------------------------

.. highlight:: django

To make an entire admin widget visible or not, depending on some
condition that you want to calculate inside the widget’s code, you can
use the `widget_wrapper` block (which sits around the entire widget)
in combination with the :ref:`tag-inherit` tag, wrapping that with a
condition.

For instance, :ref:`mod_backup` uses this technique to display the
import/export sidebar widget. Excerpt from mod_backup’s `_admin_edit_sidebar.tpl`::

    {# Make the widget conditional, based on the config value mod_backup.admin_panel #}
    {% block widget_wrapper %}
        {% if m.config.mod_backup.admin_panel.value %}
            {% inherit %}
        {% endif %}
    {% endblock %}

In this example, when the condition is true, the wrapper is rendered normally (content is inherited from the extended template); when false, the wrapper block is overridden by new (but void) content.


.. seealso:: :ref:`template-admin_edit_widget_i18n`, :ref:`tag-inherit`


Extending the admin overview page
---------------------------------

The overview page at ``admin/overview`` shows a table with links to all pages. It can be filtered and sorted. This extension deals with the view when a category filter has been selected.

The goal is to add more specific information that helps to distinguish pages.

The `Category` column, second from the left, can be extended to carry category-specific information. As an example, pages of category Event show the start date for each event, grayed out when the event is in the past. Something like this:

    ============ ===========================  ===================
    Title        Starts                       Created on
    ============ ===========================  ===================
    Great event  2016-08-27 00:00:00          25 Aug 2015, 22:19
    Past event   :gray:`2014-01-02 00:00:00`  01 Jan 2014, 13:10
    ============ ===========================  ===================

Instead of dates, the information can be anything - from color coding labels to location data, the number of comments or the completeness state of product descriptions.

Setting up templates
````````````````````

To make it work we are using 3 templates (where `category_name` is the lowercase name of your category):

``_admin_overview_list.category_name.tpl``

    Overrides the overview with a ``field`` variable for our custom sort. If we are using :ref:`an existing resource property<model-rsc>` such as ``date_start``, we write::

      {% include "_admin_overview_list.tpl"
        field="rsc.pivot_date_start"
      %}

``_admin_sort_header.category_name.tpl``

    The sort header caption. For a non-sortable header, just write the caption as text. For a sortable header, include the sort functionality in ``_admin_sort_header.tpl`` and pass the caption as variable::

        {% include "_admin_sort_header.tpl"
            caption="My caption"
            type="date"
        %}

``type="date"`` indicates that sorting should start descending, from new to old.


``_admin_overview_list_data.category_name.tpl``

    Contains the category-specific information. This is a freeform Zotonic template. The Events example checks if the end date is in the past::

        {% if id.date_end|in_past %}
            <span class="text-muted">{{ id.date_start|timesince }}</span>
        {% else %}
            <span>{{ id.date_start|timesince }}</span>
        {% endif %}


Custom sort properties
``````````````````````

To sort on values that are not stored in the default Zotonic resources, you will need to create a :ref:`custom pivot <cookbook-custom-pivots>`. This will create an additional database table with the values to sort on.

Let's take the (unlikely) example where we want to display the summary of each page (and sort on it as well). The summary data is not stored in an easily accessible way (at least for sorting), so we need to add 2 pivot methods to our Erlang module::

    init(Context) ->
        z_pivot_rsc:define_custom_pivot(?MODULE, [{summary, "binary"}], Context),
        ok.

    observe_custom_pivot({custom_pivot, Id}, Context) ->
        case m_rsc:p(Id, summary, Context) of
            {trans, [{en, Summary}]} ->
                {?MODULE, [{summary, Summary}]};
            _ -> none
        end.

For this example we are just grabbing the English text, and assume that other translations exist.

The ``field`` name in ``_admin_overview_list.category_name.tpl`` now just needs to contain the pivot column name::

    {% include "_admin_overview_list.tpl"
        field="summary"
    %}

And the sort header template ``_admin_sort_header.category_name.tpl`` adds the custom pivot variable::

    {% include "_admin_sort_header.tpl"
        caption="Summary"
        custompivot="my_module"
    %}


Resource meta `features`
------------------------

Resources in the meta category can have 'features': certain resource
properties (usually in the form of checkboxes) that decide what to
show or hide on certain pages in the admin. To use this, create a
``_admin_features.category.tpl`` in your module.

For instance, the module `mod_geomap` defines the following
``_admin_features.category.tpl`` to create an extra checkbox so that
per category can be defined whether or not the geodata box should be
shown::

  <div class="controls">
	  <label class="checkbox">
          <input value="1" type="checkbox"
                 name="feature_show_geodata"
                 {% if id.feature_show_geodata|if_undefined:`true` %}checked{% endif %}
                 />
          {_ Show geo data on edit page _}
      </label>
  </div>

And on the edit page there is this check to conditionally include the geodata box::

  {% if id.category_id.feature_show_geodata|if_undefined:`true` %}

The ``if_undefined`` is used so that the default value can be true
when the checkbox has never been touched.


Configuration keys
------------------

For the admin there are two configuration keys: ``mod_admin.rsc_dialog_tabs`` and ``mod_admin.rsc_dialog_is_published``.

The ``mod_admin.rsc_dialog_tabs`` key defines which tabs are shown in the new resource, media-upload, and image-link dialogs.
Per defauls these dialogs show all the possible tabs, with this configurarion key it is possible to change that.

The tabs are: ``find,new,upload,url,embed,oembed,depiction``

The ``depiction`` is used for the TinyMCE image-link dialog; it shows all media connected using the ``depiction`` predicate.

The ``mod_admin.rsc_dialog_is_published`` defines the default *is_published* state for new resources being mad in the *new* tab.
Setting this key to `1` will check the *is_published* checkbox.

The ``mod_admin.edge_list_max_length`` defines the maximum number of connections shown per predicate in the
connection list sidebar. If there are more connections then the list truncated, and the message _Too many connections, only the first and last are shown._ is displayed. The default is 100 connections.


.. seealso:: :ref:`filter-if_undefined`
