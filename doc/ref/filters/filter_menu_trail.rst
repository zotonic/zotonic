.. highlight:: django
.. include:: meta-menu_trail.rst

Return a breadcrumb navigation trail for the given id.

This filter locates the filter value which represents the current page
in the main menu or in the menu saved in the resource of the given id.

For example::

  {% print id|menu_trail:55 %}

Could print the list ``[13, 33]`` if ids 13 and 33 are the parents of
the `id` argument in the menu resource 55.

If no argument is given, it takes menu from the resource with the name
``main_menu``.


Showing Menu Trail only for submenu items
-----------------------------------------

It is sometimes useful to suppress the menu trail on top level
items. Here is how to do it.

The :ref:`filter-menu_trail` includes the whole path through the menu
to the current page if it is reachable that way. Sometimes it may seem
pointless to show the menu trail if we are on the first level of the
menu.  If we want to avoid this we need to avoid rendering a trail
when it is less than two items long.

One simple condition change to `_article_chapeau.tpl` from the `blog`
skeleton makes this work::

  {% with id|menu_trail as parents %}
    {% if parents|length > 1 %}
    <h5 class="chapeau">
    {% for p in parents %}
    <a href="{{ m.rsc[p].page_url }}">{{ m.rsc[p].title }}</a>
    {% if not forloop.last %}&raquo;{% endif %}
  {% endfor %}</h5>{% endif %}{% endwith %}

The key here is ``{% if parents|length > 1 %}`` in place of just ``{% if parents %}``.

The :ref:`tag-if` tag is now rendering the menu_trail only if there
are two or more items in it which - as I mentioned before - happens
when you are at least two levels deep in the menu.


.. seealso:: :ref:`filter-menu_subtree`, :ref:`filter-menu_flat`
