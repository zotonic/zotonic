
.. include:: meta-notifier.rst

Use the :ref:`notification system <manual-notification>` from your templates.

This model makes it possible to user the power of the notifier mechanism inside your templates.


m.notifier.first
................

Send the message between the square brackets to the notifier
system. The value of the first reaction is used. The order in which
the modules are called is based on the priority of the modules.

When rendered the templates sends a notification with the message
``banner`` to all listeners, rendering the first reaction::

  {{ m.notifier.first[`banner`] }}

A module would implement the following callback::

  observe_banner(banner, _Context) ->
      <<"<h1>This is a banner</h1>">>.

m.notifier.map
..............

Send the message between the brackets to the notifier message. All
reactions are returned in a list. The order in which the modules are
called is based on the priority of the module.

Here is an example of the use of map to insert footer navigation
items. This works as follows. When the template is rendered the
zotonic notifier is called with the message ``{footer_navigation,
[{current_location=Id}]}``. Zotonic modules can listen to this message
and return this information. This can be very handy if you have to
create dynamic interfaces based on what modules are enabled::

  {% for item in m.notifier.map[{footer_navigation current_location=id}] %}
      {% include "footer_item.tpl" item=item %}
  {% endfor %}

