
Toggle a CSS class from an HTML element.

Example::

   {% button action={toggle_class target="myid" class="newclass"} %}

When the HTML element with id "myid" has the CSS class "newclass" then it is removed, otherwise it is added.

.. seealso:: actions :ref:`action-add_class` and :ref:`action-remove_class`.
