.. highlight:: django
.. include:: meta-remove_class.rst
.. seealso:: actions :ref:`action-add_class` and :ref:`action-toggle_class`.

Remove a CSS class from an HTML element.

Example::

   {% button action={remove_class target="myid" class="newclass"} %}

Removes the CSS class "newclass" from the element with HTML id "myid".

