.. highlight:: django
.. include:: meta-add_class.rst
.. seealso:: actions :ref:`action-remove_class` and :ref:`action-toggle_class`.

Add a css class to an html element.

Example::

   {% button action={add_class target="myid" class="newclass"} %}

Adds the CSS class "newclass" to the element with HTML id "myid".
