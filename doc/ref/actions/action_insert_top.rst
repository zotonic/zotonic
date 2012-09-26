
.. include:: meta-insert_top.rst


Inserts HTML before the contents of an HTML element.

Adds a template or a literal HTML text before the existing content.

Example::

   <div id="mydiv"><p>Bye Bye.</p></div>
   {% button text="hello" action={insert_top target="mydiv" text="<p>Hello World!</p>"} %}

After the button is clicked, the contents of the div will be `<p>Hello World!</p><p>Bye Bye.</p>`.

Another example, now rendering a template::

   <ul id="mylist"><li>Some item</li></li>
   {% button text="hello" action={insert_top target="mylist" template="_list_item.tpl" id=42} %}

This insert the output of the template `_list_item.tpl` above the existing `<li/>`.  All arguments to the update action are also arguments to the template.

.. seealso:: actions :ref:`action-insert_after`, :ref:`action-insert_before`, :ref:`action-insert_bottom` and :ref:`action-update`.
