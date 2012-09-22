
Inserts HTML after the contents of an HTML element.

Adds a template or a literal HTML text after the existing content.

Example::

   <div id="mydiv"><p>Bye Bye.</p></div>
   {% button text="hello" action={insert_bottom target="mydiv" text="<p>Hello World!</p>"} %}

After the button is clicked, the contents of the div will be `<p>Bye Bye.</p><p>Hello World!</p>`.

Another example, now rendering a template::

   <ul id="mylist"><li>Some item</li></li>
   {% button text="hello" action={insert_bottom target="mylist" template="_list_item.tpl" id=42} %}

This insert the output of the template `_list_item.tpl` below the existing `<li/>`.  All arguments to the update action are also arguments to the template.

.. seealso:: actions :ref:`action-insert_after`, :ref:`action-insert_before`, :ref:`action-insert_top` and :ref:`action-update`.

`Edit <https://github.com/zotonic/zotonic/edit/master/doc/ref/actions/doc-insert_bottom.rst>`_
