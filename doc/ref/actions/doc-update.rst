
Updates the content of an HTML element with a template or a literal HTML text.

Example::

   <div id="mydiv"><p>Bye Bye.</p></div>
   {% button text="hello" action={update target="mydiv" text="<p>Hello World!</p>"} %}

When clicked, the contents of the div will be set to the HTML fragment `<p>Hello World!</p>`. This replaces any content present.

Another example, now rendering a template::

  <ul id="mylist"><li>Some item</li></li>
  {% button text="hello" action={update target="mylist" template="_list_item.tpl" id=42} %}

This updates the `<ul/>` with the output of the template `_list_item.tpl`.  All arguments to the update action are also arguments to the template.

.. seealso:: actions :ref:`action-insert_top` and :ref:`action-insert_bottom`.
