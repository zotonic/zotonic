.. highlight:: django
.. include:: meta-enable.rst
.. seealso:: action :ref:`action-disable`.

Resets the "disabled" attribute of a HTML tag and removes the CSS class "disabled".

Example::

   <input id="myid" disabled="disabled" class="disabled" type="text" value="hello" />
   {% button text="enable" action={enable target="myid"} %}

After clicking the button the input will be::

   <input id="myid" class="" type="text" value="hello" />
