
.. include:: meta-disable.rst


Sets the "disabled" attribute of a HTML tag and adds the CSS class "disabled".

Example::

   <input id="myid" type="text" value="hello" />
   {% button text="disable" action={disable target="myid"} %}

After clicking the button the input will be::

  <input id="myid" disabled="disabled" class="disabled" type="text" value="hello" />

.. seealso:: action :ref:`action-enable`.
