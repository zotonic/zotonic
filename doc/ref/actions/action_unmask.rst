.. highlight:: django
.. include:: meta-unmask.rst
.. seealso:: action :ref:`action-mask`.

Removes a mask that was placed over an element using the :ref:`action-mask` action.

Example::

   {% wire action={unmask target="logon_outer"} %}

In this example the mask over the `logon_outer` div will be removed.
