.. highlight:: django
.. include:: meta-overlay_close.rst
.. seealso:: actions :ref:`action-overlay_open`, :ref:`action-dialog_open` and :ref:`action-dialog`.


Closes the currently open overlay. When there is no overlay open then nothing happens.

Example::

   {% button text="cancel" action={overlay_close} %}

This button closes any open overlay when clicked.
