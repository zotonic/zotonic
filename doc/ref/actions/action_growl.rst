.. highlight:: django
.. include:: meta-growl.rst
.. seealso:: actions :ref:`action-alert` and :ref:`action-confirm`; and :ref:`guide-cookbook-frontend-growl`.


Show a message in the upper right corner of the browser window. The message will automatically disappear after some time.

Example::

   {% button action={growl text="hello world"} %}

Shows a message with the text "hello world".

Growl accepts the following arguments:

========  ================================  ============
Argument  Description                       Example
========  ================================  ============
text      The text to be displayed.         text="Hola!"
stay      When true then the message
          does not disappear automatically  stay
type      Type of the message, one of
          "notice" or "error".
          Default is "notice".              type="error"
========  ================================  ============
