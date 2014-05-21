
.. include:: meta-session_set.rst

Set a session variable.

Example::

   {% button action={session_set key="foo" value="bar"} action={reload} %}

This sets the session variable "foo" to the value "bar", and then reloads the page. In your templates, you can use :ref:`model-session` to retrieve this value again::

  {{ m.session.foo }}

``key`` and ``value`` are both required values.
  
