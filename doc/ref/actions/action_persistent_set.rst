
.. include:: meta-persistent_set.rst

Set a persistent variable.

Example::

   {% button action={persistent_set key="foo" value="bar"} %}

This sets the session variable "foo" to the value "bar". In your templates, you can use :ref:`model-persistent` to retrieve this value again::

  {{ m.persistent.foo }}

``key`` and ``value`` are both required values.
  
