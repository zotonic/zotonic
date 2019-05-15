
.. include:: meta-to_name.rst

Map a string to a *name*. That is a lowercased string with only ``[a-z0-9_]`` characters.

Example::

  {{ "Hello World!"|to_name }}

Results in the string value ``hello_world``.
