
.. include:: meta-persistent_get.rst

Retrieve a value from the Zotonic visitor record.

The visitor record’s value for the ``key`` argument is returned::

  http://localhost:8000/api/base/persistent_get?key=foo

might return::

  {
      "key": "foo",
      "value": null
  }

If the value is undefined; or ::

  {
      "key": "foo",
      "value": "bar"
  }

If the value was set.

.. seealso:: :ref:`service-persistent_set`
