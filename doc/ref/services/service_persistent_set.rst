
.. include:: meta-persistent_set.rst

Set a value in the Zotonic visitor record.

Required arguments are ``key`` and ``value``.

  http://localhost:8000/api/base/persistent_set?key=foo&value=bar

Returns a key/value tuple to confirm that the value has been set::

  {
      "key": "foo",
      "value": "bar"
  }

.. seealso:: :ref:`service-persistent_get`
               
