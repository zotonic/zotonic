
.. include:: meta-tkvstore.rst

Simple read-only interface to the typed key-value store of :ref:`mod_tkvstore`.
To get a value from the store: use `m.tkvstore.type.key`, like this::

  {% print m.tkvstore.keytype.examplekey %}

When the key is not a literal value but a variable or a number, use
the following notation::

  {% print m.tkvstore.keytype[keyvar] %}

To store data in a key, use ``m_tkvstore:put/4``, as follows::

  m_tkvstore:put(KeyType, KeyVar, Value, Context).

For instance, from within Erlang, let’s store `edge data` for a given
edge id, 3434::

  Edgeid = 3434,
  Value = <<"Hello">>,
  m_tkvstore:put(edge_data, EdgeId, Value, Context).

Now, to retrieve it in the template, do the following::

  {{ m.tkvstore.edge_data[3434] }}

This will output the string ``Hello``.

Note that the value can be any type: not only a simple string but also
a list, tuple, or any other Erlang composite type.

.. seealso:: :ref:`mod_tkvstore`

