
.. include:: meta-mod_tkvstore.rst

Simple (type,key)/value store. Stores data in the store with minimal
latency and (local) serialization of get/put requests.

The store itself is implemented as a PostgreSQL table with columns
`type`, `key` and `props`. `props` holds the value for the given
type+key combination.

A model, :ref:`model-tkvstore`, is provided to give easy access to the
type+key combinations from the templates, and to perform get/put
operations from within Erlang.

.. seealso:: :ref:`model-tkvstore`
