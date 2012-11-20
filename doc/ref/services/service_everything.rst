
.. include:: meta-everything.rst

Returns a single array with every id of every :term:`Resource` in the
database::

  $ curl http://localhost:8000/api/base/everything

  [1,101,102,103,104,106,107,108,109,110,111,112,113,114,115,116,117,119,120,121,122,123,300,301,303,...


Due to performance reasons, these results are cached in-memory for one
hour.
