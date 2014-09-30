
.. include:: meta-everything.rst

Returns a single array with every id of every :term:`Resource` in the
database::

  $ curl http://localhost:8000/api/base/everything

  [1,101,102,103,104,106,107,108,109,110,111,112,113,114,115,116,117,119,120,121,122,123,300,301,303,...

The results are paged per maximum 100 items, supply a `page` argument to fetch another page than page number 1::

  $ curl http://localhost:8000/api/base/everything?page=10

  [4169,4170,4171,4172,4173,4174,4175,4176,4177,4178,4179,4180,4181,4182,4183,4184,4185,4186,4187,4188,4189,4190,4191,4192,...

Due to access control restrictions it is possible that less than 100 ids are returned for a page.
