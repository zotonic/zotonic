
.. include:: meta-search.rst

Search Zotonic’s :term:`resources <resource>` using the
:ref:`manual-datamodel-query-model`.

For instance, the API call::

  http://localhost:8000/api/search?cat=text&text=test

Returns a JSON list of all resource ids of the :ref:`category
<manual-datamodel-categories>` `text` that contain the string `test`::

  [320]

.. seealso:: :ref:`manual-datamodel-query-model`

             
