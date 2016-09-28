
.. include:: meta-search.rst

Search Zotonic’s :term:`resources <resource>` using the
:ref:`guide-datamodel-query-model`.

For instance, the API call:

.. code-block:: none

  http://localhost:8000/api/search?cat=text&text=test

Returns a JSON list of all resource ids of the :ref:`category
<guide-datamodel-categories>` ``text`` that contain the string ``test``::

  [320]

Adding ``&format=simple`` to the API call gives us a list of JSON objects::

  [
    {
        "category": [
            "text"
        ],
        "id": 338,
        "preview_url": "http://example.dev:8000/image/2014/10/8/image_2014_09_09_19_19_41.png%28800x800%29%28upscale%29%28CF8AD1D93AC1B8457F9AD6B9BA64C74F%29.jpg",
        "summary": {
            "trans": {
                "en": "English summary",
                "es": "Spanish summary"
            }
        },
        "title": {
            "trans": {
                "en": "English",
                "es": "Hola hola Espanol"
            }
        }
    }
  ]


Parameters
----------

You can enter all parameters that are in the standard :ref:`guide-datamodel-query-model`.

Besides, the following parameters exist:

**limit**

  The number of results to return. Defaults to 20; maximum number to
  return per call is 1000. To return more, do multiple requests with
  the ``offset`` parameter.

**offset**

  Start offset for the result set.

**format**

  Either ``ids`` to return a plain array of rsc ids; or ``simple`` to
  return a list of JSON objects with the resource's `title`,
  `summary`, `category`, and `preview_url` (the first image).

.. seealso:: :ref:`guide-datamodel-query-model`


