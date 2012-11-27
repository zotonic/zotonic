
.. include:: meta-meta.rst

Returns meta-information about all available API calls::

  http://localhost:8000/api/base/meta

Returns a JSON object like the following::

  [
    {
        "method": "search/search",
        "module": "mod_search",
        "service": "service_search_search",
        "title": "Search Zotonic resources.",
        "needauth": false,
        "http": "GET,HEAD"
    },
    {
        "method": "geomap/locations",
        "module": "mod_geomap",
        "service": "service_geomap_locations",
        "title": "List locations of a collection, search_query or search parameters.",
        "needauth": false,
        "http": "GET,HEAD"
    },
    {
        "method": "geomap/countries",
        "module": "mod_geomap",
        "service": "service_geomap_countries",
        "title": "JSON for the country map, colours and country id per country.",
        "needauth": false,
        "http": "GET,HEAD"
    },
    {
        "method": "base/meta",
        "module": "mod_base",
        "service": "service_base_meta",
        "title": "Meta-information about all API calls.",
        "needauth": false,
        "http": "GET,HEAD"
    },
    ...
  ]

