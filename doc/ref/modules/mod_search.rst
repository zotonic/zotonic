
.. include:: meta-mod_search.rst
.. seealso:: :ref:`guide-datamodel-query-model`, :ref:`cookbook-custom-search`

mod_search implements various ways of searching through the main
resource table using :ref:`model-search`.

Configuration
-------------

There are two :ref:`site configuration variables <ref-site-configuration>` to
tweak `PostgreSQL text search settings`_.

mod_search.rank_behaviour
^^^^^^^^^^^^^^^^^^^^^^^^^

An integer representation to influence PostgreSQL search behaviour.

Default: ``37`` (``1 | 4 | 32``)

mod_search.rank_weight
^^^^^^^^^^^^^^^^^^^^^^

A set of four numbers to override relative weights for the ABCD categories.

Default: ``{0.05, 0.25, 0.5, 1.0}``

The following searches are implemented in mod_search:

+------------------------+---------------------------------------------------------------+-------------------+
|Name                    |Description                                                    |Required arguments |
+========================+===============================================================+===================+
|query                   |Very powerful search with which you can implement almost all of|                   |
|                        |the other search functionality. See:                           |                   |
|                        |:ref:`guide-datamodel-query-model`                             |                   |
+------------------------+---------------------------------------------------------------+-------------------+
|facets                  |Performs a ``query`` (see above) and then calculates facets.   |                   |
|                        |The facets are defined using the ``pivot/facet.tpl`` template. |                   |
|                        |Facets are returned in the ``facets`` field of the search      |                   |
|                        |result. This shows per facet the counts for alternative values.|                   |
+------------------------+---------------------------------------------------------------+-------------------+
|subfacets               |Performs a ``query`` (see above) and then calculates facets.   |                   |
|                        |The facets are defined using the ``pivot/facet.tpl`` template. |                   |
|                        |Facets are returned in the ``facets`` field of the search      |                   |
|                        |result. This is a *drill down* where the facets are counted    |                   |
|                        |from the matching resources in the query. That means that only |                   |
|                        |facet values that are in the result set are shown.             |                   |
+------------------------+---------------------------------------------------------------+-------------------+
|facet_values            |Returns an empty result with the facets field set to all       |                   |
|                        |possible values per facet, or the min/max range for a range    |                   |
|                        |facet.                                                         |                   |
+------------------------+---------------------------------------------------------------+-------------------+
|featured                |List of pages, featured ones first.                            |                   |
+------------------------+---------------------------------------------------------------+-------------------+
|featured                |List of pages in a category, featured ones first.              |cat                |
+------------------------+---------------------------------------------------------------+-------------------+
|featured                |List of pages in a category having a certain object, featured  |cat, object,       |
|                        |pages first.                                                   |predicate          |
+------------------------+---------------------------------------------------------------+-------------------+
|latest                  |The newest pages.                                              |                   |
+------------------------+---------------------------------------------------------------+-------------------+
|latest                  |The newest pages within in a category.                         |cat                |
+------------------------+---------------------------------------------------------------+-------------------+
|upcoming                |Selects pages with future date_end.                            |                   |
+------------------------+---------------------------------------------------------------+-------------------+
|upcoming_on             |Selects pages with date_end after the given time.              |datetime           |
+------------------------+---------------------------------------------------------------+-------------------+
|upcoming_date           |Selects pages with date_end after the given date.              |date               |
+------------------------+---------------------------------------------------------------+-------------------+
|finished                |Selects pages with a past date_end.                            |                   |
+------------------------+---------------------------------------------------------------+-------------------+
|finished_on             |Selects pages with a date_end before the given time.           |datetime           |
+------------------------+---------------------------------------------------------------+-------------------+
|finished_date           |Selects pages with a date_end before the given date.           |date               |
+------------------------+---------------------------------------------------------------+-------------------+
|unfinished              |Selects pages with a date_end in the future.                   |                   |
+------------------------+---------------------------------------------------------------+-------------------+
|unfinished_on           |Selects pages with a date_end after the given time.            |datetime           |
+------------------------+---------------------------------------------------------------+-------------------+
|unfinished_date         |Selects pages with a date_end after the given date.            |date               |
+------------------------+---------------------------------------------------------------+-------------------+
|ongoing                 |Pages with past date_start and future date_end.                |                   |
+------------------------+---------------------------------------------------------------+-------------------+
|ongoing_on              |Pages with a date_start - date_end range around the given time.|datetime           |
+------------------------+---------------------------------------------------------------+-------------------+
|ongoing_date            |Pages with a date_start - date_end range around the given date.|date               |
+------------------------+---------------------------------------------------------------+-------------------+
|autocomplete            |Full text search where the last word gets a wildcard.          |text               |
+------------------------+---------------------------------------------------------------+-------------------+
|autocomplete            |Full text search where the last word gets a wildcard, filtered |cat, text          |
|                        |by category.                                                   |                   |
+------------------------+---------------------------------------------------------------+-------------------+
|fulltext                |Full text search. Returns ``{id,score}`` tuples.               |text               |
+------------------------+---------------------------------------------------------------+-------------------+
|fulltext                |Full text search, filtered by category. Returns ``{id,score}`` |cat, text          |
|                        |tuples.                                                        |                   |
+------------------------+---------------------------------------------------------------+-------------------+
|referrers               |All subjects of a page.                                        |id                 |
+------------------------+---------------------------------------------------------------+-------------------+
|media_category_image    |All pages with a medium and within a certain category. Used to |cat                |
|                        |find category images.                                          |                   |
+------------------------+---------------------------------------------------------------+-------------------+
|media_category_depiction|All pages with a depiction edge to an image. Used to find      |cat                |
|                        |category images.                                               |                   |
+------------------------+---------------------------------------------------------------+-------------------+
|media                   |All pages with a medium, ordered by descending creation date.  |                   |
+------------------------+---------------------------------------------------------------+-------------------+
|all_bytitle             |Return all ``{title,id}`` pairs for a category, sorted on      |cat                |
|                        |title.                                                         |                   |
+------------------------+---------------------------------------------------------------+-------------------+
|all_bytitle_featured    |Return all ``{title,id}`` pairs for a category, sorted on      |cat                |
|                        |title, featured pages first                                    |                   |
+------------------------+---------------------------------------------------------------+-------------------+
|all_bytitle             |Return all ``{title,id}`` pairs for a category without         |cat_is             |
|                        |subcategories, sorted on title.                                |                   |
+------------------------+---------------------------------------------------------------+-------------------+
|all_bytitle_featured    |Return all ``{title,id}`` pairs for a category without         |cat_is             |
|                        |subcategories, sorted on title, featured pages first.          |                   |
+------------------------+---------------------------------------------------------------+-------------------+
|match_objects           |Returns a list of pages with similar object ids to the objects |id                 |
|                        |of the given resource with the given id.                       |                   |
|                        |Accepts optional ``cat`` parameters for filtering on           |                   |
|                        |category. Optionally accepts a ``predicate`` parameter to only |                   |
|                        |use the objects that are connected to the given ``id`` using   |                   |
|                        |the predicate or predicates.                                   |                   |
+------------------------+---------------------------------------------------------------+-------------------+
|match_objects_cats      |Returns a list of pages with similar object ids or categories. |id                 |
|                        |Accepts an optional ``cat``                                    |                   |
|                        |parameter for filtering on category.                           |                   |
+------------------------+---------------------------------------------------------------+-------------------+
|archive_year            |Returns an overview on publication year basis, for a specified |cat                |
|                        |category. Every row returned has parts: "as_date", "year" and  |                   |
|                        |"count". The order is descending, newest year first.           |                   |
+------------------------+---------------------------------------------------------------+-------------------+
|archive_year_month      |Return a grouped "archive" overview of resources within a      |cat                |
|                        |category. The result is a double list, consisting of ``[ {year,|                   |
|                        |[ months ] }]``. The result is grouped on publication year and |                   |
|                        |month, and includes counts. The order is descending, newest    |                   |
|                        |year first.                                                    |                   |
+------------------------+---------------------------------------------------------------+-------------------+
|keyword_cloud           |Return a list of ``{keyword_id, count}`` for all resources     |cat, keywordpred,  |
|                        |within a given category. The list is ordered on keyword title. |keywordcat         |
|                        |Default predicate is ``subject``, default category is          |                   |
|                        |``keyword``. Change optional ``keywordpred`` and ``keywordcat``|                   |
|                        |to create a different cloud.                                   |                   |
+------------------------+---------------------------------------------------------------+-------------------+
|previous                |Given an id, return a list of "previous" ids in the given      |id, cat            |
|                        |category. This list is ordered by publication date, latest     |                   |
|                        |first.                                                         |                   |
+------------------------+---------------------------------------------------------------+-------------------+
|next                    |Given an id, return a list of "next" ids in the given          |id, cat            |
|                        |category. This list is ordred by publication date, oldest      |                   |
|                        |first.                                                         |                   |
+------------------------+---------------------------------------------------------------+-------------------+


.. _PostgreSQL text search settings: https://www.postgresql.org/docs/current/static/textsearch-controls.html
