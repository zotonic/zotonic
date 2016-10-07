.. highlight:: django
.. include:: meta-search.rst

The m_search model provides access to different kinds of search queries for searching through models.

Most searches in Zotonic are implemented in the :ref:`mod_search`
module, searching through the ``rsc`` table in different kinds of ways.

Though, any module can implement a search by observing the
``search_query`` notification.

The search module is used inside templates. For example, the following
snippet fetches the latest 10 modified pages in the “text” category::

  {% for id in m.search[{latest cat="text" pagelen=10}] %}
      {{ m.rsc[id].title }}
  {% endfor %}

Another example, searching for a text and requesting the second page with 20 results at a time::

  {% for id, rank in m.search.paged[{fulltext text=query_string page=2 pagelen=20}] %}
      {{ m.rsc[id].title }}
  {% endfor %}

.. seealso::

    * :ref:`guide-datamodel-query-model`
    * :ref:`scomp-pager` tag
    * :ref:`mod_search` module
    * :ref:`cookbook-custom-search`
