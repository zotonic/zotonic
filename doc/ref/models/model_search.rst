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

.. seealso:: the :ref:`scomp-pager` scomp, and the :ref:`mod_search` module.



Implementing a custom search
----------------------------

Like stated, any module can implement a search by observing the
``search_query`` notification:

.. code-block:: erlang

   observe_search_query({search_query, Req, OffsetLimit}, Context) ->
       search(Req, OffsetLimit, Context).

  search({foo_table, [{bar, Bar}]}, _OffsetLimit, _Context) ->
    #search_sql{
        select="f.id",
        from="foo f",
        where="f.bar = $1",
        order="f.baz desc",
        args=[Bar],
        tables=[{foo,"f"}]
    };

  search(_Other, _OffsetLimit, _Context) ->
      undefined.

Do not forget to add the last function that matches and returns
``undefined`` for any other search request. If you forget this, the
notification fold will crash when using any other search query.

.. highlight:: django

This can then be used in your template like this::

  {% for id in m.search[{foo bar=123}] %}
  ... looping over all ids in the 'foo' table for which bar = 123
  {% endfor %}
