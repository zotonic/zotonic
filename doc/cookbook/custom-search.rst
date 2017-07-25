.. _cookbook-custom-search:

Custom search
=============

Implement a custom search by observing the :ref:`search_query` notification
in your module. Imagine you want to search cookies in your database that
either have chocolate or do not::

    -include_lib("zotonic_core/include/zotonic.hrl").

    -export([
        observe_search_query/2
    ]).

    observe_search_query(#search_query{search = Search, offsetlimit = OffsetLimit}, Context) ->
        %% Pass on to an internal function.
        search(Search, OffsetLimit, Context).

    search({cookies, [{chocolate, Boolean}]}, _OffsetLimit, Context)
        %% Handle search queries of the cookie type.
        #search_sql{
            select="c.id",
            from="foo c",
            where="c.chocolate = $1",
            order="c.name desc",
            args=[Boolean],
            tables=[{cookies_table, "c"}]
        };
    search(_Other, _OffsetLimit, _Context) ->
        %% Let other modules handle other types of search queries.
        undefined.

Do not forget to add the last function that matches and returns
``undefined`` for any other search request. If you forget this, the
notification fold will crash when using any other search query.

.. highlight:: django

This can then be used in your template like this::

    {% for id in m.search[{cookies chocolate=true}] %}
        Looping over all ids in the ‘cookies_table’ table for which chocolate = true
    {% endfor %}

