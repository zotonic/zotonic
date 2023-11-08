.. highlight:: django
.. include:: meta-pager.rst

Show a pager for search results.

This generates a pager as seen on the search results pages. It is used in conjunction with a paged search result.

For example, a fulltext search where the search parameters come from the query string::

   {% with m.search.paged[{fulltext cat=q.qcat text=q.qs page=q.page pagelen=20}] as result %}
     <ul>
       {% pager result=result dispatch="admin_overview_rsc" qargs %}
       {% for id,score in result %}
         <li><a href="">{{ m.rsc[id].title }}</a></li>
       {% empty %}
         <li>Nothing found</li>
       {% endfor %}
     </ul>
   {% endwith %}

This will show a list of titles and above that the links to the next, previous and other pages.

.. note:: that we are using ``m.search.paged`` here and not :ref:`m.search <model-search>`. The pager only works with results from ``m.search.paged``.

The generated pager code will look something like (when searching for the text “filter”)::

   <ul class="pager block">
   <li>« prev</li>
   <li class="current"><a href="/admin/overview?qs=filter&page=1">1</a></li>
   <li><a href="/admin/overview?qs=filter&page=2">2</a></li>
   <li><a href="/admin/overview?qs=filter&page=3">3</a></li>
   <li><a href="/admin/overview?qs=filter&page=4">4</a></li>
   <li class="pager-sep">…</li>
   <li><a href="/admin/overview?qs=filter&page=5">5</a></li>
   <li><a href="/admin/overview?qs=filter&page=2">next»</a></li>
   </ul>

The pager tag accepts the following arguments:

+----------------+------------------------------------------------------------------+----------------------------------+
|Argument        |Description                                                       |Example                           |
+================+==================================================================+==================================+
|result          |The result from a search.  This must be a ``#search_result``      |result=mysearchresult             |
|                |or a list. This mostly the result of a ``m.search`` call.         |                                  |
+----------------+------------------------------------------------------------------+----------------------------------+
|dispatch        |Name of the dispatch rule to be used for the page urls. Defaults  |dispatch="searchresult"           |
|                |to the dispatch rule of the current page. If there is no dispatch |                                  |
|                |rule defined then ``none`` is used and only a link with the query |                                  |
|                |arguments is generated. For example ``?page=2&qs=test``           |                                  |
+----------------+------------------------------------------------------------------+----------------------------------+
|qargs           |Append all the arguments in the HTTP request’s query string whose |qargs                             |
|                |name starts with a 'q' as an argument to the dispatch rule.       |                                  |
+----------------+------------------------------------------------------------------+----------------------------------+
|hash            |Hash to append to the pagination links. Used to jump to the search|hash="#content-pager"             |
|                |results on the load of a new page.                                |                                  |
+----------------+------------------------------------------------------------------+----------------------------------+
|hide_single_page|When this argument is true, do not show the pager when the result |hide_single_page=1                |
|                |fits on one page (e.g. the pager will be useless).                |                                  |
+----------------+------------------------------------------------------------------+----------------------------------+
|template        |Name of the template for rendering the pager. Defaults to         |template="_pager.tpl"             |
|                |``_pager.tpl``. See below for specific arguments passed.          |                                  |
+----------------+------------------------------------------------------------------+----------------------------------+
|page            |Current page number, the first page is page number 1. Fetched from|page=2                            |
|                |the search result, this argument, or ``q.page``.                  |                                  |
+----------------+------------------------------------------------------------------+----------------------------------+
|pagelen         |Number of items per page, fetch from the search result or         |pagelen=10                        |
|                |``q.pagelen``. Defaults to 20.                                    |                                  |
+----------------+------------------------------------------------------------------+----------------------------------+
|topic           |The topic for handling the link clicks. Normally a link click     |topic="/model/location/post/push" |
|                |will just load a new page in the browser. This intercepts the     |                                  |
|                |click and sends the new link to the given topic.                  |                                  |
+----------------+------------------------------------------------------------------+----------------------------------+
|\*              |Any other argument is used as an argument for the dispatch rule.  |                                  |
+----------------+------------------------------------------------------------------+----------------------------------+


Pager template
--------------

The pager is rendered using a template. The default template for the pager is ``_pager.tpl``.

The pager template receives the following variables:

 * ``prev_url`` The url to the previous page, ``undefined`` if at first page.
 * ``next_url`` The url to the next page, ``undefined`` if at last page.
 * ``pages`` A list of tuples. Either ``{PageNumber, Url}`` or ``{undefined, sep}`` (*sep* is an atom).
 * ``page`` The current page number
 * All other arguments passed to the scomp (**attention:** these are also used as dispatch arguments)

The default ``_pager.tpl`` displays an ellipsis for the ``sep`` entry.

If the result set is empty then the template is not rendered. The template is also not rendered if there
is a single page *and* ``hide_single_page`` is set.

Result argument
---------------

It is also possible to pass a list, a ``#rsc_list{ list=Ids }`` record, or a list of lists (pages) for the resut.
In this case you need to perform the pagination for displaying the results yourself. You can use the :ref:`filter-chunk`
for this. The pager scomp will fetch the ``page`` and ``pagelen`` from the pager arguments, or from the query
arguments (if any). If the list is pre-chunked then the pages does not need the ``pagelen`` argument.

Topic
-----

If the search result is refreshed on the page without a page reload, then the ``topic`` should be passed. This is
the topic that directly or indirectly triggers a refresh of the element displaying the search result.

For example, you can make a live search::

   {% live topic="model/location/event/qlist"
           template="_search.tpl"
           method="patch"
   %}

With a ``_search.tpl`` template like this::

    <form data-onsubmit-topic="model/location/post/qlist/submit"
          data-oninput-topic="model/location/post/qlist/submit"
          method="GET"
    >
        <input type="text" name="qs" value="{{ q.qs|escape }}">
    </form>

    {% with m.search.query::%{ qargs: true, page: q.page } as result %}
      <ul>
        {% for id in result %}
          <a href="{{ id.page_url }}">{{ id.title }}</a>
        {% endfor %}
      </ul>
      {% pager result=result topic="mode/location/post/push" qargs %}
    {% endwith %}

Typing in the search will automatically update the search result without reloading the page, as will a click
on one of the pager links.
