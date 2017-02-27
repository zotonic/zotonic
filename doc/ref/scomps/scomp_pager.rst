.. highlight:: django
.. include:: meta-pager.rst

Show a pager for search results.

This generates a pager as seen on the search results pages. It is used in conjunction with a paged search result.

For example, a fulltext search where the search parameters come from the query string::

   {% with m.search.paged[{fulltext cat=q.qcat text=q.qs page=q.page}] as result %}
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

+----------------+------------------------------------------------------------------+------------------------+
|Argument        |Description                                                       |Example                 |
+================+==================================================================+========================+
|result          |The result from a search.  This must be a ``#search_result`` or   |result=mysearchresult   |
|                |``#m_search_result`` record. Note that this must be the result of |                        |
|                |a ``m.search.paged`` and not of a ``m.search`` call.              |                        |
+----------------+------------------------------------------------------------------+------------------------+
|dispatch        |Name of the dispatch rule to be used for the page urls. Defaults  |dispatch="searchresult" |
|                |to the dispatch rule of the current page.                         |                        |
+----------------+------------------------------------------------------------------+------------------------+
|qargs           |Append all the arguments in the HTTP request’s query string whose |qargs                   |
|                |name starts with a 'q' as an argument to the dispatch rule.       |                        |
+----------------+------------------------------------------------------------------+------------------------+
|hide_single_page|When this argument is true, do not show the pager when the result |hide_single_page=1      |
|                |fits on one page (e.g. the pager will be useless).                |                        |
+----------------+------------------------------------------------------------------+------------------------+
|template        |Name of the template for rendering the pager. Defaults to         |template="_pager.tpl"   |
|                |``_pager.tpl``. See below for specific arguments passed.          |                        |
+----------------+------------------------------------------------------------------+------------------------+
|\*              |Any other argument is used as an argument for the dispatch rule.  |                        |
+----------------+------------------------------------------------------------------+------------------------+


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


