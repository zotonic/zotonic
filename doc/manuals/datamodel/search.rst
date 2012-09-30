.. highlight:: django
.. _manual-datamodel-query-model:

The Query search-model
======================

Using the query search API you can retrieve lists of resources in
various ways. The query search-model is a powerful search model that
accepts a lot of options. This page documents those options.

You access the query model in the following way::

  {% for id in m.search[{query (options go here...) }] %}

For instance, to select all news items, ordered by their modification date, newest first::

  {% for id in m.search[{query cat='news' sort='-rsc.modified'}] %}
      {{ id }}
  {% endfor %}

Trying it out
-------------

Ofcourse you can create your own ``for``-loop in a template, but there
are more easier ways to check out the inner workings of the
query-model: through your browser.

The query-model is exposed to the browser in (currently) 2 URLs: the
Atom feed module for creating a customized update feed, and the API
for receiving lists of ids in JSON.

Get all resource of the "documentation" category on zotonic.com::

  http://zotonic.com/api/search?cat=documentation

Get a feed of most recent documentation containing the word "filter"::

  http://zotonic.com/feed/search?cat=documentation&text=filter

Note that ``mod_atom_feed`` automatically appends a sorting on
last-modified date, something which ``api/search`` does not do.


Query-model arguments
-------------------------

**authoritative**

  Boolean, filters whether a resource is considered "authoritative"
  (belonging on this site) or not.

  ``authoritative=1``

**cat**

  Filter resources on a specific category. Specifying multiple 'cat'
  arguments will do an "or" on the categories.

  ``cat='news'``

**cat_exclude**

  Filter resources to exclude the given category.

  ``cat_exclude='meta'``

**hassubject**

  Select all resources that have an outgoing connection to the given
  page, which is specified by the argument (123 in the
  example). Optionally, you can give the name of a predicate as second
  argument, to specify that the connection should have this
  predicate. Specifying this multiple times does an "or" of the conditions.

  ``hassubject=123``
  ``hassubject=[123,'author']``

**hasobject**

  Like hassubject, but selects all pages that have an incoming
  connection to the given page, which is specified by the
  argument. Optionally, you can give the name of a predicate as second
  argument, to specify that the connection should have this predicate.

  ``hasobject=123``
  ``hasobject=[123,'document']``

**is_featured**

  A boolean option that specifies if a page should be featured or not.

  ``is_featured``

**is_published**

  Select published, unpublished or omit the publish check. Legal values are true,false or all.

  ``is_published='all'``

**is_public**

  Filter on whether an item is publicly visible or not. Valid values are 'true', 'false', 'all'.

  ``is_public='false'``

**upcoming**

Specifying 'upcoming' means that you only want to select things that have a start date which lies in the future. Like the name says, useful to select upcom  ing events.

  ``upcoming``

**sort**

  Sort the result on a field. The name of the field is a string which
  directly refers to the sql-join that is being used. If you specify a
  dash ("-") in front of the field, the order is descending. Leaving
  this out or specifying a "+" means ascending.

  Some sort fields:

  - ``rsc.modified`` - date of last modification
  - ``rsc.pivot_date_start`` - the start date specified in the admin
  - ``rsc.pivot_date_end`` - the end date specified in the admin
  - ``rsc.pivot_title`` - the title of the page. When making multilanguage sites, the behaviour of sorting on title is undefined.

  For all the sort fields, you will have to consult the Zotonic's data model. Example sorting on modification date, newest first:

  ``sort='-rsc.modified'``

**custompivot**

  Add a join on the given custom pivot table. The table is joined to the primary ``rsc`` table.

  ``custompivot=foo``
  (joins the ``pivot_foo`` table into the query)

**hasobjectpredicate**

  Filter on all things which have any outgoing edge with given predicate.

  ``hasobjectpredicate='document'``

**hassubjectpredicate**

  Filter on all things which have any incoming edge with given predicate.

  ``hasobject='author'``

**text**

  Perform a fulltext search on the primary "rsc" table. The result will automatically be ordered on the relevancy (rank) of the result.

  ``text="test"``

**query_id**

  Load the query arguments from the saved ``query`` resource.

  ``query_id=331``

**publication_month**

  Filter on month of publication date

  ``publication_month=9``

**publication_year**

  Filter on year of publication date

  ``publication_year=2012``

**date_start_after**

  Select items with a start date greater than given value

  ``date_start_after="2010-01-01"``

**date_start_before**

  Select items with a start date smaller than given value

  ``date_start_before="2010-01-01"``

**date_start_year**

  Select items with a "event start date" in the given year.

  ``date_start_year=2012``

**date_end_year**

  Select items with a "event end date" in the given year.

  ``date_end_year=2012``

