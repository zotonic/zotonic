.. highlight:: django
.. _guide-datamodel-query-model:

Search
======

Using the query search API you can retrieve lists of resources in
various ways. In your templates, you do so through the :ref:`search model <model-search>`::

    {% for id in m.search[{query (options go here...) }] %}

For instance, to select all news items, ordered by their modification
date, newest first::

    {% for id in m.search[{query cat='news' sort='-rsc.modified'}] %}
        {{ id }}
    {% endfor %}

Trying it out
-------------

Of course you can create your own ``for``-loop in a template, but
there are easier ways to check out the inner workings of the
query model: through your browser.

The query-model is exposed to the browser in (currently) 2 URLs: the
Atom feed module for creating a customized update feed, and the API
for receiving lists of ids in JSON.

Get all resource of the "documentation" category on zotonic.com:

http://zotonic.com/api/search?cat=documentation

Get a feed of most recent documentation containing the word "filter":

http://zotonic.com/feed/search?cat=documentation&text=filter

.. note::

   ``mod_atom_feed`` automatically sorts on last-modified date,
   ``api/search`` doesn't.


Query arguments
---------------

cat
^^^

Filter resources on a specific category::

    cat='news'

Specifying multiple ‘cat’ arguments will do an OR on the categories. So to
select both news and person resources::

    cat='news' cat='person'

cat_exact
^^^^^^^^^

Filter resources to include the given category, but exclude any subcategory::

    cat_exact='news'

cat_exclude
^^^^^^^^^^^

Filter resources to exclude the given category::

    cat_exclude='meta'

id_exclude
^^^^^^^^^^

Filter resources to exclude the ones with the given ids::

    id_exclude=123

filter
^^^^^^

Filtering on columns::

    filter=['pivot_title', 'Hello']

In its most simple form, this does an 'equals' compare filter. The
``filter`` keywords expects a list. If the list is two elements long,
we expect the first column to be the filter column name from the
database table, and the second column name to be the filter value::

    filter=['numeric_value', `gt`, 10]

If the filter is a three-column list, the second column is the
operator. This must be an atom (surround it in backquotes!) and must
be one of the following: ``eq``, ``ne``, ``gt``, ``gte``, ``lt``,
``lte``; or one of ``=``, ``<>``, ``>``, ``>=``, ``<``, ``<=``::

    filter=['numeric_value', `>`, 10]

It is possible to define an OR query for multiple terms::

    filter=[ ['numeric_value', `>`, 10], ['numeric_value', `<=`, 0] ]

hassubject
^^^^^^^^^^

Select all resources that have an *incoming edge* from the given
page, which is specified by the argument (the page id ``123`` in the
example, or the unique page name ``tag_gift``). Optionally, you can
pass the name of a predicate as the second argument, to specify that
the connection should have this predicate.

So, to select all resources that have an incoming edge from a subject with id
``123``::

    hassubject=123

Alternatively, use the subject’s unique name::

    hassubject='tag_gift'

Specifying this multiple times does an AND of the conditions::

    hassubject=123
    hassubject=[123,'author']

hasobject
^^^^^^^^^

Like ``hassubject``, but selects all pages that have an **outgoing edge** to
the given page, which is specified by the argument. Optionally, you can pass the
name of a predicate as the second argument, to specify that the connection
should have this predicate::

    hasobject=123
    hasobject='tag_gift'
    hasobject=[123,'hasdocument']

hasanyobject
^^^^^^^^^^^^

Like ``hasobject``, but allows to define an OR operation on the edge. You can
define multiple combinations of predicates and objects; any resource having such
an outgoing edge will be matched. The argument is a list. Each element in the
list is either an id or an id/predicate combination.

To select all resources that have an outgoing edge to an object with id 1, 2 or 3::

    hasanyobject=[1, 2, 3]

For each list element, you can add the connection’s predicate. So, to select all
resources that have an outgoing ‘author’ edge to an object with id 123::

    hasanyobject=[[123, 'author']]

And to do the same but also include resources that have an ‘editor’ edge to an
object with id 456::

    hasanyobject=[[123, 'author'], [456, 'editor']]

Substitute ``'*'`` for the object id to match *any* object. So, to select all
resources that have any author or editor edge::

    hasanyobject[['*', 'author'], ['*', 'editor']]

You can also mix the two types of elements. To select all resources that have an
author or a connection (with any predicate) to resource 2 or 3::

    hasanyobject[['*', 'author'], 2, 3]

match_objects
^^^^^^^^^^^^^

Find the resources that have similar object edges as the given resource.
This is done using a full text query. The resource with most overlapping
objects ids will be returned first::

    match_objects=1234

An ``id_exlude=...`` is automatically added for the resource in the argument.

is_authoritative
^^^^^^^^^^^^^^^^

Boolean, filters whether a resource is considered authoritative
(originating from this site) or not::

    is_authoritative

is_featured
^^^^^^^^^^^

A boolean option that specifies if a page should be featured or not::

    is_featured

is_published
^^^^^^^^^^^^

Select published, unpublished or omit the publish check. Legal
values are true, false or all::

    is_published='all'

upcoming
^^^^^^^^

Specifying 'upcoming' means that you only want to select things that
have a start date which lies in the future. Like the name says,
useful to select upcoming events::

    upcoming

ongoing
^^^^^^^

Specifying 'ongoing' means that you only want to select things that
are happening now: that have a start date which lies in the past,
and an end date which lies in the future::

    ongoing

finished
^^^^^^^^

Specifying 'finished' means that you only want to select things that
have a start date which lies in the past::

    finished

unfinished
^^^^^^^^^^

Specifying 'unfinished' means that you only want to select things that
have an end date which lies in the future::

    unfinished

unfinished_or_nodate
^^^^^^^^^^^^^^^^^^^^

Specifying 'unfinished_or_nodate' means that you only want to select things that
have an end date which lies in the future or no start date::

     unfinished_or_nodate

sort
^^^^

Sort the result on a field. The name of the field is a string which
directly refers to the SQL join that is being used. If you specify a
dash (``-``) in front of the field, the order is descending. Leaving
this out or specifying a ``+`` means ascending.

Some sort fields:

- ``rsc.modified`` - date of last modification
- ``rsc.pivot_date_start`` - the start date specified in the admin
- ``rsc.pivot_date_end`` - the end date specified in the admin
- ``rsc.pivot_title`` - the title of the page. For
  multilingual sites, the behavior of sorting on title is undefined.
- ``seq`` - sequence number of the first edge (ignored if no edge is joined)
- ``edge.created`` - creation date of the first edge (ignored if no edge is joined)

For all the sort fields, you will have to consult Zotonic’s data
model. Example sorting on modification date, newest first::

    sort='-rsc.modified'

.. seealso:: :ref:`cookbook-pivot-templates`

.. _custompivot:

custompivot
^^^^^^^^^^^

Add a join on the given custom pivot table. The table is joined to
the primary ``rsc`` table: ``custompivot=foo`` (joins the ``pivot_foo`` table into the query)

The pivot tables are aliassed with a number in order of their
occurrence, with the first pivot table aliassed as ``pivot1``. This
allows you to do filtering on custom fields like this::

    {query custompivot="pivotname" filter=["pivot1.fieldname", `=`, "hello"]}

.. seealso:: :ref:`cookbook-custom-pivots`

hasobjectpredicate
^^^^^^^^^^^^^^^^^^

Filter on all things which have any outgoing edge with the given
predicate::

    hasobjectpredicate='hasdocument'

hassubjectpredicate
^^^^^^^^^^^^^^^^^^^

Filter on all things which have any incoming edge with the given
predicate::

    hassubjectpredicate='author'

text
^^^^

Perform a fulltext search on the primary "rsc" table. The result
will automatically be ordered on the relevancy (rank) of the result::

    text="test"

Use prefix ``id:`` to find specific resources by id or name::

    text="id:1000"

    text="id:1000,1001,1002"

    text="id:category,1"

query_id
^^^^^^^^

Load the query arguments from the saved ``query`` resource:

``query_id=331``

.. seealso:: :ref:`guide-query-resources`

qargs
^^^^^

Take all the arguments from the current request and use these.
The arguments have to start with a ``q``, for example::

    http://example.com/search?qs=test&qcat=text

With the query:

    m.search.paged[{query qargs page=q.page pagelen=20}]

Will find all pages containing the string *test* in the category *text*.

As ``qs`` is the usual text search argument in forms it is mapped to ``text``.
All other arguments have the ``q`` removed and should map to known query-model
arguments.

publication_month
^^^^^^^^^^^^^^^^^

Filter on month of publication date

``publication_month=9``

publication_year
^^^^^^^^^^^^^^^^

Filter on year of publication date

``publication_year=2012``

date_start_after
^^^^^^^^^^^^^^^^

Select items with a start date greater than given value

``date_start_after="2010-01-15"``

It also possible to use relative times:

* ``now``
* ``+0 sunday``  (last sunday or the current sunday)
* ``+0 monday``  (last monday or the current monday)
* ``+1 minute``
* ``+1 hour``
* ``+1 day``
* ``+1 week``
* ``+1 month``
* ``+1 year``

Negative offsets are allowed as well. There //must// be a ``+`` or ``-`` sign.

date_start_before
^^^^^^^^^^^^^^^^^

Select items with a start date smaller than given value::

    date_start_before="2010-01-15"

date_start_year
^^^^^^^^^^^^^^^

Select items with an "event start date" in the given year::

    date_start_year=2012

date_end_after
^^^^^^^^^^^^^^

Select items with a end date greater than given value::

    date_end_after="2010-01-15"

date_end_before
^^^^^^^^^^^^^^^

Select items with a end date smaller than given value::

    date_end_before="2010-01-15"

date_end_year
^^^^^^^^^^^^^

Select items with an "event end date" in the given year::

    date_end_year=2012

content_group
^^^^^^^^^^^^^

Select items which are member of the given content group (or one of its children)::

    content_group=public

Filter behaviour
----------------

All of the filters work as AND filter. The only exception to this
is the ``cat=`` filter: if you specify multiple categories, those
categories are "OR"'ed together, to allow to search in multiple
distinct categories with a single search query.

.. _guide-query-resources:

Query resources
---------------

Query resources are, as the name implies,
:ref:`guide-datamodel-resources` of the special category `query`. In
the admin this category is called "search query". it is basically a
stored (and thus content manageable) search query. You create an
editable search query in an admin page that then is invoked from a
template.

When creating such a resource in the page, you will see on the admin
edit page an extra text field in which you can add search terms. Each
search term goes on its own line, and the possible search terms are
equal to the ones described on this page (the `Query-model
arguments`).

.. seealso::

    - :ref:`mod_search` reference: Zotonic’s built-in search module.
    - `mod_elasticsearch <https://github.com/driebit/mod_elasticsearch>`_
      on using Elasticsearch with Zotonic.
    - `mod_search_solr <https://github.com/arjan/mod_search_solr>`_ on
      using Solr for search.
