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

.. note::

   ``mod_atom_feed`` automatically sorts on last-modified date,
   ``api/model/search/get`` doesn't.

Of course you can create your own ``for``-loop in a template, but
there are easier ways to check out the inner workings of the
query model: through your browser.

The query-model is exposed to the browser in (currently) 2 URLs: the
Atom feed module for creating a customized update feed, and the API
for receiving lists of ids in JSON.

Get all resources of the "documentation" category on zotonic.com:

https://zotonic.com/api/model/search/get?qcat=documentation

Get a feed of most recent documentation containing the word "filter":

https://zotonic.com/feed/search?cat=documentation&text=filter


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

id
^^

Filter resources to only include the ones with the given ids::

    id=123

id_exclude
^^^^^^^^^^

Filter resources to exclude the ones with the given ids::

    id_exclude=123

filter
^^^^^^

Filtering on columns::

    filter=['pivot.title', 'Hello']
    filter=['pivot.mypivot.foo', 'bar']
    filter=['facet.somefacet', 'baz']

In its most simple form, this does an 'equals' compare filter. The
``filter`` keywords expects a list. If the list is two elements long,
we expect the first column to be the filter column name from the
database table, and the second column name to be the filter value::

    filter=['facet.numeric_value', `gt`, 10]

If the filter is a three-column list, the second column is the
operator. This must be an atom (surround it in backquotes!) and must
be one of the following: ``eq``, ``ne``, ``gt``, ``gte``, ``lt``,
``lte``; or one of ``=``, ``<>``, ``>``, ``>=``, ``<``, ``<=``, ``~``::

    filter=['facet.numeric_value', `>`, 10]

It is possible to define an OR query for multiple terms::

    filter=[ ['facet.numeric_value', `>`, 10], ['facet.numeric_value', `<=`, 0] ]

All postal codes starting with ``10``::

    filter=[ ['pivot.postcode', `~`, "10" ] ]

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

    hasanyobject=[['*', 'author'], ['*', 'editor']]

You can also mix the two types of elements. To select all resources that have an
author or a connection (with any predicate) to resource 2 or 3::

    hasanyobject=[['*', 'author'], 2, 3]


hasanysubject
^^^^^^^^^^^^^

Like ``hasanyobject`` but then searching for subjects (resources referring to) of
the found resource id.


hasmedium
^^^^^^^^^

Only returns resources that have a ``medium`` record attached or only those that
do not have a ``medium`` record attached.

For example::

    hasmedium

Return only resources with a medium record.

Or::

    hasmedium=false

Return only resources without a medium record.

The joined medium record is `medium`, that enables sorting on, for example, the
medium record’s creation date with ``sort=medium.created``.


match_objects
^^^^^^^^^^^^^

Find the resources that have similar object edges as the given resource.
This is done using a full text query. The resource with most overlapping
objects ids will be returned first::

    match_objects=1234

An ``id_exlude=...`` is automatically added for the resource in the argument.

Optionally accepts a ``predicate`` option to only match using the object-ids
that are connected to the id using the given predicate or predicates.

Example::

    %{
       term: "match_objects",
       value: id,
       predicate: [ "subject", "author" ]
    }

This returns a list of resource ids that have similar objects as the authors and
subjects of the resource ``id``. The objects can be connected to the resulting
ids using any predicate.


match_object_ids
^^^^^^^^^^^^^^^^

Find the resources that have similar object edges to the given resources.
This is done using a full text query. The resource with most overlapping
objects ids will be returned first::

    match_object_ids=[108, 238, 1234]

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

is_findable
^^^^^^^^^^^

A boolean option that specifies if a page should be findable or not::

    is_findable

This checks the rescource’s ``is_unfindable`` flag. To be findable in
searches the flag must be set to ``false``, which is the default.

is_unfindable
^^^^^^^^^^^^^

A boolean option that specifies if a page should not be findable::

    is_unfindable

This checks the rescource’s ``is_unfindable`` flag.

upcoming
^^^^^^^^

Specifying 'upcoming' means that you only want to select things that
have a start date which lies in the future. Like the name says,
useful to select upcoming events::

    upcoming

upcoming_on
^^^^^^^^^^^

Specifying 'upcoming' means that you only want to select things that
have a start date after the given date. Like the name says,
useful to select upcoming events::

    upcoming_on='+1 week'

upcoming_date
^^^^^^^^^^^^^

Specifying 'upcoming' means that you only want to select things that
have a start date after the start of the given date. Like the name says,
useful to select upcoming events::

    upcoming_date='+1 week'

ongoing
^^^^^^^

Specifying 'ongoing' means that you only want to select things that
are happening now: that have a start date which lies in the past,
and an end date which lies in the future::

    ongoing

ongoing_on
^^^^^^^^^^

Specifying 'ongoing' means that you only want to select things that
are happening on the given moment: that have a start datetime which lies before
the given datetime and an end date which lies after the given datetime::

    ongoing_on='yesterday'

ongoing_date
^^^^^^^^^^^^

Specifying 'ongoing' means that you only want to select things that
are happening on the given day: that have a start date which lies before
the given day and an end date which lies after the start of the given day::

    ongoing_date='yesterday'

finished
^^^^^^^^

Specifying 'finished' means that you only want to select things that
have a start date which lies in the past::

    finished

finished_on
^^^^^^^^^^^

Specifying 'finished' means that you only want to select things that
have a start datetime which lies before the given moment::

    finished_on='tomorrow'

finished_date
^^^^^^^^^^^^^

Specifying 'finished' means that you only want to select things that
have a start day which lies before the start of the given day::

    finished_date='tomorrow'

unfinished
^^^^^^^^^^

Specifying 'unfinished' means that you only want to select things that
have an end date which lies in the future::

    unfinished

unfinished_on
^^^^^^^^^^^^^

Specifying 'unfinished' means that you only want to select things that
have an end date which after the given date::

    unfinished_on='+3 days'

unfinished_date
^^^^^^^^^^^^^^^

Specifying 'unfinished' means that you only want to select things that
have an end date which after the end of the given day::

    unfinished_date='+3 days'

unfinished_or_nodate
^^^^^^^^^^^^^^^^^^^^

Specifying 'unfinished_or_nodate' means that you only want to select things that
have an end date which lies in the future or no start date::

     unfinished_or_nodate

sort / asort / zsort
^^^^^^^^^^^^^^^^^^^^

Sort the result on a field. The name of the field is a string which
directly refers to the SQL join that is being used. If you specify a
dash (``-``) in front of the field, the order is descending. Leaving
this out or specifying a ``+`` means ascending.

The sort terms are added in the order: ``asort``, ``sort``, and ``zsort``.

This is useful for e.g. text search. Text search will add a ``sort`` term on
relevance. This relevance sort term is appended *after* any existing sort term.
By using ``zsort`` you can force sub-sorting in case of the same relevance or no
text for the query. Example::

    {query cat='news' text=q.qsort zsort="-rsc.created"}

If ``q.qsort`` is empty, this will return the newest *news* items. If ``q.qsort``
is not empty then it will search for the text and return the best matches where
equally matching news items will have the newest on top. Use ``asort`` instead
of ``zsort`` to show the newest matching news, regardless on how well they match
the search term::

    {query cat='news' text=q.qsort asort="-rsc.created"}

Some sort fields:

- ``rsc.modified`` - date of last modification
- ``rsc.publication_start`` - publication date
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

pivot.name
^^^^^^^^^^

.. seealso:: :ref:`cookbook-custom-pivots`

Filter on the named pivot of the rsc table. The name is prefixed with ``pivot_``.

Available pivot fields are:

    *  pivot.category_nr,
    *  pivot.first_name
    *  pivot.surname
    *  pivot.gender
    *  pivot.date_start
    *  pivot.date_end
    *  pivot.date_start_month_day
    *  pivot.date_end_month_day
    *  pivot.street
    *  pivot.city
    *  pivot.state
    *  pivot.postcode
    *  pivot.country
    *  pivot.geocode
    *  pivot.title
    *  pivot.location_lat
    *  pivot.location_lng

These fields can also be used in ``filter`` and the ``sort`` terms.


pivot.mypivot.name
^^^^^^^^^^^^^^^^^^

Filter on the named pivot of the custom pivot ``mypivot``.

Here ``mypivot`` is a custom pivot table defined with ``z_pivot_rsc:define_custom_pivot/3``

Check the explanation and examples in :ref:`cookbook-custom-pivots` for more information.

These fields can also be used in ``filter`` and the ``sort`` terms.


facet.name
^^^^^^^^^^

Add a join with the facets table and filter on the named facet.

The facets table is filled from the ``pivot/facet.tpl`` template, each block is a
facet that can be used for filters or for the ``facets`` query.

The ``name`` must be the name without types of a block. That is, if a block is
called ``foo_int`` then the ``name`` is ``foo`` and the query term is ``facet.foo``.

The value can be an operator::

    >123
    >=123
    <123
    <=123
    <>123

For example, the last one translates to the SQL clause ``facet.name <> 123``.

The facet fields can also be used in ``filter`` and the ``sort`` terms.


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

.. seealso:: :ref:`guide-query-resources`

Load the query arguments from the saved ``query`` resource:

``query_id=331``

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

name
^^^^

Find resources with a matching unique name. A wildcard can be defined, for example::

    name=page_*

Searching on an empty name or just ``*`` will return all resources with a defined name.
The given name will be trimmed and converted to lowercase before searching.

language
^^^^^^^^

Find resources with a certain language. The language must be a valid ISO 639-1 language
code. Search terms with invalid language codes are ignored.

Find all resources with a German translation::

    language=de

Use the special language ``z_language`` to search in the current request language::

    language=z_language

Example, search in English or the current request language::

    language=[en,z_language]


visible_for
^^^^^^^^^^^

Filters on the ``visible_for`` resource property. This is used by some access control
modules to filter the visibility of resources. The filtered value is an integer or a
list of integers::

    visible_for=[5,6]

Note that the default :ref:`mod_acl_user_groups` does not use this property.


Filter behaviour
----------------

All of the filters work as AND filter. The only exception to this
is the ``cat=`` filter: if you specify multiple categories, those
categories are "OR"'ed together, to allow to search in multiple
distinct categories with a single search query.

.. _guide-query-resources:

Query resources
---------------

.. seealso::

    - :ref:`mod_search` reference: Zotonic’s built-in search module.
    - `mod_elasticsearch <https://github.com/driebit/mod_elasticsearch>`_
      on using Elasticsearch with Zotonic.
    - `mod_search_solr <https://github.com/arjan/mod_search_solr>`_ on
      using Solr for search.

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
