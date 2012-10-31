.. _manual-datamodel-resources:

Resources
---------

Each :term:`resource` on a site is stored in the ``rsc`` table.  Some
properties are present as columns and other properties are serialized
in a binary blob column. The properties having their own column are
essential for the system to select the wanted records or to check
uniqueness constraints or foreign key constraints.

Examples of properties represented as columns are the id, visibility,
category id, modification date, creation date, modifier id, creator
id, version number, unique name, unique path, unique uri and
publication period. For a full listing of its properties, see the
:ref:`model-rsc` page.

Resource properties can also be customly defined: any property that is
programmatically set on the resource, is stored in serialized form in
the record, and can later be retrieved.

.. seealso:: :ref:`model-rsc`

.. _manual-datamodel-categories:
             
Resource categories
...................

Every resource belongs to a single category.

There is no real distinction between rsc records that are a person, a
news item, a video or something else. The only difference is the
`category` of the rsc record, which can easily be changed. Even
categories and predicates themselves are represented as rsc records
and can, subsequently, have their own page on the web site.

Categories are organised in a hierarchical fashion, and is used to
organise the resources into meaningful groups. Zotonic has a standard
set of categories (see :ref:`manual-datamodel-domainmodel`, but it is
very usual to define your own in your own site, resulting in a custom
:term:`domain model`.

In the database, categories are stored in an extra metadata table,
``category``, which defines the hierarchy of categories using the
`Nested Set model
<http://en.wikipedia.org/wiki/Nested_set_model>`_. The tree is
strictly hierarchical: Every category has at most a single parent
category, and every resource belongs to exactly one category.  That a
resource can't belong to more than a single category is done to
maintain the datamodel's simplicity and speed of the searches in the
system.

Since in Zotonic, `everything is a resource`, categories `themselves`
are also resources, namely, resources of the category `category`. This
allows the category to be titled and described, just like other
resources. The category table only describes the nested hierarchy of
the categories. All other properties of a category are defined by its
rsc record.

.. seealso:: :ref:`model-category`


Medium
......

Medium management is described in full in :ref:`manual-media`. Media
metadata is stored in a separate table, called ``medium``, since one
media is a medium. When a resource contains a medium, this table holds
a record describing it. Amongst others, it stores its mime type,
width, height and file size.

Besides the ``medium`` table, a ``medium_deleted`` table exists. When
a medium is deleted then any files referenced by that medium will be
added to this table. Zotonic periodically checks this table to delete
files that are no longer referenced by any media.

.. seealso:: :ref:`model-media`


Blocks
......

Blocks are a specific feature in a resource. The ``blocks`` property
of a resource is a list of blocks which can be dynamically added and
removed from the resource in the admin edit page. Each module can
define their own blocks, which consist of an edit template and a view
template.

The survey module uses the blocks feature to allow you to dynamically
create a list of questions which a user has to answer.

.. todo:: Fix blocks documentation


Pivot columns
.............

Most properties of a resource are stored in the resource record in a
single column, called ``props``. This column cannot be read by the
database (or by humans for that matter), as it is a single, serialized
Erlang term containing all the properties.

This is a very flexible approach, which allows that any property that
you set on a resource can be stored and later retrieved. As such, it
is fine for most properties.

The "standard" properties of the resource, like dates, the title, name
and address details, are also stored in "real" database columns, thus
allowing you to use SQL to filter and order on these.

Zotonic is smart enough that when you enter any textual information
into any resource property, it will extract this and put it in the
`pivot_tsv` column, for use in full-text searches.

However, if you want to search by or order on any custom defined
property, you need to define your own database column in a so-called
"custom pivot"; see: :ref:`manual-datamodel-custompivots`

The pivot queue
'''''''''''''''

When the version number or modification date of a resource is updated
then its id is added to the `pivot queue`. Zotonic has a pivot process
running in the background which looks at this queue and for each
queued resource, extract all texts and some other information from the
record, filling the pivot columns of the rsc record. The pivot columns
are used for searching, they contain amongst others the full text
index.

The ``rsc_pivot_queue`` table is used to hold the queue of resource
ids that are waiting to be pivoted.

The ``pivot_task_queue`` holds a second queue for more generic task
processing: it holds references to functions which need to be called
in the background.


Identities
..........

A rsc record can become an user by adding the user's credentials to
this table. A single user can have multiple kinds of credentials,
think of his/her username, openid uri etc. A user doesn't necessarily
be a person. See also: :ref:`model-identity`.
