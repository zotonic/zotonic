.. _guide-datamodel-resources:

Resources
=========

Resources are Zotonic’s main data unit. You may want to familiarise yourself
with the Zotonic :ref:`data model <guide-datamodel>` in the User Guide.

Resource properties
-------------------

Resources are very flexible data units: they can have any property
that the developer needs them to have. However, by default, Zotonic’s
admin is designed to edit a common set of properties. The
:ref:`m_rsc model <model-rsc>` is used to edit resources and display them in
templates.

.. _categories:

Categories
----------

Every resource belongs to a single category.

There is no real distinction between rsc records that are a person, a
news item, a video or something else. The only difference is the
*category* of the rsc record, which can easily be changed. Even
categories and predicates themselves are represented as rsc records
and can, subsequently, have their own page on the web site.

Categories are organized in a hierarchical fashion, and used to
organize the resources into meaningful groups. Zotonic has a standard
set of categories (see :ref:`guide-domain-model`), but it is
very usual to define your own in your own site, resulting in a custom
:term:`domain model`.

In the database, categories are stored in an extra metadata table,
``category``, which defines the hierarchy of categories using the
`Nested Set model
<http://en.wikipedia.org/wiki/Nested_set_model>`_. The tree is
strictly hierarchical: Every category has at most a single parent
category, and every resource belongs to exactly one category.  That a
resource can’t belong to more than a single category is done to
maintain the datamodel’s simplicity and speed of the searches in the
system.

Since in Zotonic, `everything is a resource`, categories `themselves`
are also resources, namely, resources of the category `category`. This
allows the category to be titled and described, just like other
resources. The category table only describes the nested hierarchy of
the categories. All other properties of a category are defined by its
rsc record.

.. seealso:: :ref:`model-category` model reference

Medium
------

Medium management is described in full in :ref:`guide-media`. Media
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
------

Blocks are a specific feature in a resource. The ``blocks`` property
of a resource is a list of blocks which can be dynamically added and
removed from the resource in the admin edit page. Each module can
define their own blocks, which consist of an edit template and a view
template.

The survey module uses the blocks feature to allow you to dynamically
create a list of questions which a user has to answer.

.. todo:: Fix blocks documentation

Manipulating resources
----------------------

How resources are stored
^^^^^^^^^^^^^^^^^^^^^^^^

Each :term:`resource` on a site is stored in the ``rsc`` database table. The
resource’s properties are stored in two ways.

* The *core properties* are persisted in separate columns. They include id, name,
  category, modification date, path, publication period. These properties are
  used for searching, filtering and sorting resources. As they can have unique
  or foreign key constraints, they help in preserving data sanity.
* All *other properties* are serialized together into one binary blob column
  named ``props``. This includes any custom properties that you set on the
  resource. These serialized properties cannot be used for finding or sorting
  data, but only for later retrieval.

Storing properties in a serialized form is a flexible approach. You can save any
property on a resource without having to make changes to your database schema.

Changing resources
^^^^^^^^^^^^^^^^^^

Imagine you wish to store whether resources are liked by users. No need to
change the database schema, define the property or whatsoever. Just update the
resource and set a custom ``is_liked`` property (using :ref:`model-rsc`)::

    m_rsc:update(123, [{is_liked, true}], Context).

``is_liked=true`` will now be stored in the database for resource ``123``, so
you can retrieve it like you would any other property::

    ?DEBUG(m_rsc:p(123, is_liked, Context)).
    %% prints: true

Or, in a template:

.. code-block:: django

    {{ id.is_liked }}

    which is equivalent to:

    {{ m.rsc[id].is_liked }}

To remove the property, just store it as ``undefined``::

    m_rsc:update(123, [{is_liked, undefined}], Context).

This flexible approach is fine for custom properties that you only want to
retrieve and display. However, if you need to *find* all liked resources, you
need to define ``is_liked`` as a pivot column (see below).

.. seealso:: :ref:`m_rsc model reference <model-rsc>`

Pivots
------

Pivot columns
^^^^^^^^^^^^^

If you want to *filter* or *sort* on any custom defined property, you need to store
that property in a separate database column using a
:ref:`custom pivot <cookbook-custom-pivots>`. If you want to *find* resources
based on text values in custom properties, you can change the texts that are
pivoted with :ref:`pivot templates <cookbook-pivot-templates>`.

The pivot queue
^^^^^^^^^^^^^^^

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
----------

An rsc record can become a user by adding the user’s credentials to
this table. A single user can have multiple kinds of credentials,
think of his/her username, openid uri etc. A user isn't necessarily a
person.

.. seealso:: :ref:`model-identity`.

Deleted resources
-----------------

Whenever a resource is deleted, an entry is added to the ``rsc_gone``
table.  The page and id controllers will serve a *410 Gone* when a
deleted resource is requested.

.. seealso:: :ref:`model-rsc_gone`.
