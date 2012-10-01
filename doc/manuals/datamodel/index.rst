.. _manual-datamodel:

The data model
==============

.. image:: /img/zotonic_datamodel.png
   :align: right

An overview of the Zotonic data model.

The Zotonic data model has two tables at its core. The rsc
(:term:`resource` aka :term:`page`) table and the :term:`edge`
table. All other tables are for access control, visitor
administration, configuration and other purposes.

For simplicity of communication the rsc record is often referred to as
a page, as every rsc record can have their own page on the website.

Zotonic's data model is a mixture between a traditional database and a
triple store. Some resource properties are stored as columns, some are
serialized in a binary column and some are represented as directed
`edges` to other pages.

There is no real distinction between rsc records that are a person, a
news item, a video or something else. The only difference is the
`category` of the rsc record. And the rsc's category can easily be
changed. Even categories and predicates are represented as rsc records
and can, subsequently, have their own page on the web site.

.. toctree::
   search

rsc
---

The rsc (from resource) table holds all (well most) properties of a
page. Some properties are present as columns and other properties are
serialized in a binary blob column. The properties having their own
column are essential for the system to select the wanted records or to
check uniqueness constraints or foreign key constraints.

Examples of properties represented as columns are the id, visibility,
category id, modification date, creation date, modifier id, creator
id, version number, unique name, unique path, unique uri and
publication period. For a full listing of its properties, see the
:ref:`model-rsc` page.

rsc properties can also be customly defined: any property that is
programmatically set on the rsc, is stored in serialized form in the
record, and can later be retrieved.

When the version number or modification date of a rsc record is
updated then its id is added to the pivot queue. The Erlang pivot
process will extract all texts and some other information from the rsc
record and fill the pivot columns of the rsc record. The pivot columns
are used for searching, they contain amongst others the full text
index.

.. seealso:: :ref:`model-rsc`

edge
----

The edge table defines relations between rsc records (pages). It does this by adding a directed edge from one rsc record (:index:`subject`) to another (:index:`object`). It also adds a reference to the rsc record that describes the meaning of the edge (:index:`predicate`).

These edges are visible in the admin interface on the "Page connections" panel.

All edges with a certain subject are defined to `belong` to that
subject, this to simplify the access control. So if you are allowed to
edit the resource, you're also allowed to edit its `outgoing` edges ("Page
connections" in the admin).

.. seealso:: :ref:`model-edge`


medium
------

One media is a medium. When a rsc is a medium then this table holds a
record describing that medium. It stores the mime type, width, height
and other properties of the medium.

.. seealso:: :ref:`model-media`


category
--------

The category table defines a hierarchy of categories. Every category has at most a single parent category. Every rsc belongs to exactly one category.

That a rsc can't belong to more than one category is for simplicity and speed of the searches in the system.

A category is a rsc, the category table only describes the hierarchy of the categories. All other properties of a category are defined by its rsc record.

.. seealso:: :ref:`model-category`


identity
--------

A rsc record can become an user by adding the user's credentials to this table. A single user can have multiple kinds of credentials, think of his/her username, openid uri etc. A user doesn't necessarily be a person.

.. seealso:: :ref:`model-identity`

medium_deleted
--------------

When a medium is deleted then any files referenced by that medium will be added to this table. Zotonic periodically checks this table to delete files that are not needed anymore.


visitor / visitor_cookie
------------------------

Every visitor to a Zotonic web site gets a unique id. This id is stored as a cookie on the user agent. When the visitor returns to the site then he/she will be recognized by the cookie with the visitor id. This enables the possibility to remember the visitor's settings and preferences.

When a visitor logs on then the Zotonic can attach the user's visitor record to the user's rsc record. Zotonic can then also merge multiple visitor records into one. Enabling the possibility to remember preference changes between different user agents.


protect
-------

A simple table that prevents accidental deletions of protected rsc records. It does this by having a foreign key constraint that prohibits the deletion of the referred rsc record.


predicate_category
------------------

Defines which categories are valid as subject or object for a predicate. The admin uses this table to show only valid predicates whose valid subject match the category of the page being edited.


comment
-------

Comments on a page. Comments are not added as a separate rsc record because that will add many extra records and also because of access control restrictions. When a page is not visible to a certain user then its comments shouldn't be visible as well. To simplify this check the comments are placed separate and made part of the rsc record.

This separate comment table also helps with cleaning up comments when the rsc record is deleted.

.. seealso:: :ref:`mod_comment`


config
------

System and module configuration keys.

.. seealso:: :ref:`model-config`, :ref:`mod_admin_config`


rsc_pivot_queue
---------------

Queue with rsc ids that are modified and whose indices should be updated with the new information.


pivot_task_queue
----------------

Queue with tasks that will take a long time to run. They are executed by the pivoting process.


