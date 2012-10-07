.. _manual-datamodel:

The Zotonic data model
======================


Zotonic's data model can be seen as a pragmatic implementation of the
`Semantic Web <http://en.wikipedia.org/wiki/Semantic_Web>`_: a mixture
between a traditional database and a triple store.

The data model has two main concepts: the :term:`resource` and the :term:`edge`.

Resources, in the admin often called `pages`, are the main data unit:
they have properties like title, summary, body text; and, important,
they belong to a certain :term:`category`.

Edges are nothing more than connections between two resources. Each of
such connections are labeled, with a so-called :term:`predicate`.

This manual describes the data model and its related database table in depth.

Table of contents
-----------------

.. toctree::
   :maxdepth: 2

   example
   resources
   edges
   search
   custompivots
   domainmodel



