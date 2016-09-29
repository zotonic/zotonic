.. _guide-datamodel:

The Zotonic data model
======================

Zotonic’s data model can be seen as a pragmatic implementation of the
`Semantic Web <http://en.wikipedia.org/wiki/Semantic_Web>`_: a mixture
between a traditional database and a triple store.

The data model has two main concepts: the :term:`resource` and the :term:`edge`.

Resources, which are often called *pages* in the admin, are the main
data unit: they have properties like title, summary, body text; and
importantly, they belong to a certain :term:`category`.

Edges are nothing more than connections between two resources. Each
edge is labeled, with a so-called :term:`predicate`.

This manual describes the data model and its related database table in depth.

.. _guide-datamodel-example:

The data model by example
-------------------------

.. image:: /img/exampledomain.png
   :align: right

Take the example data on the right. It shows resources, connected to
each other by edges. The rectangular blocks denote the resources, the
arrow between them denote the edges.

It shows the :term:`domain model` of a basic blog-like structure:
`article` resources which are written by `persons` and articles being
tagged with keywords.

The edge between `Alice` and `cooking for dummies` is labelled
`author`: indicating that Alice wrote that article.

Note that these edges have a direction. When reasoning about edges, it
is the easiest to think of them in terms of grammar: `Subject - verb -
object
<http://en.wikipedia.org/wiki/Subject%E2%80%93verb%E2%80%93object>`_. In
the case of Alice:

- **Cooking for dummies** is authored by **Alice**;
- **Cooking for dummies** has the keyword **Cooking**;

or more general:

- **subject**  **predicate**  **object**.


In Zotonic, the terms `subject` and `object` (shortened as `s` and
`o`) are used in the templates, allowing you to traverse the edges
between resources::

  {{ m.rsc[id].o.author[1].title }}

Returns the name of the first author of the article with the given
`id`, by traversing to the first `author` object edge of the
given id. See the :ref:`model-rsc` model for more details on this.

.. _pages:

Pages
-----

Pages are the main building block of Zotonic's :ref:`data model
<guide-datamodel>`. For simplicity of communication, a resource is
often referred to as a page. Every resource usually has its own page
on the web site.

By default, each page has the following properties.

================= ==============================================================
id                Unique number that identifies the resource and can be used for
                  referring to the resource
title             Main title of the page
summary           Short, plain-text, summary
short title       Short version of the title that is used in navigation and
                  other places where an abbreviated title is shown
published         Only published pages are visible to the general public

publication start Date at which the page will be published and
publication end   Data at which the page will be unpublished

name              Alternative for id: used to uniquely identify the resource
page path         The page’s URL on the site. Defaults to ``page/{id}/{title}``
================= ==============================================================

.. seealso::

    :ref:`resources <guide-datamodel-resources>` in the Developer Guide

.. _guide-datamodel-categories:

Categories
----------

Each page belongs to one category. The category a page is in determines how
it is displayed.

.. _guide-datamodel-edges:

Edges
-----

An :term:`edge` is a labeled connection between two resources.

The ``edge`` table defines these relations between resources. It does
this by adding a directed edge from one rsc record (:index:`subject`)
to another (:index:`object`). It also adds a reference to the
:term:`predicate`: the label of the edge.

In the admin, edges are represented in the "Page connections" sidebar
panel, of the edit page of the `subject`: the resource where the edges
originate. By convention, edges are said to `belong` to their subject.
This is to simplify the access control: if you are allowed to edit the
resource, you’re also allowed to edit its `outgoing` edges ("Page
connections" in the admin), creating connections to other resources.

.. seealso:: :ref:`model-edge`

.. _guide-datamodel-edge-predicates:

Predicates
----------

Edges have a label: like in :ref:`guide-datamodel-example`, `author`
is a :term:`predicate` of an edge which denotes that a certain
`article` was written by a certain `person`

Just like categories, these predicates are themselves also resources:
allowing you to specify metadata, give them a meaningful title, et
cetera.

Each predicate has a list of valid subject categories and valid object
categories (stored in the ``predicate_category`` table). This is used
to filter the list of predicates in the admin edit page, and also to
filter the list of found potential objects when making a
connection. On their edit page in the admin interface, you can edit
the list of valid subject and object categories for a predicate.

.. seealso:: :ref:`model-predicate`

Further reading
---------------

* Zotonic’s defaults resources and categories: the
  :ref:`domain model <guide-domain-model>`.
