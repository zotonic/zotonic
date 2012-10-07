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

.. toctree::
   :maxdepth: 2
           
   resources
   edges
   search
   custompivots
   domainmodel


An example
----------

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








visitor / visitor_cookie
------------------------

Every visitor to a Zotonic web site gets a unique id. This id is
stored as a cookie on the user agent. When the visitor returns to the
site then he/she will be recognized by the cookie with the
visitor id. This enables the possibility to remember the visitor's
settings and preferences.

When a visitor logs on then the Zotonic can attach the user's visitor
record to the user's rsc record. Zotonic can then also merge multiple
visitor records into one. Enabling the possibility to remember
preference changes between different user agents.

