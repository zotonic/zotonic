The datamodel by example
------------------------

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
