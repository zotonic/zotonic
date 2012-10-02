.. highlight:: django
.. include:: meta-is_not_a.rst

is_not_a mirrors :ref:`filter-is_a`. It is particularly useful when
iterating over a category and excluding members of a sub-category
(iterating over all images associated with a page except images in the
thumbnail category).

Example for looping over all media in a rsc but excluding the
thumbnail resources::

  {% for m in m.rsc[id].media|is_not_a:"thumbnail" %}
  ...
  {% endfor %}

.. seealso:: :ref:`filter-is_a`

