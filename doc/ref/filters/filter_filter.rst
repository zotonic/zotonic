.. highlight:: django
.. include:: meta-filter.rst

Filters a list on the value of a property, either on presence or equality.

Testing presence
----------------

To filter a list of values::

  {% print somelist|filter:`p` %}

Results in a list where all elements have the property ``p`` defined and
where the property (after conversion to boolean) is ``true``.

This can be used to filter a list of resource ids on the presence of a property. For example, to see all published elements in a list of resource ids::

  {% print [1,2,3,4,5,6]|filter:`is_published` %}
  
To find all pages from page connection ``hasdocument`` that have an image::

  {% print id.o.hasdocument|filter:`depiction` %}


Testing equality
----------------

A second argument can be added to test on equality::

  {% print somelist|filter:`title`:"Untitled" %}

Shows all elements whose ``title`` property is "Untitled".



.. seealso:: :ref:`filter-is_visible`, :ref:`filter-is_a`

