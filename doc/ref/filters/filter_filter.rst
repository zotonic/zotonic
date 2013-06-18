.. highlight:: django
.. include:: meta-filter.rst

Filters a list on the value of a property.

There are two ways to filter a list of values with this filter::

  {% print somelist|filter:`p` %}

This results in a list where all elements have the property ``p`` defined and
where the property (after conversion to boolean) is ``true``.

A second argument can be added to test on equality::

  {% print somelist|filter:`title`:"Untitled" %}

Shows all elements whose ``title`` property is "Untitled".

This can also be used to filter a list of resource ids on the presence of a property.

For example, see all published elements in a list of resource ids::

  {% print [1,2,3,4,5,6]|filter:`is_published` %}

.. seealso:: :ref:`filter-is_visible`, :ref:`filter-is_a`

