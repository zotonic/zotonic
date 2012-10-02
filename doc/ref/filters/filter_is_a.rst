.. highlight:: django
.. include:: meta-is_a.rst

Filter a list of resource ids on category, or test if a single
resource id belongs to a category.

This filter can be applied to a list of resource ids or a single
resource id.

When it is applied to a list then it will filter the list of ids. Only
those resource ids that belong to a certain category
remain. Optionally the filter only returns the first n matches.

When applied to a single integer (resource id), then it will return a
boolean.  True when the id belongs to the parameter's category, false
otherwise.

Apply to a single resource id
-----------------------------

Example::

  {{ 1|is_a:"person"|yesno }}

Will output "yes", because the resource with id 1 is a person (the
System Administrator).

Apply to a list of resource ids
-------------------------------

When applied to a list of ids::

  {% for part_id in m.rsc[id].o.haspart|is_a:"person" %}
      {{ m.rsc[part_id].title }}
  {% endfor %}

This will list all collection members that are a person. While::

  {% for part_id in m.rsc[id].o.haspart|is_a:"person":3 %}
      {{ m.rsc[part_id].title }}
  {% endfor %}

Lists only the first three collection members that are a person.

.. seealso:: :ref:`filter-is_not_a`
