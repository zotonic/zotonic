.. highlight:: django
.. include:: meta-is_visible.rst

Filter a list of resource ids so that only the visible ids remain.

This filter can be applied to a list of resource ids or a single resource id.

This filter can be applied to a list of resource ids. Only those resource ids that are visible for the current user remain. Optionally the filter only returns the first n matches.

An example::

  <ul>
  {% for part_id in m.rsc[id].o.haspart|is_visible %}
      <li>{{ m.rsc[part_id].title }}</li>
  {% endfor %}
  </ul>

This will list all collection members that are visible, preventing
empty list items.

Whilst::

  {% for part_id in m.rsc[id].o.haspart|is_visible:3 %}
      {{ m.rsc[part_id].title }}
  {% endfor %}

Lists only the first three collection members that are visible.
