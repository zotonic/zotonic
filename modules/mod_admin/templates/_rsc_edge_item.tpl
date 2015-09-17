{#
Link contents of connection list item.

Params:
- id
#}
{% with m.rsc[id].title as title %}
{% image id mediaclass="admin-list-dashboard" %}
{{ title|truncate:30|default:"<i>untitled</i>" }}
<span class="category">{{ id.category_id.title|truncate:20 }}</span>
{% endwith %}