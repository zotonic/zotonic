{#
Link contents of connection list item.

Params:
- id
#}
{% with id.title as title %}
{% image id mediaclass="admin-list-dashboard" %}
<span class="menu-label">{% if title %}{{ title|truncate_html:30 }}{% else %}<em>{_ untitled _}</em>{% endif %}</span>
<span class="category">{{ id.category_id.title|truncate_html:20 }}</span>
{% endwith %}