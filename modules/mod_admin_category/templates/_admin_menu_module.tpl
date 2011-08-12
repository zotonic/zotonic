{% if m.acl.insert.category %}
<li><a href="{% url admin_category_sorter %}" {% if page_admin_category_sorter %}class="current"{% endif %}>{_ Categories _}</a></li>
{% endif %}

