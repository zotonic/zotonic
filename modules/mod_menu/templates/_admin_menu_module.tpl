{% if m.acl.is_admin or m.acl.is_public_publisher %}
<li><a href="{% url admin_menu %}" {% if page_admin_menu %}class="current"{% endif %}>Menu</a></li>
{% endif %}
