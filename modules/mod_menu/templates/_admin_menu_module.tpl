{% if m.acl.use.mod_admin_menu %}
<li><a href="{% url admin_menu %}" {% if page_admin_menu %}class="current"{% endif %}>Menu</a></li>
{% endif %}
