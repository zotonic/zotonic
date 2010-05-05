{% if m.acl.is_admin %}
<li><a href="{% url admin_backup %}" {% if page_admin_backup %}class="current"{% endif %}>Backups</a></li>
{% endif %}
