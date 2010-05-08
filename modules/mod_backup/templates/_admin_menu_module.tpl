{% if m.acl.use.mod_backup %}
<li><a href="{% url admin_backup %}" {% if page_admin_backup %}class="current"{% endif %}>Backups</a></li>
{% endif %}
