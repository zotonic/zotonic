{% if m.acl.use.mod_backup %}
<li><a href="{% url admin_backup %}" {% if page_admin_backup %}class="current"{% endif %}>{_ Backups _}</a></li>
{% endif %}
