{% if m.acl.use.mod_comment %}
<li><a href="{% url admin_comments %}" {% if page_admin_comments %}class="current"{% endif %}>Comments</a></li>
{% endif %}
