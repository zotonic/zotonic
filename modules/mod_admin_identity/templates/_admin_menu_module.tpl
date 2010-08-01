{% if m.acl.use.mod_admin_identity %}
<li><a href="{% url admin_user %}" {% ifequal selected "users" %}class="current"{% endifequal %}>{_ Users _}</a></li>
{% endif %}
