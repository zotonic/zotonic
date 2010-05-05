{% if m.acl.is_admin %}
<li><a href="{% url admin_acl %}" {% ifequal selected "acl" %}class="current"{% endifequal %}>Access Control</a></li>
{% endif %}
