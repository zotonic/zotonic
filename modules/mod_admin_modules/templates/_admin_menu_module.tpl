{% if m.acl.is_admin %}
<li><a href="{% url admin_modules %}" {% ifequal selected "modules" %}class="current"{% endifequal %}>Modules</a></li>
{% endif %}
