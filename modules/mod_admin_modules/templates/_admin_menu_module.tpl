{% if m.acl.is_admin %}
<li><a href="{% url admin_modules %}" {% if page_admin_modules %}class="current"{% endif %}>Modules</a></li>
{% endif %}
