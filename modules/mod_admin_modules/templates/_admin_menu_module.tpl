{% if m.acl.is_admin %}
<li><a href="{% url admin_modules %}" {% ifequal selected "modules" %}class="current"{% endifequal %}>{_ Modules _}</a></li>
{% endif %}
