{% if m.acl.use.mod_development %}
<li><a href="{% url admin_development %}" {% ifequal selected "development" %}class="current"{% endifequal %}>Development</a></li>
{% endif %}


<li><a href="{% url wmtrace_conf star='' %}" {% if page_admin_wmtrace_conf %}class="current"{% endif %}>WM Trace Config</a></li>
<li><a href="{% url wmtrace star='' %}" {% if page_admin_wmtrace %}class="current"{% endif %}>Webmachine Trace</a></li>
