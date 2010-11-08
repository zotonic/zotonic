{% if m.acl.is_admin %}
	<li><a href="{% url admin_config %}" {% if page_admin_config %}class="current"{% endif %}>{_ Config _}</a></li>
{% endif %}
