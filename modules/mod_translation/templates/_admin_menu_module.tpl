{% if m.acl.use.mod_translation %}
	<li><a href="{% url admin_translation %}" {% if page_admin_translation %}class="current"{% endif %}>{_ Translation _}</a></li>
{% endif %}
