{% if m.acl.use.mod_translation %}
	<li><a href="{% url admin_translation %}" {% ifequal selected "translation" %}class="current"{% endifequal %}>{_ Translation _}</a></li>
{% endif %}
