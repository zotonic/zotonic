{% if m.acl.user %}
	<a class="logoff_link" href="{% url logoff %}">Log Off</a>
{% else %}
	<a class="logon_link" href="{% url logon %}">Log On</a>
{% endif %}
	