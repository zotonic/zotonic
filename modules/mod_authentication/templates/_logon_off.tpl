{% if m.acl.user %}
	<a href="{% url logoff %}">{_ Log Off _}</a>
{% else %}
	<a href="{% url logon %}">{_ Log On _}</a>
{% endif %}
	