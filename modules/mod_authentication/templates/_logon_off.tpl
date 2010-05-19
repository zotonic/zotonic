{% if m.acl.user %}
	<a class="logoff_link" href="{% url logoff %}">{_ Log Off _}</a>
{% else %}
	<a class="logon_link" href="{% url logon %}">{_ Log On _}</a>
{% endif %}
	