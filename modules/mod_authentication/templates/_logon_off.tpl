{% if m.acl.user %}
	<a href="{% url logoff %}">{_ Sign out _}</a>
{% else %}
	<a href="{% url logon %}">{_ Sign in _}</a>
{% endif %}

