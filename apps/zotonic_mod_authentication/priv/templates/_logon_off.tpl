{% if m.acl.user %}
	<a href="{% url logoff %}">{_ Log out _}</a>
{% else %}
	<a href="{% url logon %}">{_ Log in _}</a>
{% endif %}

