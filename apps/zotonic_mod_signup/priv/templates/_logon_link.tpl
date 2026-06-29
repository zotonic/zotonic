{% if not m.acl.user %}
	<p>{_ No account yet? _} <a href="{% url signup p=page %}" id="go_to_signup">{_ Sign up _}</a></p>
{% endif %}
