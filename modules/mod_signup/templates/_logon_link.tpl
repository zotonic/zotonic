{% if not m.acl.user %}
	<p id="logon_link_signup" class="logon_link"><a href="{% url signup p=page %}">{_ I don’t have an account, please sign me up. _}</a></p>
{% endif %}