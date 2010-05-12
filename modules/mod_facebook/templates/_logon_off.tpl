{% if m.acl.user %}
	<a class="logoff_link" href="{% url logoff %}" 
	{% if m.session.facebook_logon %}onclick="FB.logout(function() { window.location = '{% url logoff %}'}); return false;"{% endif %}
	>Log Off</a>
{% else %}
	<a class="logon_link" href="{% url logon %}">Log On</a>
{% endif %}
