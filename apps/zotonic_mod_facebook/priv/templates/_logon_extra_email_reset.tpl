{% if 'facebook'|member:identity_types and m.facebook.useauth %}
<p>{_ You have coupled your <strong>Facebook</strong> account. _} <a href="{% url logon absolute_url%}">Log on with Facebook</a></p>
{% endif %}
