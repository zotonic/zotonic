{% if 'twitter'|member:identity_types and m.twitter.useauth %}
<p>{_ You have coupled your <strong>Twitter</strong> account. _} <a href="{% url logon absolute_url%}">Log on with Twitter</a></p>
{% endif %}
