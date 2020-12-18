{% if 'linkedin'|member:identity_types and m.linkedin.useauth %}
<p>{_ You have coupled your <strong>LinkedIn</strong> account. _} <a href="{% url logon absolute_url%}">{_ Log on with LinkedIn _}</a></p>
{% endif %}
