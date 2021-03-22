{% if 'microsoft'|member:identity_types and m.microsoft.useauth %}
<p>{_ You have coupled your <strong>Microsoft</strong> account. _} <a href="{% url logon absolute_url%}">{_ Log on with Microsoft _}</a></p>
{% endif %}
