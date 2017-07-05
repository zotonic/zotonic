{% if 'instagram'|member:identity_types and m.config.mod_instagram.consumer_key.value and m.config.mod_instagram.useauth.value %}
<p>{_ You have coupled your <strong>Instagram</strong> account. _} <a href="{% url logon absolute_url%}">Log on with Instagram</a></p>
{% endif %}
