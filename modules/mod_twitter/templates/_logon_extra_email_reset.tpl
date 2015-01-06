{% if 'twitter'|member:identity_types and m.config.mod_twitter.consumer_key.value and m.config.mod_twitter.useauth.value %}
<p>{_ You have coupled your <strong>Twitter</strong> account. _} <a href="{% url logon use_absolute_url%}">Log on with Twitter</a></p>
{% endif %}
