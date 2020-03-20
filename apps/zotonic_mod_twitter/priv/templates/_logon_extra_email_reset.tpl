{% if 'twitter'|member:identity_types and m.twitter.useauth %}
<p>{_ You have coupled your <strong>Twitter</strong> account. _} <a href="{% url logon absolute_url%}">{_ Log on with Twitter _}</a></p>
{% endif %}
