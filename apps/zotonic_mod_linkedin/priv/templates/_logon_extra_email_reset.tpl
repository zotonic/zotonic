{% if 'linkedin'|member:identity_types and m.config.mod_linkedin.useauth.value and m.config.mod_linkedin.appid.value %}
<p>{_ You have coupled your <strong>LinkedIn</strong> account. _} <a href="{% url logon absolute_url%}">{_ Log on with LinkedIn _}</a></p>
{% endif %}
