{% if 'facebook'|member:identity_types and m.config.mod_facebook.useauth.value and m.config.mod_facebook.appid.value %}
<p>{_ You have coupled your <strong>Facebook</strong> account. _} <a href="{% url logon use_absolute_url%}">Log on with Facebook</a></p>
{% endif %}
