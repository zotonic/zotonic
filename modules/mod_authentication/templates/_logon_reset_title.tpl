{#
Params:
- username: set by controller_logon
#}
{% if username %}
<h2 class="z-logon-title">{_ Reset your password _}</h2>

<p>{_ Enter the new password for your account _} <strong>{{ username|escape }}</strong>.</p>
{% endif %}