{#
Params:
- username: set by controller_logon
#}
{% if q.username %}
    {% if q.is_expired %}
        <h2 class="z-logon-title">{_ Your password has expired _}</h2>
    {% else %}
        <h2 class="z-logon-title">{_ Reset your password _}</h2>
    {% endif %}

    <p>{_ Enter the new password for your account _} <strong>{{ q.username|escape }}</strong>.</p>
{% endif %}
