{#
Params:
- username: set by controller_logon
#}
{% if q.username %}
    {% if q.is_expired %}
        <h2 class="z-logon-title">{_ Please enter a new password _}</h2>

        <p>{_ Your current password is expired, does not meet our security requirements, or has been found in <a href="https://haveibeenpwned.com/Passwords" target="_blank" rel="noreferrer noopener">public records</a>. _}</p>
    {% else %}
        <h2 class="z-logon-title">{_ Reset your password _}</h2>
    {% endif %}

    <p>{_ Enter the new password for your account _} <strong>{{ q.username|escape }}</strong>.</p>
{% endif %}
