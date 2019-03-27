{#
Params:
- username: set by controller_logon
#}
{% if q.username and q.error != 'unknown_code' %}
    <form id="password_reset" method="post" class="z_logon_form"
          data-onsubmit-topic="model/auth-ui/post/form/reset">

        {% include form_fields_tpl %}

    </form>
{% elseif q.error == `ratelimit` %}
    <h2 class="z-logon-title">{_ Sorry, too many retries _}</h2>

    <p>
        {_ Please try again in _}
        {% with m.ratelimit.timeout as seconds %}
            {% if seconds == 3600 %}{_ an hour _}.
            {% elseif seconds > 3600 %}{{ ((seconds+3599)/3600)|round }} {_ hours _}.
            {% else %}{{ (seconds / 60)|round }} {_ minutes _}.
            {% endif %}
        {% endwith %}
    </p>
{% else %}
    <h2 class="z-logon-title">{_ Sorry, your password reset code is unknown or expired _}</h2>
    <p>{_ For security reasons, password reset codes are only kept for a limited amount of time and can only be used once. _}</p>
    <p><strong>{_ Simply try again: _}</strong></p>

    <p>
        <a href="{% url logon_reminder %}" data-onclick-topic="model/auth-ui/post/view/reminder">{_ I forgot my password _}</a>
    </p>
{% endif %}
