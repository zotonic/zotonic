{#
Params:
- username: set by controller_logon
#}
{% if username %}
    {% wire id="password_reset" type="submit" postback={reset secret=secret username=username} delegate=`controller_logon` %}
    <form id="password_reset" method="post" action="postback" class="z_logon_form">
        {% include form_fields_tpl username=username %}
        {% javascript %}
            setTimeout(function(){
                z_init_postback_forms();
            }, 100);
        {% endjavascript %}
    </form>
{% elseif error == `ratelimit` %}
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
    <p><a href="{% url logon_reminder %}">{_ I forgot my password _}</a></p>
{% endif %}
