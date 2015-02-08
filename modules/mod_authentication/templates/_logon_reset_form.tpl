{#
Params:
- username: set by controller_logon
#}
{% if username %}
    {% wire id="password_reset" type="submit" postback={reset} delegate=`mod_authentication` %}
    <form id="password_reset" method="post" action="postback" class="z_logon_form">
        <input type="hidden" name="secret" value="{{ secret|escape }}" />

        {% include form_fields_tpl username=username %}

        {% javascript %}
            setTimeout(function(){
                z_init_postback_forms();
            }, 100);
        {% endjavascript %}
    </form>
{% else %}
    <h2 class="z-logon-title">{_ Sorry, your password reset code is unknown or expired _}</h2>
    <p>{_ For security reasons, password reset codes are only kept for a limited amount of time and can only be used once. _}</p>
    <p><strong>{_ Simply try again: _}</strong></p>
    <p><a href="{% url logon_reminder %}">{_ I forgot my password _}</a></p>
{% endif %}