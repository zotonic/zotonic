{% if not m.auth2fa.is_totp_enabled %}
    {% with m.auth2fa.session_mode as mode %}
         {% if mode >= 2 or (mode == 1 and not m.auth2fa.is_totp_requested) %}
            {% wire postback={request_2fa} delegate=`mod_auth2fa` %}
        {% endif %}
    {% endwith %}
{% endif %}
