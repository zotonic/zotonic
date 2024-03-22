{# This template is only shown iff no passcode entry field is shown. #}
{% if m.auth2fa.mode == 3 %}
    {% include "_logon_login_set_passcode.tpl" is_reset %}
{% endif %}