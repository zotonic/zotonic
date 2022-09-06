{# This template is only shown iff no passcode entry field is shown. #}
{% if m.config.mod_auth2fa.mode.value == "3" %}
    {% include "_logon_login_set_passcode.tpl" is_reset %}
{% endif %}
