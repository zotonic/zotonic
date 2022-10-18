{# This template is only shown iff no passcode entry field is shown. #}
{% if m.auth2fa[user_id].user_mode == 3 %}
    {% include "_logon_login_set_passcode.tpl" is_reset user_id=user_id %}
{% endif %}
