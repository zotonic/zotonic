{% if m.auth2fa[id].is_totp_enabled %}
    <span class="label label-success" title="{_ Two-factor authentication is enabled _}">√ {_ 2FA _}</span>
{% else %}
    <span class="label label-danger" title="{_ Two-factor authentication is not enabled _}">&times; {_ 2FA _}</span>
{% endif %}