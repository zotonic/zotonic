{# Show the form with the passcode to be saved by the user, could also be a QR code #}
{% wire action={update target="set_passcode" template="_logon_login_set_passcode.tpl"} %}
{% wire action={hide target=" .username-password"} %}
