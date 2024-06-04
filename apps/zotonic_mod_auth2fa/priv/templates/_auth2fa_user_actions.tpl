{% if m.auth2fa[id].is_totp_enabled %}
    <p><span class="fa fa-check-circle"></span> {_ Two-factor authentication is enabled for this account. _}</p>

    <p>
      {% if (id == 1 and m.acl.user.id == 1) or id != 1 %}
          {% button class="btn btn-default"
                    text=_"Remove two-factor..."
                    action={confirm
                        text=_"This will disable the two-factor authentication.<br>The old QR code will not be valid anymore."
                        ok=_"Remove two-factor"
                        is_danger
                        postback={auth2fa_remove id=id}
                        delegate=`mod_auth2fa`
                    }
          %}
      {% endif %}

      {% if id == m.acl.user %}
          {% button class="btn btn-default"
                    text=_"Reset two-factor..."
                    postback={dialog_2fa}
                    delegate=`mod_auth2fa`
          %}
      {% endif %}
    </p>
{% else %}
    <p class="text-danger"><span class="fa fa-warning"></span> {_ Two-factor authentication is not enabled for this account. _}</p>

    {% if id == m.acl.user %}
        <p>
          {% button class="btn btn-default"
                  text=_"Enable two-factor authentication..."
                  postback={request_2fa}
                  delegate=`mod_auth2fa`
          %}
        </p>
    {% endif %}

{% endif %}

{% if id == 1 %}
    <p class="text-muted">{_ Only the admin can change the two-factor authentication of the admin. _}</p>
{% endif %}
