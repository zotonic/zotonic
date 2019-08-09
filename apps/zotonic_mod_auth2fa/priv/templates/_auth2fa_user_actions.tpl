{% if m.auth2fa[id].is_totp_enabled %}
    <p><span class="fa fa-check-circle"></span> {_ Two-factor authentication is enabled for this user. _}</p>

    <p>
      {% if (id == 1 and m.acl.user.id == 1) or id != 1 %}
          {% button class="btn btn-default"
                    text=_"Remove two-factor..."
                    action={confirm
                        text=_"This will disable the two-factor authentication.<br>The old barcode will not be valid anymore."
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
                    action={confirm
                        text=_"This will generate a new barcode for two-factor authentication.<br>The old barcode will not be valid anymore."
                        ok=_"Generate barcode"
                        action={dialog_open
                                  title=_"Scan two-factor authentication barcode"
                                  template="_dialog_auth2fa_passcode.tpl"
                                  id=id
                         }
                    }
          %}
      {% endif %}
    </p>
{% else %}
    <p class="text-danger"><span class="fa fa-warning"></span> {_ Two-factor authentication is not enabled for this user. _}</p>

    {% if id == m.acl.user %}
        <p>
          {% button class="btn btn-default"
                  text=_"Enable two-factor authentication..."
                  action={confirm
                      text=_"This will generate a new barcode.<br>From then on you will need to use a passcode to sign in."
                      ok=_"Generate barcode"
                      action={dialog_open
                            title=_"Scan two-factor authentication barcode"
                            template="_dialog_auth2fa_passcode.tpl"
                            id=id
                            backdrop=`static`
                      }
                   }
          %}
        </p>
    {% endif %}

{% endif %}

{% if id == 1 %}
    <p class="text-muted">{_ Only the admin can change the two-factor authentication of the admin. _}</p>
{% endif %}
