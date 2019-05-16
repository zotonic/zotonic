{% if m.auth2fa[id].is_totp_enabled %}
    <p class="alert alert-info">{_ Two-factor authentication is enabled for this user. _}</p>

    {% if (id == 1 and m.acl.user.id == 1) or id != 1%}
        {% button class="btn btn-default"
                  text=_"Remove two-factor..."
                  action={confirm
                      text=_"This will disable the two-factor authentication.<br>The old barcode will not be valid anymore."
                      ok=_"Remove"
                      postback={auth2fa_remove id=id}
                      delegate=`mod_auth2fa`
                  }
        %}
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
    {% else %}
        <p class="text-muted">{_ Only the admin can change the two-factor authentication of the admin. _}</p>
    {% endif %}
{% else %}
    <p>{_ Two-factor authentication is not enabled for this user. _}</p>

    {% if (id == 1 and m.acl.user.id == 1) or id != 1 %}
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
    {% else %}
        <p class="text-muted">{_ Only the admin can change the two-factor authentication of the admin. _}</p>
    {% endif %}

{% endif %}
