{% if m.auth2fa[id].is_totp_enabled %}
    <p class="alert alert-info">{_ Two factor authentication is enabled for this user. _}</p>

    {% button class="btn btn-default"
              text=_"Remove 2FA..."
              action={confirm
                  text=_"This will disable the 2FA.<br>The old barcode will not be valid anymore."
                  ok=_"Remove"
                  postback={auth2fa_remove id=id}
                  delegate=`mod_auth2fa`
              }
    %}
    {% button class="btn btn-default"
              text=_"Reset 2FA..."
              action={confirm
                  text=_"This will generate a new barcode.<br>The old barcode will not be valid anymore."
                  ok=_"Generate barcode"
                  action={dialog_open
                            title=_"Scan 2FA Passcode"
                            template="_dialog_auth2fa_passcode.tpl"
                            id=id
                   }
              }
    %}
{% else %}
    <p>{_ Two factor authentication is not enabled for this user. _}</p>

    {% button class="btn btn-default"
              text=_"Scan 2FA barcode..."
              action={confirm
                  text=_"This will generate a new barcode.<br>From then on you will need to use a passcode to log on."
                  ok=_"Generate barcode"
                  action={dialog_open
                        title=_"Scan 2FA Passcode"
                        template="_dialog_auth2fa_passcode.tpl"
                        id=id
                        backdrop=`static`
                  }
               }
    %}
{% endif %}
