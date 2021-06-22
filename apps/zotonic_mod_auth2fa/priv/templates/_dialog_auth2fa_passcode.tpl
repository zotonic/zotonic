{% if id == m.acl.user %}
    <p>{_ Scan the two-factor authentication QR code with an app such as <a target="_blank" rel="noopener noreferrer" href="https://support.google.com/accounts/answer/1066447">Google Authenticator</a> or <a target="_blank" rel="noopener noreferrer" href="https://duo.com/product/trusted-users/two-factor-authentication/duo-mobile">Duo Mobile</a>. _}<br>
        {_ If you are not able to scan the QR code then copy the code manually to your authentication App. _}
    </p>

    {% with m.auth2fa.totp_image_url[request_key] as totp %}
        <p style="text-align: center">
            <img src="{{ totp.url }}" style="width: 200px; height: 200px; max-width: 90%">
        </p>
        <p style="text-align: center" class="form-inline">
            <input disabled
                   type="text"
                   value="{{ totp.secret }}"
                   id="{{ #secret }}"
                   style="text-align: center">
            <button class="btn btn-default" id="{{ #btn }}"><span class="fa fa-copy"></span> {_ Copy _}</button>
            {% wire id=#btn
                    action={script
                        script="
                            document.getElementById('" ++ #secret ++ "').select();
                            document.execCommand('copy');
                        "
                    }
                    action={growl text=_"Copied to clipboard"}
            %}
        </p>
    {% endwith %}

    <p>
        {_ From now on an extra passcode is needed to sign in. _}
    </p>

    <div class="modal-footer">
        {% button tag="a" class="btn btn-primary" text=_"Close" action={dialog_close} %}
        {% button tag="a"
                  class="pull-left btn btn-danger"
                  text=_"Remove two-factor"
                  action={dialog_close}
                  postback={auth2fa_remove id=m.acl.user}
                  delegate=`mod_auth2fa`
        %}
    </div>
{% else %}
    <p class="alert alert-info">
        {_ Only the user themselves can set their new two-factor authentication QR code. _}
    </p>

    <div class="modal-footer">
        {% button tag="a" class="btn btn-primary" text=_"Close" action={dialog_close} %}
    </div>
{% endif %}
