{% if id == m.acl.user %}
    <p>{_ Scan the two-factor authentication barcode with an app such as <a href="https://support.google.com/accounts/answer/1066447">Google Authenticator</a> or <a href="https://duo.com/product/trusted-users/two-factor-authentication/duo-mobile">Duo Mobile</a>. _}</p>

    <p style="text-align: center">
        <img src="{{ m.auth2fa.totp_image_url[request_key] }}" style="width: 200px; height: 200px; max-width: 90%">
    </p>

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
        {_ Only the user themselves can set their new two-factor authentication barcode. _}
    </p>

    <div class="modal-footer">
        {% button tag="a" class="btn btn-primary" text=_"Close" action={dialog_close} %}
    </div>
{% endif %}
