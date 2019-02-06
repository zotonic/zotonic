<p>{_ Scan the two-factor authentication barcode with an app such as <a href="https://support.google.com/accounts/answer/1066447">Google Authenticator</a> or <a href="https://duo.com/product/trusted-users/two-factor-authentication/duo-mobile">Duo Mobile</a>. _}</p>

<p style="text-align: center">
    <img src="{{ m.auth2fa[id].totp_image_url }}" style="width: 200px; height: 200px; max-width: 90%">
</p>

<p>
    {_ From now on an extra passcode is needed to sign in. _}
</p>

<div class="modal-footer">
    {% button tag="a" class="btn btn-primary" text=_"Close" action={dialog_close} %}
</div>
