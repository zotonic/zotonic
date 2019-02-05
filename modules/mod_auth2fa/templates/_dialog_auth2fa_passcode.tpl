<p>{_ Scan the two-factor authentication barcode with your App _}</p>

<p style="text-align: center">
    <img src="{{ m.auth2fa[id].totp_image_url }}" style="width: 200px; height: 200px; max-width: 90%">
</p>

<p>
    {_ From now on an extra passcode is needed to sign in. _}
</p>

<div class="modal-footer">
    {% button tag="a" class="btn btn-primary" text=_"Close" action={dialog_close} %}
</div>
