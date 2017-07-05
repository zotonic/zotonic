{% if tokens %}
    <p>{_ There are users using this application. If you delete this application, you will revoke access and most likely their applications will break. _}</p>
{% else %}
    <p>{_ This application is not yet used by anybody. _}</p>
{% endif %}

<p>{_ Do you really want to delete this application? _}</p>
<div class="modal-footer">
    {% button class="btn btn-default" action={dialog_close} text=_"Cancel" tag="a" %}
    {% button class="btn btn-primary" text=_"Delete this application!" postback={confirm_del_app id=id} %}
</div>
