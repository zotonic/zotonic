
{% wire type="submit" id=#form postback={mailing_testaddress id=id} action={dialog_close} delegate="mod_mailinglist" %}
<form id="{{ #form }}" method="post" action="postback">

    <p>{_ Please enter the e-mail address you want to send a test mail to. _}</p>

    <div class="form-group">
        <div class="control-label">
            <label class="control-label" for="email">{_ E-mail _}</label>
            <div>
                <input id="{{ #email }}" type="text" value="{{ m.acl.user.email }}" class="input-xlarge form-control" autofocus name="email" />
                {% validate id=#email name="email" type={presence} type={email} %}
            </div>
        </div>
    </div>
    <div class="modal-footer">
        {% button class="btn btn-default" text=_"Cancel" action={dialog_close} tag="a" %}
        {% button class="btn btn-primary" type="submit" text=_"Send e-mail" %}
    </div>
</form>
