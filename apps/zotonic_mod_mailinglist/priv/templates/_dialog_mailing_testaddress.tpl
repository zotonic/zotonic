
{% wire type="submit" id=#form postback={mailing_testaddress id=id} action={dialog_close} delegate="mod_mailinglist" %}
<form id="{{ #form }}" method="post" action="postback">

    <p>{_ Please enter the e-mail address you want to send a test mail to. _}</p>

    <div class="form-group label-floating">
        <input id="{{ #email }}" type="email" value="{{ m.acl.user.email }}" class="form-control" autofocus name="email" placeholder="{_ E-mail _}">
        <label class="control-label" for="email">{_ E-mail _}</label>
        {% validate id=#email name="email" type={presence} type={email} %}
    </div>
    <div class="modal-footer">
        {% button class="btn btn-default" text=_"Cancel" action={dialog_close} tag="a" %}
        {% button class="btn btn-primary" type="submit" text=_"Send e-mail" %}
    </div>
</form>
