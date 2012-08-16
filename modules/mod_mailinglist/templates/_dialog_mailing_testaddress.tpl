
{% wire type="submit" id=#form postback={mailing_testaddress id=id} action={dialog_close} delegate="mod_mailinglist" %}
<form id="{{ #form }}" method="post" action="postback">

    <p>{_ Please enter the e-mail address you want to send a test mail to. _}</p>
    
    <div class="control-label">
        <label class="control-label" for="email">{_ Email _}</label>
        <div class="controls">
            <input id="email" type="text" value="" class="input-xlarge do_autofocus" name="email" />
            {% validate id="email" type={presence} type={email} %}
        </div>
    </div>

    <div class="modal-footer">
        {% button class="btn" text=_"Cancel" action={dialog_close} tag="a" %}
        {% button class="btn btn-primary" text=_"Send mailing" %}
    </div>
</form>
