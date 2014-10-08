
{% wire type="submit" id=#form postback={mail_page id=id on_success=on_success} action={dialog_close} delegate=delegate %}
<form id="{{ #form }}" method="post" action="postback">

    <p>{_ Please enter the e-mail address you want to send this page to. _}</p>

    <div class="control-label">
        <label class="control-label" for="email">{_ E-mail _}</label>
        <div>
            <input id="{{ #email }}" type="text" value="" class="input-xlarge form-control" autofocus name="email" />
            {% validate id=#email name="email" type={presence} type={email} %}
        </div>
    </div>

    <div class="modal-footer">
	    {% button class="btn btn-default" text=_"Cancel" action={dialog_close} tag="a" %}
	    {% button class="btn btn-primary" text=_"Send e-mail" %}
    </div>
</form>
