<p>{_ Please enter the e-mail address you want to send a test mail to. _}</p>

{% wire type="submit" id=#form postback={mailing_testaddress id=id} action={dialog_close} delegate="mod_mailinglist" %}
<form id="{{ #form }}" method="post" action="postback">

		<div class="form-item clearfix">
            <label for="email">{_ Email _}</label>
            <input id="email" type="text" value="" name="email" />
            {% validate id="email" type={presence} type={email} %}
        </div>

		<div class="form-item clearfix">
            {% button text=_"Send mailing" %}
            {% button text=_"Cancel" action={dialog_close} %}
        </div>
</form>
