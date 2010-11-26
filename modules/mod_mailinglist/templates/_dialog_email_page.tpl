<p>{_ Send the page _} “{{ m.rsc[id].title }}” {_ to: _}</p>

{% wire type="submit" id=#form postback={email_page id=id on_success=on_success on_success={growl text=_"Sent the e-mail."}} action={dialog_close} %}
<form id="{{ #form }}" method="post" action="postback">

	<div class="form-item">
		<label for="{{ #recipient }}">{_ e-Mail _}</label>
		<input type="text" id="{{ #recipient }}" name="email" value="" />
		{% validate id=#recipient name="email" type={presence} type={email} %}
	</div>
	
	{% button text=_"Send e-mail" %}
	{% button text=_"Cancel" action={dialog_close} %}
</form>
