<p>Send the page “{{ m.rsc[id].title }}” to:</p>

{% wire type="submit" id=#form postback={email_page id=id on_success=on_success on_success={growl text="Sent the e-mail."}} action={dialog_close} %}
<form id="{{ #form }}" method="post" action="postback">

	<div class="form-item">
		<label for="{{ #recipient }}">e-Mail</label>
		<input type="text" id="{{ #recipient }}" name="email" value="" />
		{% validate id=#recipient name="email" type={presence} type={email} %}
	</div>
	
	{% button text="Send e-mail" %}
	{% button text="Cancel" action={dialog_close} %}
</form>
