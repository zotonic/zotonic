{% wire id=#form type="submit" postback={recipient_add id=id} delegate=delegate %}
<form id="{{ #form }}" method="post" action="postback">

	<p>Give the e-mail address of the recipient, and check if the recipient will receive a welcome message.</p>

	<div class="form-item">
		<label for="{{ #email }}">e-Mail</label>
		<input type="text" id="{{ #email }}" name="email" value="" />
		{% validate id=#email name="email" type={presence} type={email} %}
	</div>

	<div class="form-item">
		<label for="{{ #welcome }}">Send welcome</label>
		<input type="checkbox" id="{{ #welcome }}" name="send_welcome" value="1" checked="checked" />
	</div>

	{% button text="subscribe" %}
	{% button text="cancel" action={dialog_close} %}

</form>
