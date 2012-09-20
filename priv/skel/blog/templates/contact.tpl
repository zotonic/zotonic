{% extends "page.tpl" %}

{% block below_body %}

{% wire id="contact-form" type="submit" postback={contact} delegate="controller_default_contact" %}
<form id="contact-form" method="post" action="postback" class="form">

	<div class="control-group">
	<label class="control-label" for="name">Name</label>
		<div class="controls">
			<input type="text" name="name" id="name" class="span4" />
		</div>
	</div>

	<div class="control-group">
	<label class="control-label" for="email">E-mail</label>
		<div class="controls">
			<input type="text" name="mail" id="email" class="span4" />
			{% validate id="email" type={email} type={presence} %}
		</div>
	</div>

	<div class="control-group">
	<label class="control-label" for="message">Message</label>
		<div class="controls">
		<textarea name="message" id="message" cols="60" rows="8" class="span4"></textarea>
			{% validate id="message" type={presence} %}
		</div>
	</div>

	<div class="control-group">
		<div class="controls">
			<button class="btn btn-primary" type="submit">Send</button>
		</div>
	</div>

</form>

{% endblock %}
