{% extends "page.tpl" %}

{% block below_body %}

{% wire id="contact-form" type="submit" postback={contact} delegate="resource_default_contact" %}
<form id="contact-form" method="post" action="postback">

  	<div class="form-item">
		<label for="name">Name</label>
    	<input type="text" name="name" id="name" />
   	</div>

	<div class="form-item">
		<label for="email">E-mail</label>
    	<input type="text" name="mail" id="mail" />
    	{% validate id="mail" type={email} type={presence} %}
	</div>
	
	<div class="form-item">
	    <label for="message">Message</label>
	    <textarea name="message" id="message" cols="60" rows="8"></textarea>
    	{% validate id="message" type={presence} %}
	</div>

    <button type="submit">Send</button>

</form>

{% endblock %}