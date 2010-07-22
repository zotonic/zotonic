{% extends "page.tpl" %}

{% block below_body %}

{% wire id="contact-form" 
        type="submit" 
        postback={contact email_template="email_contact.tpl"} 
        delegate="mod_contact" %}
<form id="contact-form" method="post" action="postback">
 	<div class="form-item">
		<label for="name">{_ Name _}</label>
    	<input type="text" name="name" id="name" />
   	</div>

	<div class="form-item">
		<label for="email">{_ E-mail _}</label>
    	<input type="text" name="mail" id="mail" />
    	{% validate id="mail" type={email} type={presence} %}
	</div>
	
	<div class="form-item">
	    <label for="message">{_ Message _}</label>
	    <textarea name="message" id="message" cols="60" rows="8"></textarea>
    	{% validate id="message" type={presence} %}
	</div>

    <button type="submit">{_ Send _}</button>

</form>

<div id="contact-form-sent" style="display: none">
    <h2>{_ Thank you! _}</h2>
    <p>{_ Your message has been submitted! We’ll get in touch soon. _}</p>
</div>

{% endblock %}