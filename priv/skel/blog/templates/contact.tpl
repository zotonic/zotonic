{% extends "page.tpl" %}

{% block below_body %}

    {% wire id="contact-form" type="submit" postback={contact} delegate="controller_default_contact" %}
    <form id="contact-form" method="post" action="postback" class="form">

	    <div class="form-group row">
	        <label class="control-label col-md-3" for="name">Name</label>
		    <div class="col-md-9">
			    <input type="text" name="name" id="name" class="col-lg-4 col-md-4 form-control" />
		    </div>
	    </div>

	    <div class="form-group row">
	        <label class="control-label col-md-3" for="mail">E-mail</label>
		    <div class="col-md-9">
			    <input type="text" name="mail" id="mail" class="col-lg-4 col-md-4 form-control" />
			    {% validate id="mail" type={email} type={presence} %}
		    </div>
	    </div>

	    <div class="form-group row">
	        <label class="control-label col-md-3" for="message">Message</label>
		    <div class="col-md-9">
		        <textarea name="message" id="message" cols="60" rows="8" class="col-lg-4 col-md-4 form-control"></textarea>
		        {% validate id="message" type={presence} %}
	        </div>
        </div>
        
        <div class="form-group row">
	        <div class="col-md-9 col-md-offset-3">
		        <button class="btn btn-primary" type="submit">Send</button>
	        </div>
        </div>
        
    </form>

{% endblock %}
