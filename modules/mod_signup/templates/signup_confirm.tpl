{% extends "base.tpl" %}

{% block title %}Confirm my account{% endblock %}

{% block content %}

{% if user_id %}
	<h1>Welcome {{ m.rsc[user_id].title }}</h1>
	
	<p>Your account is confirmed. You can now continue on our site.</p>
	
	<p><a href="{{ m.rsc[user_id].page_url }}">Bring me to my profile page</a>.</p>

{% else %}
	<h1>Confirm my account</h1>

	<p>In your e-mail you received a confirmation key. Please copy it in the input field below.</p>

	<p id="confirm_error" class="error" {% if not error %}style="display: none"{% endif %}>
		Sorry, I don't know that confirmation code. Did you copy it correctly?
	</p>

	<form id="signup_confirm_form" method="post" action="postback">
		
		<p id="confirm_key">
			<label for="key">Confirm key</label>
			<input type="text" id="key" name="key" value="{{ q.key|escape }}" />
		</p>
		
		<button>Confirm my account</button>
	</form>
{% endif %}
		
{% endblock %}
