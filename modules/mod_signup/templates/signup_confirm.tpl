{% extends "base.tpl" %}

{% block title %}{_ Confirm my account _}{% endblock %}

{% block content %}

{% if user_id %}
	<h1>{_ Welcome _} {{ m.rsc[user_id].title }}</h1>
	
	<p>{_ Your account is confirmed. You can now continue on our site. _}</p>
	
	<p><a href="{{ location|default:m.rsc[user_id].page_url }}">{_ Bring me to my profile page _}</a>.</p>

{% else %}
	<h1>{_ Confirm my account _}</h1>

	<p>{_ In your e-mail you received a confirmation key. Please copy it in the input field below. _}</p>

	<p id="confirm_error" class="error" {% if not error %}style="display: none"{% endif %}>
		{_ Sorry, I don't know that confirmation code. Did you copy it correctly? _}
	</p>

	<form class="setcookie" id="signup_confirm_form" method="post" action="postback">
		
		<p id="confirm_key">
			<label for="key">{_ Confirm key _}</label>
			<input type="text" id="key" name="key" value="{{ q.key|escape }}" />
		</p>
		
		<button>{_ Confirm my account _}</button>
	</form>
{% endif %}
		
{% endblock %}
