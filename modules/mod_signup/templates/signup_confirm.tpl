{% extends "base.tpl" %}

{% block title %}{_ Confirm my account _}{% endblock %}

{% block content %}

<h1>{_ Confirm my account _}</h1>

<div id="confirm_form">
	<div id="confirm_error" class="error" style="display: none">
		<p>
			{_ Sorry, this confirmation key is unknown or already used. Did you copy it correctly? _}<br>
			{_ Your account might have been confirmed already. _} <a href="{% url logon %}">{_ Try to log on _}</a>
		</p>
	</div>

	<form class="setcookie" id="signup_confirm_form" method="post" action="postback">
		<div class="form-group" id="confirm_key" {% if q.key %}style="display: none"{% endif %}>
			<p>{_ In your e-mail you received a confirmation key. Please copy it in the input field below. _}</p>

			<label for="key">{_ Confirm key _}</label>
			<input class="form-control" type="text" id="key" name="key" value="{{ q.key|escape }}" />
		</div>
		<button class="btn btn-primary" type="submit">{_ Confirm my account _}</button>
	</form>
</div>

<div id="confirm_ok" style="display: none">
	<p>{_ Your account is confirmed and you are logged on. _}</p>

	<p><a href="{{ location|default:m.rsc[user_id].page_url }}">{_ Bring me to my profile page _}</a>.</p>
</div>

{% endblock %}
