{% extends "base.tpl" %}

{% block title %} {{ error_code }} Error {% endblock %}

{% block content %}
	<div id="content-area">
{% if error_code == 400 %}
		<h2>{_ Bad Request _}</h2>

		<div class="block">
			<p>{_ Something is malformed in your request. Correct it and try again. _}</p>
		</div>
{% else %}
{% if error_code == 403 %}
		<h2>{_ No Access _}</h2>

		<div class="block">
			<p>{_ Sorry, you don’t have access to this page. _}</p>
		</div>
{% else %}
		{% if error_code == 404 %}
		<h2>{_ Not Found _}</h2>
		{% else %}
		<h2>{{ error_code }} {_ error _}</h2>
		{% endif %}
		<div class="block">
			<h3>{_ Dear visitor. You stumbled upon one of our error pages _}</h3>
			
			{% if error_code == 404 %}
			<p>
				{_ At first we would like to apologize for the fact that you got here. _}
				{_ It seems you where looking for a page that does not exist in this system. It could have been moved or deleted. _} 
				{_ Please use our search to find anything you like. Or go the the <a href="/" title="home">homepage</a>. _}
			</p>
			{% else %}
			<p>
				{_ The system had to handle something it couldn't handle. A mail is now send to the system administrator. _}
			</p>
			{% endif %}
			
			<div class="notification error">
				{% wire id="error-trigger" action={slide_toggle speed=350 target="error-explain"} %} 
				<a href="javascript:void(0);" id="error-trigger">{_ Click for error information. _}</a>
				<pre style="display: none;" id="error-explain">{{ error_dump }}</pre>
			</div>
		</div>
{% endif %}
{% endif %}
	</div>
{% endblock %}

{% block sidebar %}{% endblock %}