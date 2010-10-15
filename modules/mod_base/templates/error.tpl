{% extends "base.tpl" %}

{% block title %} {{ error_code }} Error {% endblock %}

{% block content %}
	<div id="content-area">
{% if error_code == 403 %}
		<h2>No Access</h2>

		<div class="block">
			<p>Sorry, you don’t have access to this page.</p>
		</div>
{% else %}
		<h2>{{ error_code }} error</h2>
		<div class="block">
			<h3>Dear visitor. You stumbled upon one of our error pages</h3>
			
			{% if error_code == 404 %}
			<p>
				At first we would like to apologize
				for the fact that you got here. It seems you where looking for a page that does not exist
				in this system. It could have been moved or deleted. Please use our search to find anything you like. Or go the the <a href="/" title="home">homepage</a>.
			</p>
			{% else %}
			<p>
				The system had to handle something it couldn't handle. A mail is now send to the system administrator.
			</p>
			{% endif %}
			
			<div class="notification error">
				{% wire id="error-trigger" action={slide_toggle speed=350 target="error-explain"} %} 
				<a href="javascript:void(0);" id="error-trigger">Click for error information.</a>
				<pre style="display: none;" id="error-explain">{{ error_dump }}</pre>
			</div>
		</div>
{% endif %}
	</div>
{% endblock %}

{% block sidebar %}{% endblock %}