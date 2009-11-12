{% extends "base.tpl" %}

{% block title %} {{ error_code }} error {% endblock %}

{% block content %}
	<div id="content-area">
		<h2>{{ error_code }} error</h2>
		<div class="block">
			<h3>Dear website visitor. You stumbled upon one of our error pages</h3>
			<p>
				At first we would like to apologize
				for the fact that you got here. If the title of this page is 404 then you where looking for a page that does not exist
				in this system. It could have been moved or deleted. Please use our search to find anything you like. Or go the the <a href="/" title="home">homepage</a>.
			</p>
			<p>
				If the title of this page says 500, then the system had to handle 
				something it couldn't handle. A mail is now send to the system administrator.
			</p>
			<div class="notification error">
				<h5>{{ error_code }} error</h5>
				{% wire id="error-trigger" action={slide_toggle speed=350 target="error-explain"} %} 
				<a href="javascript:void(0);" id="error-trigger">Click for error information.</a>
				<pre style="display: none;" id="error-explain">{{ error_dump }}</pre>
			</div>
		</div>
	</div>
{% endblock %}

{% block sidebar %}{% endblock %}