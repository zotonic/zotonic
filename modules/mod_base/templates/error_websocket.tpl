{% extends "base.tpl" %}

{% block title %} WebSocket Error {% endblock %}

{% block content %}
	<div id="content-area">
		<h2>WebSocket Error</h2>
		<div class="block">
			<h3>Dear visitor. You stumbled upon one of our error pages</h3>
			<p>
				You tried to access the <a href="http://en.wikipedia.org/wiki/WebSockets">WebSocket</a> access point. But without a valid <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.42">upgrade header</a>.  Maybe your proxy stripped the upgrade header.
			</p>
			
			<p>
				You can’t do anything else here, just proceed to the <a href="/">home page</a>.
			</p>
		</div>
	</div>
{% endblock %}

{% block sidebar %}{% endblock %}