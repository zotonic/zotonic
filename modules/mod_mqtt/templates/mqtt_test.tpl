{% extends "base.tpl" %}	

{% block title %}MQTT Test{% endblock %}

{% block content %}
	<h1>Hello</h1>

	{% javascript %}
	pubzub.subscribe("/test", function (topic, msg) { console.log(topic, msg); });
	pubzub.subscribe("/growl", function (topic, msg) { z_growl_add(msg.payload); });
	{% endjavascript %}

{% endblock %}

{% block _js_include_extra %}
	{% lib
		 "js/ubf.js"
		 "js/qlobber.js"
		 "js/pubzub.js"
	%}
{% endblock %}
