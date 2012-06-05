<!DOCTYPE html>
{# Base PHONE/TABLET/DESKTOP template #}
<html lang="{{ z_language|default:"en"|escape }}">
<head>
	<meta charset="utf-8" />
	<title>{% block title %}{{ id.title }}{% endblock %} &mdash; {{ m.config.site.title.value }}</title>

	<link rel="icon" href="/favicon.ico" type="image/x-icon" />
	<link rel="shortcut icon" href="/favicon.ico" type="image/x-icon" />

	<meta name="viewport" content="width=device-width, initial-scale=1.0" />
	<meta name="author" content="Marc Worrell" />

	{% all include "_html_head.tpl" %}
	{% lib 
			"bootstrap/css/bootstrap.css" 
			"bootstrap/css/bootstrap-responsive.css" 
			"bootstrap/css/bootstrap-base-site.css" 
	%}
	{% block html_head_extra %}{% endblock %}
	<style type="text/css">
	body.fullscreen {
		height: 100%;
		width: 100%;
		position: absolute;
		padding: 0 !important;
	}
	.container-fluid,
	.row-fluid { 
		height: inherit; 
		padding: 0;
	}
	.navbar-fixed-top {
		position: absolute !important;
	}
	</style>
</head>

<body class="{% block page_class %}{% endblock %} fullscreen">
<div class="container-fluid" {% include "_language_attrs.tpl" language=z_language %}>
	{% block navbar %}
		{% include "_navbar.tpl" %}
	{% endblock %}
	<div class="row-fluid">
	{% block content %}
	{% endblock %}
	</div>
</div>

{% include "_js_include.tpl" %}
{% script %}

{% block ua_probe %}
	{% include "_ua_probe.tpl"%}
{% endblock %}
</body>
</html>
