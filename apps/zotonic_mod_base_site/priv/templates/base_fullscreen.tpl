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
			"css/z.icons.css"
	        "css/site.css"
	%}
	{% block html_head_extra %}{% endblock %}
	<style type="text/css">
	{% block head_css_extra %}
		.container-fluid,
		.row-fluid {
			height: inherit;
			padding: 0;
		}
		.navbar-fixed-top {
			position: absolute !important;
		}
		.navbar .container-fluid {
			padding: 0 20px;
		}
		body.fullscreen {
			height: 100%;
			width: 100%;
			position: absolute;
			padding: 0 !important;
		}
	{% endblock %}
	</style>
</head>

<body class="{% block page_class %}{% endblock %} fullscreen">
<div class="container" {% include "_language_attrs.tpl" language=z_language %}>
	{% block navbar %}
		{% include "_navbar.tpl" %}
	{% endblock %}
	<div class="row">
	{% block content %}
	{% endblock %}
	</div>
</div>

{% include "_js_include.tpl" %}
{% script %}
</body>
</html>
