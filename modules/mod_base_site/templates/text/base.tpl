<!DOCTYPE html>
{# Base TEXT template #}
<html lang="{{ z_language|default:"en"|escape }}">
<head>
	<meta charset="utf-8" />
	<title>{% block title %}{{ id.title }}{% endblock %} &mdash; {{ m.config.site.title.value }}</title>

	<link rel="icon" href="/favicon.ico" type="image/x-icon" />
	<link rel="shortcut icon" href="/favicon.ico" type="image/x-icon" />

	<meta name="viewport" content="width=device-width, initial-scale=1.0" />
	<meta name="author" content="Marc Worrell" />

	{% all include "_html_head.tpl" %}
	{% lib "bootstrap/css/bootstrap-text.css" %}
	{% block html_head_extra %}{% endblock %}
</head>

<body class="{% block page_class %}{% endblock %}">
<div id="container">
    {% block navbar %}
    	{% include "_navbar.tpl" %}
    {% endblock %}
	{% block content_area %}
		<div class="content" {% include "_language_attrs.tpl" language=z_language %}>
		{% block content %}{% endblock %}
		</div>
	{% endblock %}
	{% include "_footer.tpl" %}
</div>
{% block ua_probe %}
	{% include "_ua_probe.tpl"%}
{% endblock %}
</body>
</html>
