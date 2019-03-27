<!DOCTYPE html>
{# Base TABLET/DESKTOP template (two columns) #}
<html lang="{{ z_language|default:"en"|escape }}">
<head>
	<meta charset="utf-8" />
	<title>{% block title %}{{ id.title }}{% endblock %} &mdash; {{ m.site.title }}</title>

	<link rel="icon" href="/favicon.ico" type="image/x-icon" />
	<link rel="shortcut icon" href="/favicon.ico" type="image/x-icon" />

	<meta name="viewport" content="width=device-width, initial-scale=1.0" />
	{% if id.o.author as author %}
		<meta name="author" content="{% include '_name.tpl' id=author[1] %}" />
	{% endif %}

	{% all include "_html_head.tpl" %}
	{% lib
	        "bootstrap/css/bootstrap.css"
	        "bootstrap/css/bootstrap-base-site.css"
	        "css/jquery.loadmask.css"
	        "css/z.icons.css"
	        "css/z.growl.css"
	        "css/z.modal.css"
	        "css/z.icons.css"
	        "css/site.css"
	%}
	{% block html_head_extra %}{% endblock %}
</head>

<body class="{% block page_class %}{% endblock %}" data-cotonic-pathname-search="{% cotonic_pathname_search %}">
{% block navbar %}
	{% include "_navbar.tpl" %}
{% endblock %}
<div class="container">
	{% block content_area %}
		<div class="content" {% include "_language_attrs.tpl" language=z_language %}>
		{% block content %}
			{% block above %}
				<div class="page-title">
					<div class="{% if z_language|is_rtl %}col-lg-8 col-md-8{% endif %}">
						{% include "_breadcrumb.tpl" %}
						{% include "_title.tpl" %}
					</div>
				</div>
			{% endblock %}
			<div class="row">
				<div class="main col-lg-8 col-md-8">
					{% block main %}{% endblock %}
				</div>

				<div id="subnavbar" class="col-lg-4 col-md-4">
					{% block subnavbar %}
                        {% include "_subnav.tpl" %}
					{% endblock %}
				</div>
			</div>
			{% block below %}{% endblock %}
		{% endblock %}
		</div>
	{% endblock %}
	{% include "_footer.tpl" %}
</div>

{% include "_js_include.tpl" %}
{% script %}

</body>
</html>
