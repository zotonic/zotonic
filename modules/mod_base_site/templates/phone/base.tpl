<!DOCTYPE html>
{# Base PHONE template #}
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
	        "bootstrap/css/bootstrap-base-site.css" 
	        "css/jquery.loadmask.css" 
	        "css/z.growl.css" 
	        "css/z.modal.css" 
	        "css/z.icons.css" 
	        "css/site.css" 
	%}
	{% block html_head_extra %}{% endblock %}
</head>

<body class="{% block page_class %}{% endblock %}">
{% block navbar %}
	{% include "_navbar.tpl" %}
{% endblock %}
<div class="container">
	{% block content_area %}
		<div class="content" {% include "_language_attrs.tpl" language=z_language %}>
		    {% block content %}
 		    <div class="row">
		    	<div class="main">
                    		{% block main %}{% endblock %}
		    	</div>
		    	<div class="subnavbar">
                    		{% block subnavbar %}
                        		{% include "_subnav.tpl" %}
                    		{% endblock %}
                    	</div>
	            </div>
                    {% endblock %}
		    {% block below %}{% endblock %}
		</div>
	{% endblock %}
	{% include "_footer.tpl" %}
</div>

{% include "_js_include.tpl" %}
{% script %}

{% block ua_probe %}
	{% include "_ua_probe.tpl"%}
{% endblock %}
</body>
</html>
