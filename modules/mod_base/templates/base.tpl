<!DOCTYPE html>
<html lang="{{ z_language|default:"en"|escape }}">
    <head>
        <meta charset="utf-8" />
	<title>{% block title %}{% endblock %} &mdash; {{ m.config.site.title.value }}</title>

	<link rel="icon" href="/favicon.ico" type="image/x-icon" />
	<link rel="shortcut icon" href="/favicon.ico" type="image/x-icon" />
	
        <meta name="viewport" content="width=device-width, initial-scale=1.0" />
	<meta name="author" content="Arjan Scherpenisse" />

	{% all include "_html_head.tpl" %}

	{% lib
	"bootstrap/css/bootstrap.min.css"
	"css/z-menu.css"
	"css/jquery.loadmask.css" 
	"css/project.css"
	%}

	{% block html_head_extra %}{% endblock %}
    </head>

    <body class="{% block page_class %}{% endblock %}">

        <div class="container">
            <div class="navbar">
                <div class="navbar-inner">
                    <div class="container">
        	        <a class="brand" href="/">{{ m.config.site.title.value }} {% if m.config.site.subtitle.value %}{% endif %}</a>
                        
                        <div class="pull-right">
                            {% menu id=id %}
                        </div>
                    </div>
                </div>
            </div><!-- end navbar -->

            <div class="row">
                <div class="span12" id="content-area">
		    {% block content_area %}
		    {% block content %}{% endblock %}
		    {% block sidebar %}{% endblock %}
		    {% endblock %}
                </div>
            </div>
	    
            <div class="row">
                <div class="span12" id="footer">
		    {% include "_footer.tpl" %}
                </div>
            </div>

        </div><!-- end container -->
        
	{% include "_js_include.tpl" %}

	{% script %}
	
	{% block ua_probe %}
		{% include "_ua_probe.tpl"%}
	{% endblock %}
</body>
</html>
