<!DOCTYPE html>
<html lang="{{ z_language|default:"en"|escape }}">
    <head>
        <meta charset="utf-8" />
	<title>{% block title %}{% endblock %} &mdash; {{ m.config.site.title.value }}</title>

	<!--
		Website built by:
		YOUR NAME HERE

		Proudly powered by: Zotonic, the Erlang CMS <http://www.zotonic.com>
	-->

        <meta name="viewport" content="width=device-width, initial-scale=1.0" />
	<meta name="author" content="YOUR NAME HERE &copy; 2009" />

	{% all include "_html_head.tpl" %}

	{% lib
        "bootstrap/css/bootstrap.min.css"
        "bootstrap/css/bootstrap.responsive.min.css"
        "css/jquery.loadmask.css"
        "css/zp-menu.css"
        "css/project.css"
        %}

	{% block html_head_extra %}{% endblock %}
    </head>
    <body class="{% block page_class %}page{% endblock %}">

        <div class="navbar navbar-fixed-top">

            <div class="navbar-inner">
                <div class="container">
        	    <a class="brand" href="/">{{ m.config.site.title.value }} {% if m.config.site.subtitle.value %}{% endif %}</a>
                    {# <span>{{ m.config.site.subtitle.value }}</span> #}
                    
                    {% menu id=id %}
                </div>
            </div>
        </div><!-- end navbar -->
        
        <div class="container">

            <div class="row">
	        {% block content_area %}
	        {% block chapeau %}{% endblock %}

                <div class="span8">
                    {% block content %}
		    <!-- The default content goes here. -->
                    {% endblock %}
                </div>

                <div id="sidebar" class="span4">
                    {% block sidebar %}
		    {% include "_sidebar.tpl" %}
    	            {% endblock %}
                </div>

		{% endblock %}

            </div>

            <div class="row">
                <div class="span12" id="footer">
		    <div class="pull-right">
		        <p class="footer-blog-title">{_ Website powered by _} <a href="http://zotonic.com">Zotonic</a> {{ m.config.zotonic.version.value }}.</p>
                    </div>
		    {% menu id=id menu_id='footer_menu' %}
                </div>
            </div>
        </div>
        
	{% include "_js_include_jquery.tpl" %}
        {% lib
        "bootstrap/js/bootstrap.min.js"
	"js/apps/zotonic-1.0.js" 
	"js/apps/z.widgetmanager.js" 
	"js/modules/livevalidation-1.3.js" 
	"js/modules/z.inputoverlay.js"
	"js/modules/jquery.loadmask.js"
        "js/z.superfish.js" 
	%}
	
	{% block _js_include_extra %}{% endblock %}

        <script type="text/javascript">
            $(function() { $.widgetManager(); });
        </script>

	{% stream %}
        {% script %}

        {% all include "_html_body.tpl" %}
    </body>
</html>
