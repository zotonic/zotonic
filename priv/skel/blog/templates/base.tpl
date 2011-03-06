<!DOCTYPE html>
<html lang="en">
<head>
	<title>{% block title %}{% endblock %} &mdash; {{ m.config.site.title.value }}</title>

	<!--
		Website built by:
		YOUR NAME HERE

		Proudly powered by: Zotonic, the Erlang CMS <http://www.zotonic.com>
	-->

	<meta http-equiv="Content-type" content="text/html;charset=UTF-8" />
	<meta name="author" content="YOUR NAME HERE &copy; 2009" />

	{% all include "_html_head.tpl" %}

	{% lib "css/zp-compressed.css" "css/zp-project.css" "css/zp-menu.css" "css/jquery.loadmask.css"  %}
	<!--[if IE]>{% lib "css/zp-ie.css" %}<![endif]-->
	{% lib "js/apps/modernizr.js" %}
	{% block html_head_extra %}{% endblock %}
</head>
<body class="{% block page_class %}page{% endblock %}">

	<section class="skip">
		<a href="#content-area" title="Go directly to page content">Go to page content</a>
	</section>

    <section class="zp-wrapper">

        <header class="clearfix">
        	<h1 class="left"><a href="/">{{ m.config.site.title.value }} {% if m.config.site.subtitle.value %}<span>{{ m.config.site.subtitle.value }}</span>{% endif %}</a></h1>
        </header>
        <nav class="clearfix">
            {% menu id=id %}
        </nav>

        <section id="content-area" class="clearfix">

			{% block content_area %}
			{% block chapeau %}{% endblock %}

            <article id="content" class="zp-65">
                <div class="padding">
                    {% block content %}
						<!-- The default content goes here. -->
                    {% endblock %}
                </div>
            </article>

            <section id="sidebar" class="zp-35 last">
                {% block sidebar %}
					{% include "_sidebar.tpl" %}
    	        {% endblock %}
            </section>

			{% endblock %}

        </section>

        <footer class="clearfix">
			<nav class="left">{% menu id=id menu_id='footer_menu' %}</nav>
			<section class="right">
				<p class="footer-blog-title">{_ Website powered by _} <a href="http://zotonic.com">Zotonic</a> {{ m.config.zotonic.version.value }}.</p>
			</section>
		</footer>

    </section>

	{% include "_js_include_jquery.tpl" %}
    {% lib 
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
