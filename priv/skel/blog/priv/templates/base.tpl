<!DOCTYPE html>
<html lang="{{ z_language|default:"en"|escape }}">
	<head>
		<meta charset="utf-8" />
		<title>{% block title %}{% endblock %} &mdash; {{ m.site.title }}</title>

		<!--
			Website built by:
			YOUR NAME HERE

			Proudly powered by: Zotonic, the Erlang CMS <http://www.zotonic.com>
		-->

		<meta name="viewport" content="width=device-width, initial-scale=1.0" />
		<meta name="author" content="YOUR NAME HERE &copy; 2014" />

		{% all include "_html_head.tpl" %}

		{% lib
			"bootstrap/css/bootstrap.min.css"
			"bootstrap/css/bootstrap-theme.min.css"
			"css/jquery.loadmask.css"
			"css/z-menu.css"
			"css/project.css"
		%}

		{% block html_head_extra %}{% endblock %}
	</head>
	<body class="{% block page_class %}page{% endblock %}">

		<div class="navbar navbar-inverse navbar-fixed-top">

			<div class="container">
				<div class="navbar-header">
				    <a class="navbar-brand" href="/">{{ m.site.title|default:"Your Zotonic Site" }} {% if m.site.subtitle %}{% endif %}</a>
					{# <span>{{ m.site.subtitle }}</span> #}
				</div>
				{% menu id=id %}
			</div>
		</div>
		<!-- end navbar -->

		<div class="container">

			<div class="row">
				{% block content_area %}
					{% block chapeau %}{% endblock %}

					<div class="col-lg-8 col-md-8">
						{% block content %}
							<!-- The default content goes here. -->
						{% endblock %}
					</div>

					<div id="sidebar" class="col-lg-4 col-md-4">
						{% block sidebar %}
							{% include "_sidebar.tpl" %}
						{% endblock %}
					</div>

				{% endblock %}

			</div>

			<div class="row">
				<div class="col-lg-12 col-md-12 clearfix" id="footer">
					<div class="pull-right">
						<p class="footer-blog-title">{% include "_powered_by_zotonic.tpl" %}</p>
					</div>
					{% menu id=id menu_id='footer_menu' %}
				</div>
			</div>
		</div>

		{% include "_js_include_jquery.tpl" %}
		{% lib
	        "js/modules/jstz.min.js"
		    "cotonic/zotonic-wired-bundle.js"
			"bootstrap/js/bootstrap.min.js"
			"js/apps/zotonic-wired.js"
			"js/apps/z.widgetmanager.js"
			"js/modules/livevalidation-1.3.js"
			"js/modules/z.dialog.js"
			"js/modules/jquery.loadmask.js"
		%}

		{% block _js_include_extra %}{% endblock %}

		<script type="text/javascript">
			$(function() { $.widgetManager(); });
		</script>

		{% script %}

		{% worker name="auth" src="js/zotonic.auth.worker.js" %}

		{% all include "_html_body.tpl" %}
	</body>
</html>
