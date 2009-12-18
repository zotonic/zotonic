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
	{% include "_atom_feed_link.tpl" cat="blog" %}

	{% all include "_html_head.tpl" %}

	{% lib "css/zp-compressed.css" "css/zp-project.css" %}
	<!--[if IE]>{% lib "css/zp-ie.css" %}<![endif]-->
	
	{% lib "js/apps/modernizr.js" %}
	
</head>
<body class="{% block page_class %}page{% endblock %}">

<section class="zp-wrapper">
	
	<header>
		<nav>{% menu id=id %}</nav>
		{% block featured %}<div class="featured-empty"></div>{% endblock %}
	</header>
	
	<section id="content-area" class="clearfix">
	
		<article id="content" class="zp-75">
			<div class="padding">
				{% block content %}
					The default content goes here.
				{% endblock %}
			</div>
		</article>
		
		<aside class="zp-25 last">
			{% block sidebar %}
				{% include "_sidebar.tpl" %}
			{% endblock %}
		</aside>
	
	</section>

	<footer>
		Website powered by <a href="http://zotonic.com">Zotonic</a>, the Erlang CMS.
	</footer>

</section>

<script type="text/javascript" src="http://ajax.googleapis.com/ajax/libs/jquery/1.3.2/jquery.min.js"></script>

{% lib "js/apps/zotonic-1.0.js" "js/apps/z.widgetmanager.js" "js/modules/livevalidation-1.3.js" %}

<script type="text/javascript">
	$(function() { $.widgetManager(); });
</script>

{% script %}

</body>
</html>