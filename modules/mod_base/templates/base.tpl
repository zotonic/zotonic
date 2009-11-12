<!DOCTYPE html>
<html lang="en">
<head>
	<meta http-equiv="Content-type" content="text/html; charset=utf-8" />
	<title>{% block title %}{{ m.rsc[id].seo_title | default: m.rsc[id].title }}{% endblock %} &mdash; Zotonic</title>

	<link rel="icon" href="/favicon.ico" type="image/x-icon" />
	<link rel="shortcut icon" href="/favicon.ico" type="image/x-icon" />
	
	<meta name="author" content="Tim Benniks" />
	{% include "_atom_feed_link.tpl" cat="blog" %}

	{% all include "_html_head.tpl" %}

	{% lib
		"css/zp-compressed.css"
		"css/zp-project.css"
	%}

	<!--[if IE]>
	{% lib	"css/zp-ie.css" %}
	<![endif]-->
	
	<!-- Make ie6 understand html5 -->
	<script>
		document.createElement('header');
	   	document.createElement('footer');
	   	document.createElement('section');
	   	document.createElement('aside');
	   	document.createElement('nav');
	   	document.createElement('article');
   	</script>

</head>

<body class="{% block page_class %}{% endblock %}">

	<section class="skip">
		<a href="#content-area" title="Go directly to page content">Go to page content</a>
	</section>

	<section class="zp-wrapper">
		<header class="clearfix">
			{#
			<figure id="logo" class="left">
				<a href="/" title="Home"><img src="/lib/images/logo.jpg" alt="Zotonic &mdash; Simple stuff that works" /></a>
			</figure>
			#}
			
			<nav class="right">
				{% menu id=id class="list" %}
			</nav>
		</header>

		{% block banner %}{% endblock %}
		
		<section id="content-area" class="clear clearfix">
			{% block content %}{% endblock %}
			{% block sidebar %}{% endblock %}
		</section>

		<div class="push"><!-- push down --></div>
	</section>
	
	<footer>
		{% include "_footer.tpl" %}
	</footer>

	{% include "_js_include.tpl" %}

	{% script %}

</body>
</html>
