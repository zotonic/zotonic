<!DOCTYPE html>
<html lang="en">
<head>
	<meta http-equiv="Content-type" content="text/html; charset=utf-8" />
	<meta name="robots" content="noindex" />

	<title>{% block title %}Sites Dashboard{% endblock %} &mdash; Zotonic</title>

	<link rel="icon" href="/favicon.ico" type="image/x-icon" />
	<link rel="shortcut icon" href="/favicon.ico" type="image/x-icon" />
	
	{% lib
		"css/zp-compressed.css"
		"css/zp-project.css"
		"css/zp-growl.css"
		"css/jquery.loadmask.css" 
	%}

	<!--[if IE]>
	{% lib "css/zp-ie.css" %}
	<![endif]-->
	
	<!-- Make ie understand html5 -->
	{% lib "js/modernizr.js" %}

	{% block html_head_extra %}{% endblock %}
</head>

<body class="page">

	<section class="skip">
		<a href="#content-area" title="Go directly to page content">Go to page content</a>
	</section>

	<section class="zp-wrapper">
		<header class="clearfix">
			<figure id="logo" class="left">
				<a href="/" title="Home"><img src="/lib/images/logo.png" alt="Zotonic &mdash; Simple stuff that works" /></a>
			</figure>
			
			<nav class="right">
				<ul id="navigation" class="list">
					{% include "_menu.tpl" %}
				</ul>
			</nav>
		</header>

		{% block banner %}{% endblock %}

		<!--[if lte IE 6]>
		<div id="ie6-upgrade" class="notification notice">
			<h4>Warning</h4>
			Your version of Internet Explorer is extremely <strong>out of date</strong> and has known <strong>security issues!</strong><br />
			To have the best experience using this website and to protect your computer you can correct this by installing <a href="http://www.opera.com">Opera</a>, <a href="http://mozilla.com">FireFox</a>, <a href="http://www.apple.com/safari/download/">Safari</a> or a higher version <a href="http://www.microsoft.com/windows/downloads/ie/getitnow.mspx">Internet Explorer</a>.
		</div>
		<![endif]-->
		
		<section id="content-area" class="clear clearfix">
		{% block content_area %}
			{% block content %}{% endblock %}
			{% block sidebar %}{% endblock %}
		{% endblock %}
		</section>

		<div class="push"><!-- push down --></div>
	</section>
	
	<footer>
		{% include "_footer.tpl" %}
	</footer>

	{% include "_js_include.tpl" %}

</body>
</html>
