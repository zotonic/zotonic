<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en">
<head>
	<meta http-equiv="Content-type" content="text/html; charset=utf-8" />
	<title>Zotonic {% block title %}Admin{% endblock %}</title>

	<meta name="author" content="Tim Benniks" />
	
	{% lib 
		"css/zp-compressed.css"
		"css/zp-admin.css"
		"css/zp-wysiwyg.css"
		"css/zp-dialog.css"
		"css/zp-formreplace.css"
		"css/zp-growl.css"
		"css/zp-datepicker.css"
	%}
	
	<!--[if IE]>
		{% lib
			"css/zp-ie.css"
		%}
	<![endif]-->
</head>
<body class="zp-wide">
	<div class="skip">
		<a href="#content" title="Go directly to page content">Go to page content</a>
	</div>
	<div class="zp-wrapper">
		<div id="header" class="clearfix">
			<h1 class="zotonic left">
				<a href="/admin/">
					<img src="/lib/images/admin_zotonic.png">
				</a>
				<span><a href="http://{{ m.site.hostname }}" title="Visit your site">visit site &raquo;</a></span>
			</h1>
			
			{% block search %}
			<div class="right search">
				<form action="/admin/overview" method="get">
					<div class="search-wrapper">
						<input type="text" name="qs" value="{{q.qs}}">
					</div>
				</form>
			</div>
			{% endblock %}
		</div>

		<!--[if lte IE 6]>
		<div id="ie6-upgrade" class="notification notice">
			<h4>Warning</h4>
			Your version of Internet Explorer is extremely <strong>out of date</strong> and has known <strong>security issues!</strong><br />
			To have the best experience using the Zotonic admin and to protect your computer correct this by installing <a href="http://www.opera.com">Opera</a>, <a href="http://mozilla.com">FireFox</a>, <a href="http://www.apple.com/safari/download/">Safari</a> or a higher version <a href="http://www.microsoft.com/windows/downloads/ie/getitnow.mspx">Internet Explorer</a>.
		</div>
		<![endif]-->

		{% block navigation %}
		
			{% wire id="zp-logoff" action={logoff} %}
		
			<ul id="navigation" class="zp-10" style="margin: 0 10px 0 0;">
				<li><a href="/admin/" {% if page_dashboard %}class="current"{% endif %}>Dashboard</a></li>
				<li><a href="/admin/overview/" {% if page_overview %}class="current"{% endif %}>Pages</a></li>
				<li><a href="{% url admin_media %}" {% if page_media %}class="current"{% endif %}>Media</a></li>
			
				{% all include "_admin_menu_module.tpl" %}

				<li><a id="zp-logoff" href="#">Logoff</a></li>
			</ul>
		
		{% endblock %}

		{% block content %}{% endblock %}
	</div>

	{% include "_admin_js_include.tpl" %}
	{% block js_extra %}{% endblock %}
	
	{% script %}

	{% block tinymce %}{% endblock %}
	
</body>
</html>
