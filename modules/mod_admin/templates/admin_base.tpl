<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en">
<head>
	<meta http-equiv="Content-type" content="text/html; charset=utf-8" />
	<title>{% block title %}{_ Admin _}{% endblock %} &mdash; {{ m.config.site.title.value|default:"Zotonic" }} Admin</title>

	<meta name="author" content="Tim Benniks" />
	
	{% lib 
		"css/zp-compressed.css"
		"css/zp-admin.css"
		"css/zp-wysiwyg.css"
		"css/zp-dialog.css"
		"css/zp-formreplace.css"
		"css/zp-growl.css"
		"css/zp-datepicker.css"
		"css/zp-icon-status.css"
		"css/zp-menuedit.css"
		"css/jquery.loadmask.css"
	%}

	{% include "_js_include_jquery.tpl" %}
	<!--[if IE]>
		{% lib
			"css/zp-ie.css"
		%}
	<![endif]-->
    {% block head_extra %}
    {% endblock %}
</head>
<body class="zp-wide">
	<div class="skip">
		<a href="#content" title="Go directly to page content">Go to page content</a>
	</div>
	<div class="zp-wrapper">
		<div id="header" class="clearfix">
			{% include "_admin_header_title.tpl" %}

			{% block search %}
			<div class="right">
                {% all include "_admin_headeritem.tpl" %}
				<form class="headeritem" action="{% url admin_overview_rsc %}" method="get">
                    <input type="hidden" name="qsort" value="{{ q.qsort|escape }}" />
                    <input type="hidden" name="qcat" value="{{ q.qcat|escape }}" />
					<div class="search-wrapper">
						<input type="text" name="qs" value="{{q.qs|escape}}">
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
		
			<ul id="navigation" class="zp-10" style="margin: 0 10px 0 0;">
				<li><a href="/admin/" {% ifequal selected "dashboard" %}class="current"{% endifequal %}>{_ Dashboard _}</a></li>
				<li><a href="/admin/overview/" {% ifequal selected "overview" %}class="current"{% endifequal %}>{_ Pages _}</a></li>
				<li><a href="{% url admin_media %}" {% ifequal selected "media" %}class="current"{% endifequal %}>{_ Media _}</a></li>
			
				{% all include "_admin_menu_module.tpl" %}

				{% if m.acl.is_admin %}
					<li><a href="{% url admin_status %}" {% ifequal selected "status" %}class="current"{% endifequal %}>{_ System _}</a></li>
				{% endif %}
				<li>{% include "_logon_off.tpl" %}</li>
			</ul>
		
		{% endblock %}

		{% block content %}{% endblock %}
	</div>

	{% include "_admin_js_include.tpl" %}
	{% block js_extra %}{% endblock %}
	
	{% stream %}
	{% script %}

	{% block tinymce %}{% endblock %}
	
	{% block html_body_admin %}{% all include "_html_body_admin.tpl" %}{% endblock %}
	
</body>
</html>
