<html>
<head>
	<meta http-equiv="Content-type" content="text/html; charset=utf-8" />
	<title>{% block title_wrapper %}{% block title %}{_ Hello from Zotonic _}{% endblock %}{% endblock %}</title>
	<base href="http://{{ m.site.hostname }}/" />
	<base target="_blank" />
	{% block email_styles %}{% include "_email_styles.tpl" %}{% endblock %}
</head>
<body>
{% block body_all %}
<div align="center">
	<table width="{% block body_width %}635{% endblock %}" id="email-table" border="0" cellspacing="0" cellpadding="0">
		{% block header %}{% endblock %}
		{% block content %}
		<tr>
			<td id="content">
			    {% block body %}
				<h1>Lectori Salutem,</h1>
				<p>{_ This is the base message. When you receive this text then the template builder did not overrule the <tt>body</tt> block. _}</p>
				{% endblock %}
				{% block closing %}
					<p>{_ Kind regards, _}</p>
					<p><a href="http://{{ m.site.hostname }}/">{{ m.config.site.title.value }}</a></p>
				{% endblock %}
				{% block disclaimer %}
				{% endblock %}
			</td>
		</tr>
		{% endblock %}
		{% block footer %}{% endblock %}
	</table>
</div>
{% endblock %}
</body>
</html>
