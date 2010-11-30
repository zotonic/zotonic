<html>
	<head>
		<meta http-equiv="Content-type" content="text/html; charset=utf-8" />
		<title>{% block title %}A mail message from Zotonic{% endblock %}</title>
		<base href="http://{{ m.site.hostname }}/" />
		<base target="_blank" />
	</head>

	<body>
		{% block body %}
		<p>Lectori Salutem,</p>

		<p>
			This is the base message.  When you receive this text then the template builder did not 
			overrule the <em>body</em> block.
		</p>
		
		<p>Kind Regards, Zotonic.</p>
		
		{% endblock %}
	</body>
</html>
