<html>
	<head>
		<title>{% block title %}A mail message from Zotonic{% endblock %}</title>
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
