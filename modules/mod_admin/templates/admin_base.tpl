<!DOCTYPE html>
<html lang="en">
    <head>
        <meta charset="utf-8" />
        <title>{% block title %}{_ Admin _}{% endblock %} &mdash; {{ m.config.site.title.value|default:"Zotonic" }} Admin</title>
        
        <meta name="viewport" content="width=device-width, initial-scale=1.0" />
        <meta name="description" content="" />
        <meta name="author" content="Arjan Scherpenisse" />

        {% lib
                "bootstrap/css/bootstrap.min.css"
                "css/zotonic-admin.css"
        %}


        {% include "_js_include_jquery.tpl" %}
        
        <!-- Le HTML5 shim, for IE6-8 support of HTML5 elements -->
        <!--[if lt IE 9]>
            <script src="http://html5shim.googlecode.com/svn/trunk/html5.js"></script>
        <![endif]-->

        {% block head_extra %}
        {% endblock %}
    </head>
    <body>

        {% block navigation %}
        {% include "_admin_menu.tpl" %}
        {% endblock %}


        <div class="container">
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
