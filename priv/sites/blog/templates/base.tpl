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

	{% lib
		"css/zp-compressed.css"
		"css/zp-project.css"
	%}

	<!--[if IE]>
	{% lib	"css/zp-ie.css" %}
	<![endif]-->

</head>
<body class="home">

<section class="zp-wrapper">
	
	<header>
	
		<nav>
            {% menu id=id class="list" %}
		</nav>

        {% block featured %}
        {% endblock %}
	
	</header>
	
	<section id="content-area" class="clearfix">
			
		<section id="content" class="zp-80">
			<div class="padding">
                {% block content %}
                The default content goes here.
                {% endblock %}
			</div>
		</section>
		
		<sidebar class="zp-20 last">
			<div class="padding">
                {% block sidebar %}
                {% include "_sidebar.tpl" %}
                {% endblock %}
			</div>
		</sidebar>
	
	</section>

    <footer>
        Website powered by <a href="http://zotonic.com">Zotonic</a>, the Erlang CMS.
    </footer>

</section>


</body>
</html>
