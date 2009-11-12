{% extends "base.tpl" %}

{% block title %}Welcome{% endblock %}

{% block content %}
	<h1>Welcome to Zotonic</h1>
	
	<p>Thank you for choosing Zotonic, and congratulations with your successful install.</p>
	<p>You see this page because you didn't enable a site module or the site module doesn't contain the template <tt>home.tpl</tt>.</p>
	
	<p>Go to the <a href="/admin/modules">Admin module pages</a> to enable the site module.</p>
{% endblock %}
