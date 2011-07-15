{% extends "admin_base.tpl" %}

{% block title %}{_ Log _}{% endblock %}

{% block content %}

	<div id="content" class="zp-85">
		<div class="block clearfix">
		
		<h2>{% block title_log %}{_ Log messages _}{% endblock %}</h2>

		{% button text=_"message log" action={redirect dispatch="admin_log"} %}
		{% button text=_"email log" action={redirect dispatch="admin_log_email"} %}
		
		<hr class="clear">
		
		{% block content_log %}
		{% endblock %}
		</div>
	</div>

{% endblock %}
