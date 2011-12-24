{% extends "base.tpl" %}

{% block title %}{_ Nodes _}{% endblock %}

{% block content %}
<article id="content" class="zp-67">
	<div class="padding">
		<h1>{_ Zotonic nodes &amp; services _}</h1>
	        
                {% nodes_status %}
	</div>
</article>
{% endblock %}



{% block sidebar %}
<aside id="sidebar" class="zp-33">
	<div>
		{% all include "_z_system_button.tpl" %}
                {% all include "_z_trace_button.tpl" %}
	</div>
	<div style="clear:left" id="notices"></div>
</aside>
{% endblock %}


