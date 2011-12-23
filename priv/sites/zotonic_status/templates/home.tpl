{% extends "base.tpl" %}

{% block title %}{_ Sites _}{% endblock %}

{% block content %}
<article id="content" class="zp-67">
	<div class="padding">
		<h1>{_ Sites on this Zotonic server _}</h1>
	
		<table id="sites">
			{% include "_sites.tpl" %}
		</table>
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


