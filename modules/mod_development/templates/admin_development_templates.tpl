{% extends "admin_base.tpl" %}

{% block title %} Development {% endblock %}

{% block content %}
<div id="content" class="zp-85">
	<div class="block clearfix">
		<h2>Site Development &mdash; Included Templates</h2>
		
		<p>Below you see in real time which templates are compiled and included.</p>
		
		{% button text="Recompile Templates" action={admin_tasks task="templates_reset"} %}
		{% button text="Empty Log" action={update target="dev_templates" text=""} %}
		
		<pre id="dev_templates" class="clearfix" style="clear:left">
		</pre>

		<div>&nbsp;</div>
		
		{% wire action={development_templates_stream target="dev_templates"} %}

	</div>
</div>
{% endblock %}
