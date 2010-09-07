{% extends "admin_base.tpl" %}

{% block title %} Development {% endblock %}

{% block content %}
<div id="content" class="zp-85">
	<div class="block clearfix">
		<h2>Site Development</h2>
		
		<p>Tools and settings that are useful for site development.</p>
		
		{% wire id="libsep" 
				action={config_toggle module="mod_development" key="libsep"}
				action={admin_tasks task='flush'} 
		%}
		<label style="width: auto">
			<input type="checkbox" id="libsep" value="1" {% if m.config.mod_development.libsep.value %}checked="checked"{% endif %} />
			Download css and javascript files as separate files (ie. don’t combine them in one url).
		</label>
		
	</div>
</div>
{% endblock %}
