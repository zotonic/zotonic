{% extends "admin_base.tpl" %}

{% block title %} Configuration {% endblock %}

{% block content %}

{% with q.qcat == "event" as is_event %}

	<div id="content" class="zp-85">
		<div class="block clearfix">

			<h2>System Configuration</h2>
			<div class="clearfix">
				{% button class="" text="make a new config setting" action={dialog_config_new on_success={reload}} %}
			</div>
			
			<hr />

			<h3 class="above-list ">Configurations overview</h3>
			<ul class="short-list">
				<li class="headers clearfix">
					<span class="zp-15">Module</span>
					<span class="zp-20">Key</span>
					<span class="zp-35">Value</span>
					<span class="zp-20">Modified on</span>
					<span class="zp-10">Options</span>
				</li>
			{% for module, keys in config %}
				{% for key, c in keys %}
					{% with c.id as id %}
						<li id="{{ #li.id }}">
							<a id="{{ #a.id }}" href="#edit-config" class="clearfix">
								<span class="zp-15">{{ module|escape|default:"-" }}</span>
								<span class="zp-20">{{ key|escape|default:"-" }}</span>
								<span class="zp-35">{{ c.value|escape|default:"-" }}</span>
								<span class="zp-20">{{ c.modified|date:"d M Y, H:i" }}</span>
								<span class="zp-10">
									{% button text="delete" disabled=(module=="zotonic") action={dialog_config_delete module=module key=key on_success={slide_fade_out target=#li.id}} %}

									{% button text="edit" disabled=(module=="zotonic") action={dialog_config_edit module=module key=key on_success={reload}} %}
								</span>
							</a>
							{% wire id=#a.id action={dialog_config_edit module=module key=key on_success={reload}} %}
						</li>
					{% endwith %}
				{% endfor %}
			{% empty %}
				<li>
					No configurations found.
				</li>
			{% endfor %}
			</ul>
		
		</div>
	</div>

{% endwith %}

{% endblock %}
