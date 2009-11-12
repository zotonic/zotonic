{% extends "admin_base.tpl" %}

{% block title %} modules {% endblock %}

{% block content %}
	<div id="content" class="zp-85">
		<div class="block clearfix">

		<h2>Zotonic Modules</h2>

		{% button text="Rescan modules" action={module_rescan} %}

		<hr class="clear" />
		<p>
			Rescanning will rebuild the index of all modules, actions, templates etc.  It will also reload all dispatch rules.
		</p>

		<h3 class="above-list ">Modules overview</h3>
		<ul class="short-list">
			<li class="headers clearfix">
				<span class="zp-20">Title</span>
				<span class="zp-45">Description</span>
				<span class="zp-5">Prio</span>
				<span class="zp-20">Author</span>
				<span class="zp-10">Activate</span>
			</li>
		{% for prio, module, props in modules %}
			<li id="{{ #li.module }}" {% if not props.active %}class="unpublished" {% endif %}>
				<a href="#" class="clearfix">
					<span class="zp-20">{{ props.mod_title|default:props.title }}</span>
					<span class="zp-45">{{ props.mod_description|default:"-" }}</span>
					<span class="zp-5">{{ prio }}</span>
					<span class="zp-20">{{ props.author|escape|default:"-" }}</span>
					<span class="zp-10">
						{% if props.active %}
							{% button text="Deactivate" action={module_toggle module=module} action={toggle_class id=#li.module class="enabled"} %}
						{% else %}
							{% button text="Activate" action={module_toggle module=module} action={toggle_class id=#li.module class="enabled"} %}
						{% endif %}
					</span>
				</a>
			</li>
		{% empty %}
			<li>
				No items found
			</li>
		{% endfor %}
		</ul>

		</div>
	</div>
{% endblock %}