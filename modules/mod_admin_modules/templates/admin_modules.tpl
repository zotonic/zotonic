{% extends "admin_base.tpl" %}

{% block title %}{_ Modules _}{% endblock %}

{% block content %}
	<div id="content" class="zp-85">
		<div class="block clearfix">

		<h2>{_ Modules _}</h2>

		{% button text="Rescan modules" action={module_rescan} %}

		<hr class="clear" />
		<p>{_ Rescanning will rebuild the index of all modules, actions, templates etc.  It will also reload all dispatch rules. _}</p>

		<h3 class="above-list ">{_ Modules overview _}</h3>
		<ul class="short-list">
			<li class="headers clearfix">
				<span class="zp-20">{_ Title _}</span>
				<span class="zp-45">{_ Description _}</span>
				<span class="zp-5">{_ Prio _}</span>
				<span class="zp-20">{_ Author _}</span>
				<span class="zp-10">{_ Activate _}</span>
			</li>
		{% for sort, prio, module, props in modules %}
			<li id="{{ #li.module }}" {% if not props.is_active %}class="unpublished" {% endif %}>
				<a href="#" class="clearfix">
					<span class="zp-20">{% include "_icon_status.tpl" status=status[module] status_id=#status.module %} {{ props.mod_title|default:props.title }}</span>
					<span class="zp-45">{{ props.mod_description|default:"-" }}</span>
					<span class="zp-5">{{ prio }}</span>
					<span class="zp-20">{{ props.author|escape|default:"-" }}</span>
					<span class="zp-10">
						{% if props.is_active %}
							{% button text="Deactivate" 
									action={module_toggle module=module status_id=#status.module}
									action={toggle_class id=#li.module class="enabled"} %}
						{% else %}
							{% button text="Activate"
									action={module_toggle module=module status_id=#status.module} 
									action={toggle_class id=#li.module class="enabled"} %}
						{% endif %}
					</span>
				</a>
			</li>
		{% empty %}
			<li>{_ No modules found _}</li>
		{% endfor %}
		</ul>

		</div>
	</div>
{% endblock %}
