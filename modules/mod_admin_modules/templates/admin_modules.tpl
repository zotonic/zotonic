{% extends "admin_base.tpl" %}

{% block title %}{_ Modules _}{% endblock %}

{% block content %}
	<div id="content" class="zp-85"  {% include "_language_attrs.tpl" language=`en` %}>
		<div class="block clearfix">

		<h2>{_ Modules _}</h2>

		<h3 class="above-list ">{_ Modules overview _}</h3>
		<ul class="short-list">
			<li class="headers clearfix">
				<span class="zp-20">{_ Title _}</span>
				<span class="zp-45">{_ Description _}</span>
				<span class="zp-5">{_ Prio _}</span>
				<span class="zp-25">{_ Author _}</span>
				<span class="zp-5">{_ Activate _}</span>
			</li>
		{% for sort, prio, module, props in modules %}
			<li id="{{ #li.module }}" class="clearfix {% if not props.is_active %}unpublished{% endif %}">
				<a href="#">
					<span class="zp-20">{% include "_icon_status.tpl" status=status[module] status_id=#status.module %} {{ props.mod_title|default:props.title }}</span>
					<span class="zp-45">{{ props.mod_description|default:"-" }}</span>
					<span class="zp-5">{{ prio }}</span>
					<span class="zp-30">{{ props.author|escape|default:"-" }}</span>
                </a>
                <span class="button-area">
						{% if props.is_active %}
							{% button text=_"Deactivate" 
									action={module_toggle module=module status_id=#status.module}
									action={toggle_class id=#li.module class="enabled"} %}
						{% else %}
							{% button text=_"Activate"
									action={module_toggle module=module status_id=#status.module} 
									action={toggle_class id=#li.module class="enabled"} %}
						{% endif %}
                </span>
			</li>
		{% empty %}
			<li>{_ No modules found _}</li>
		{% endfor %}
		</ul>

		</div>
	</div>
{% endblock %}
