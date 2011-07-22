{% extends "admin_base.tpl" %}

{% block title %}{_ Configuration _}{% endblock %}

{% block content %}

	<div id="content" class="zp-85">
		<div class="block clearfix">

			<h2>{_ System Configuration _}</h2>
			<div class="clearfix">
				{% button class="" text=_"make a new config setting" action={dialog_config_new on_success={reload}} %}
			</div>
			
			<hr />

			<h3 class="above-list ">{_ Configuration overview _}</h3>
			<ul class="short-list">
				<li class="headers clearfix">
					<span class="zp-15">{_ Module _}</span>
					<span class="zp-20">{_ Key _}</span>
					<span class="zp-35">{_ Value _}</span>
					<span class="zp-20">{_ Modified _} on</span>
					<span class="zp-10">{_ Options _}</span>
				</li>
			{% for module, keys in config %}
				{% for key, c in keys %}
					{% with c.id as id %}
						<li id="{{ #li.id }}" class="clearfix">
							<a id="{{ #a.id }}" href="#edit-config">
								<span class="zp-15">{{ module|escape|default:"-" }}</span>
								<span class="zp-20">{{ key|escape|default:"-" }}</span>
								<span class="zp-35">{{ c.value|escape|default:"-" }}</span>
								<span class="zp-30">{{ c.modified|date:"d M Y, H:i" }}</span>
							</a>
                            <span class="button-area">
                                {% button text=_"delete" disabled=(module=="zotonic") action={dialog_config_delete module=module key=key on_success={slide_fade_out target=#li.id}} %}
                                
                                {% button text=_"edit" disabled=(module=="zotonic") action={dialog_config_edit module=module key=key on_success={reload}} %}
                            </span>
							{% wire id=#a.id action={dialog_config_edit module=module key=key on_success={reload}} %}
						</li>
					{% endwith %}
				{% endfor %}
			{% empty %}
				<li>
					{_ No configurations found. _}
				</li>
			{% endfor %}
			</ul>
		</div>
	</div>

{% endblock %}
