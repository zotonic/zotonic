{% extends "admin_base.tpl" %}

{% block title %} {_ Translation _} {% endblock %}

{% block content %}

<div id="content" class="zp-85">
	<div class="block clearfix">

		<h2>{_ Translation configuration and tools _}</h2>

		<div class="clearfix">
		    {% button text=_"Generate .pot files" 
					  action={postback postback="translation_generate" delegate="mod_translation"} %}
		    <span class="expl">{_ Scan all templates for translation tags and generate .pot files that can be used for translating the templates. _}</span>
		</div>

		<div class="clearfix">
		    {% button text=_"Reload Translations" 
					  action={postback postback="translation_reload" delegate="mod_translation"} %}
		    <span class="expl">{_ Reload all translations from the modules and site. All templates will be recompiled. _}</span>
		</div>

		<div class="clearfix">
		    <a href="{% url admin_translation_status %}" class="button">{_ Translation status _}</a>
		    <span class="expl">{_ Show per module how much of the templates are translated. _}</span>
		</div>

		<hr />

		<h3>{_ Languages overview _}</h3>

		<p>{_ All languages known to the system. You can add or remove languages. _}
		<br/>{_ Enabled languages show up in the language selection menu. The default language is used for new visitors without a selected language. _}</p>

		<div class="clearfix">
			{% button class="" text="add language" 
				action={dialog_open title=_"Add language" template="_dialog_language_edit.tpl" new}
			%}
		</div>

		<h3 class="above-list ">{_ Languages overview _}</h3>

		<ul class="short-list">
			<li class="headers clearfix">
				<span class="zp-10">{_ Enabled _}</span>
				<span class="zp-10">{_ Default _}</span>
				<span class="zp-15">{_ ISO Code _}</span>
				<span class="zp-40">{_ Language _}</span>
				<span class="zp-25">{_ Options _}</span>
			</li>
	{% with m.config.i18n.language.value as default_code %}
		{% for code, lang in m.config.i18n.language_list.list %}
			<li id="{{ #li.code }}">
				<span class="zp-10">
					<input type="checkbox" id="{{ #enabled.code }}" name="is_enabled" value="1"
						{% if lang.is_enabled %}checked="checked"{% endif %} />
					{% wire id=#enabled.code postback={language_enable code=code} delegate="mod_translation" %}
				</span>
				<span class="zp-10">
					<input type="radio" id="{{ #default.code }}" name="is_default" value="{{ code }}"
						{% if code == default_code %}checked="checked"{% endif %} />
					{% wire id=#default.code postback={language_default code=code} delegate="mod_translation" %}
				</span>
				<a id="{{ #a.code }}" href="#edit-config" class="clearfix">
					<span class="zp-15">{{ code|default:"-" }}</span>
					<span class="zp-40">{{ lang.language|default:"-" }}</span>
					<span class="zp-25">
						{% button text="delete" 
							action={dialog_open
										title=_"Delete language"
										template="_dialog_language_delete.tpl"
										code=code lang=lang
									}
						%}
						{% button text="edit" 
							action={dialog_open 
								 		title=_"Edit language" template="_dialog_language_edit.tpl"
										code=code lang=lang}
						%}
					</span>
				</a>
				{% wire id=#a.code 
					action={dialog_open 
						 		title=_"Edit language" template="_dialog_language_edit.tpl"
								code=code lang=lang}
				%}
			</li>
		{% empty %}
			<li>
				{_ No languages configured. _}
			</li>
		{% endfor %}
	{% endwith %}
		</ul>
	
	</div>
</div>

{% endblock %}
