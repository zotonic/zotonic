{% with m.config.i18n.language_list.list as list %}
	{% if list %}
		<select id="{{ #lang }}">
		{% for code,lang in list %}
			{% if all or lang.is_enabled %}
				<option {% if z_language == code %}selected="selected"{% endif %} value="{{ code }}">{{ lang.language }}</option>
			{% endif %}
		{% endfor %}
		</select>
		{% wire id=#lang type="change" postback={set_language} delegate="mod_translation" %}
	{% endif %}
{% endwith %}
