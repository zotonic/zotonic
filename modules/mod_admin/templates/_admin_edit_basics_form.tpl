{% with m.rsc[id] as r %}
{% with m.rsc[id].is_editable as is_editable %}
<fieldset class="{% if not in_dialog %}admin-form{% endif %}">
	<div class="form-item clearfix">
		<label for="field-title{{ lang_code_with_dollar }}">{_ Title _} {{ lang_code_with_brackets }}</label>
		<input type="text" id="field-title{{ lang_code_with_dollar }}" name="title{{ lang_code_with_dollar }}" 
			value="{{ is_i18n|if : r.translation[lang_code].title : r.title }}"
			{% if not is_editable %}disabled="disabled"{% endif %}
			{% include "_language_attrs.tpl" language=lang_code class="field-title" %} />
	</div>

	<div class="form-item clearfix">
		<label for="field-summary{{ lang_code_with_dollar }}">{_ Summary _} {{ lang_code_with_brackets }}</label>
		<textarea rows="4" cols="10" id="field-summary{{ lang_code_with_dollar }}" 
			name="summary{{ lang_code_with_dollar }}"
			{% if not is_editable %}disabled="disabled"{% endif %}
			{% include "_language_attrs.tpl" language=lang_code class="intro" %}
			>{{ is_i18n|if : r.translation[lang_code].summary : r.summary }}</textarea>
	</div>

	<div class="form-item clearfix">
		<label for="field-short-title{{ lang_code_with_dollar }}">{_ Short title _} {{ lang_code_with_brackets }}</label>
		<input type="text" id="field-short-title{{ lang_code_with_dollar }}" name="short_title{{ lang_code_with_dollar }}" 
			value="{{ is_i18n|if : r.translation[lang_code].short_title : r.short_title }}"
			{% if not is_editable %}disabled="disabled"{% endif %}
			{% include "_language_attrs.tpl" language=lang_code %} />
	</div>
</fieldset>

{% endwith %}
{% endwith %}
