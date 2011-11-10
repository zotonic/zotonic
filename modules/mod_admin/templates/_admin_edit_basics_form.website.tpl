{% with m.rsc[id] as r %}
{% with m.rsc[id].is_editable as is_editable %}
<fieldset class="{% if not in_dialog %}admin-form{% endif %}">
	<div class="form-item clearfix">
		<label for="field-title{{ lang_code_with_dollar }}">{_ Title _} {{ lang_code_with_brackets }}</label>
		<input type="text" class="field-title" id="field-title{{ lang_code_with_dollar }}" name="title{{ lang_code_with_dollar }}" 
			value="{{ is_i18n|if : r.translation[lang_code].title : r.title }}"
			{% if not is_editable %}disabled="disabled"{% endif %}
			{% include "_language_attrs.tpl" language=lang_code %}/>
	</div>

	<div class="form-item clearfix">
		<label for="field-short-title{{ lang_code_with_dollar }}">{_ Short title _} {{ lang_code_with_brackets }}</label>
		<input type="text" id="field-short-title{{ lang_code_with_dollar }}" name="short_title{{ lang_code_with_dollar }}" 
			value="{{ is_i18n|if : r.translation[lang_code].short_title : r.short_title }}"
			{% if not is_editable %}disabled="disabled"{% endif %} 
			{% include "_language_attrs.tpl" language=lang_code %} />
	</div>

	<div class="form-item clearfix">
        <label for="website">{_ Website _}</label>
        <input id="website" type="text" name="website" value="{{ r.website }}" style="width: 295px"
			{% if not is_editable %}disabled="disabled"{% endif %} />
	</div>
</fieldset>
{% endwith %}
{% endwith %}
