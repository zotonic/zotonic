{% with m.rsc[id] as r %}
{% with m.rsc[id].is_editable as is_editable %}
<fieldset>
    <div class="control-group">
	<label class="control-label" for="field-title{{ lang_code_with_dollar }}">{_ Title _} {{ lang_code_with_brackets }}</label>
        <div class="controls">
	    <input type="text" id="field-title{{ lang_code_with_dollar }}" name="title{{ lang_code_with_dollar }}" 
		   value="{{ is_i18n|if : r.translation[lang_code].title : r.title }}"
		   {% if not is_editable %}disabled="disabled"{% endif %}
		{% include "_language_attrs.tpl" language=lang_code class="do_autofocus span8 field-title" %}
                />
        </div>
    </div>

    <div class="control-group">
	<label class="control-label" for="field-summary{{ lang_code_with_dollar }}">{_ Summary _} {{ lang_code_with_brackets }}</label>
        <div class="controls">
	    <textarea rows="4" cols="10" id="field-summary{{ lang_code_with_dollar }}" 
		      name="summary{{ lang_code_with_dollar }}"
		      {% if not is_editable %}disabled="disabled"{% endif %}
		      {% include "_language_attrs.tpl" language=lang_code class="span8 intro" %}
		      >{{ is_i18n|if : r.translation[lang_code].summary : r.summary }}</textarea>
	</div>
    </div>
    
    <div class="control-group">
	<label class="control-label" for="field-short-title{{ lang_code_with_dollar }}">{_ Short title _} {{ lang_code_with_brackets }}</label>
        <div class="controls">
	    <input type="text" id="field-short-title{{ lang_code_with_dollar }}" name="short_title{{ lang_code_with_dollar }}" 
			value="{{ is_i18n|if : r.translation[lang_code].short_title : r.short_title }}"
			{% if not is_editable %}disabled="disabled"{% endif %}
			{% include "_language_attrs.tpl" language=lang_code class="span8" %} />
       </div>
   </div>
</fieldset>

{% endwith %}
{% endwith %}
