{% with m.rsc[id] as r %}
{% with not id or m.rsc[id].is_editable as is_editable %}
<fieldset>
    <div class="control-group">
	<label class="control-label" for="{{ #title }}{{ lang_code_for_id }}">{_ Title _} {{ lang_code_with_brackets }}</label>
        <div class="controls">
	    <input type="text" id="{{ #title }}{{ lang_code_for_id }}" name="title{{ lang_code_with_dollar }}" 
		   value="{{ is_i18n|if : r.translation[lang_code].title : r.title }}"
		   {% if not is_editable %}disabled="disabled"{% endif %}
		{% include "_language_attrs.tpl" language=lang_code class="do_autofocus input-block-level field-title" %}
                />
        </div>
    </div>

    <div class="control-group">
	<label class="control-label" for="{{ #summary }}{{ lang_code_for_id }}">{_ Summary _} {{ lang_code_with_brackets }}</label>
        <div class="controls">
	    <textarea rows="4" cols="10" id="{{ #summary }}{{ lang_code_for_id }}" 
		      name="summary{{ lang_code_with_dollar }}"
		      {% if not is_editable %}disabled="disabled"{% endif %}
		      {% include "_language_attrs.tpl" language=lang_code class="input-block-level intro" %}
		      >{{ is_i18n|if : r.translation[lang_code].summary : r.summary | brlinebreaks }}</textarea>
	</div>
    </div>
    
    <div class="control-group">
	<label class="control-label" for="{{ #shorttitle }}{{ lang_code_for_id }}">{_ Short title _} {{ lang_code_with_brackets }}</label>
        <div class="controls">
	    <input type="text" id="{{ #shorttitle }}{{ lang_code_for_id }}" name="short_title{{ lang_code_with_dollar }}" 
			value="{{ is_i18n|if : r.translation[lang_code].short_title : r.short_title }}"
			{% if not is_editable %}disabled="disabled"{% endif %}
			{% include "_language_attrs.tpl" language=lang_code class="input-block-level" %} />
       </div>
   </div>
</fieldset>

{% endwith %}
{% endwith %}
