{% with m.rsc[id].language as r_lang %}
<div class="form-group">
    <div id="admin-translation-checkboxes">
        {% for code, lang in languages %}
            {% if lang.is_enabled %}
            <label class="checkbox-inline">
    	    <input type="checkbox" id="{{ #language.code }}" name="language" value="{{ code }}"
    	           {% if code|member:r_lang or (not r_lang and z_language == code) %}checked="checked"{% endif %} /> 
    	    <span {% include "_language_attrs.tpl" language=code %}>{{ lang.language }}</span>
            </label>
            {% wire id=#language.code action={toggle selector=[".tab-",code|make_list]} %}
            {% endif %}
        {% empty %}
            <div class="checkbox"><label><input type="checkbox" checked="checked" disabled="disabled"> {{ z_language }}</label></div>
        {% endfor %}
    </div>
</div>
{% endwith %}
