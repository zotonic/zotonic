{% with not id or id.is_editable as is_editable %}
<fieldset>
    <div class="form-group label-floating">
        <input type="text" id="{{ #title }}{{ lang_code_for_id }}"
            name="title{{ lang_code_with_dollar }}"
            placeholder="{_ Title _} {{ lang_code_with_brackets }}"
            value="{{ is_i18n|if : id.translation[lang_code].title : id.title }}"
            {% if not is_editable %}disabled="disabled"{% endif %}
            {% include "_language_attrs.tpl" language=lang_code class="do_autofocus field-title form-control" %}
        >
        <label class="control-label" for="{{ #title }}{{ lang_code_for_id }}">
            {_ Title _} {{ lang_code_with_brackets }}
        </label>
    </div>

    <div class="form-group label-floating">
        <textarea rows="4" cols="10" id="{{ #summary }}{{ lang_code_for_id }}"
            name="summary{{ lang_code_with_dollar }}"
            placeholder="{_ Summary _} {{ lang_code_with_brackets }}"
            {% if not is_editable %}disabled="disabled"{% endif %}
            {% include "_language_attrs.tpl" language=lang_code class="intro form-control field-summary" %}>{{ is_i18n|if : id.translation[lang_code].summary : id.summary | brlinebreaks }}</textarea>
        <label class="control-label" for="{{ #summary }}{{ lang_code_for_id }}">
            {_ Summary _} {{ lang_code_with_brackets }}
        </label>
    </div>

    <div class="form-group label-floating">
        <input class="form-control" type="text" id="{{ #shorttitle }}{{ lang_code_for_id }}"
            name="short_title{{ lang_code_with_dollar }}"
            value="{{ is_i18n|if : id.translation[lang_code].short_title : id.short_title }}"
            placeholder="{_ Short title _} {{ lang_code_with_brackets }}"
            {% if not is_editable %}disabled="disabled"{% endif %}
            {% include "_language_attrs.tpl" language=lang_code %}
        >
        <label class="control-label" for="{{ #shorttitle }}{{ lang_code_for_id }}">
            {_ Short title _} {{ lang_code_with_brackets }}
        </label>
    </div>
</fieldset>

{% endwith %}
