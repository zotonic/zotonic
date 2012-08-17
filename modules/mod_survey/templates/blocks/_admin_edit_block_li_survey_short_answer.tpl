{% extends "admin_edit_widget_i18n.tpl" %}

{% block widget_title %}{_ Block _}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-block-{{ name }}{% endblock %}

{% block widget_content %}
{% with m.rsc[id] as r %}
<fieldset class="form-vertical">
    <div class="control-group">
    {% if is_editable %}
        <input type="text" id="block-{{name}}-prompt{{ lang_code_with_dollar }}" name="block-{{name}}-prompt{{ lang_code_with_dollar }}" 
               class="span8" value="{{ blk.prompt[lang_code] }}"
               placeholder="{_ Please enter your name. _} ({{ lang_code }})" />

        <textarea id="block-{{name}}-explanation{{ lang_code_with_dollar }}" name="block-{{name}}-explanation{{ lang_code_with_dollar }}" 
               class="span8" rows="2"
               placeholder="{_ Explanation _} ({{ lang_code }})" >{{ blk.explanation[lang_code] }}</textarea>

    {% else %}
        <p>{{ blk.prompt[lang_code] }}</p>
    {% endif %}
    </div>
</fieldset>
{% endwith %}
{% endblock %}

{% block widget_content_nolang %}
<fieldset class="form-vertical">
    <div class="control-group">
        <label for="block-{{name}}-validation">{_ Validation _}</label>
        <select id="block-{{name}}-validation" name="block-{{name}}-validation">
             <option value=""></option>
             <option value="email" {% if blk.validation == "email" %}selected="selected"{% endif %}>{_ must be an e-mail address _}</option>
             <option value="numericality" {% if blk.validation == "numericality" %}selected="selected"{% endif %}>{_ must be a number _}</option>
        </select>

        <label class="checkbox">
            <input type="checkbox" id="block-{{name}}-is_required" name="block-{{name}}-is_required" value="1" {% if blk.is_required or is_new %}checked="checked"{% endif %} />
            {_ Required, this question must be answered. _}
        </label>
    </div>
</fieldset>
{% endblock %}

