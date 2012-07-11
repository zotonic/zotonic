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
               placeholder="{_ Weasels make great pets. _} ({{ lang_code }})" />

{#
       <textarea id="block-{{name}}-question{{ lang_code_with_dollar }}" name="block-{{name}}-question{{ lang_code_with_dollar }}" 
              class="span8" rows="6"
              placeholder="{_ Question _} ({{ lang_code }})" >{{ blk.question[lang_code] }}</textarea>
#}
        <div class="controls">
            <label class="input inline">
                <input type="text" id="block-{{name}}-disagree{{ lang_code_with_dollar }}" name="block-{{name}}-disagree{{ lang_code_with_dollar }}" 
                      class="span3" value="{{ blk.disagree[lang_code] }}"
                      placeholder="{_ Strongly Disagree _}" />
            </label>
            <label class="radio inline"><input type="radio" name="{{ name }}" class="nosubmit" /> 1</label>
            <label class="radio inline"><input type="radio" name="{{ name }}" class="nosubmit" /> 2</label>
            <label class="radio inline"><input type="radio" name="{{ name }}" class="nosubmit" /> 3</label>
            <label class="radio inline"><input type="radio" name="{{ name }}" class="nosubmit" /> 4</label>
            <label class="radio inline"><input type="radio" name="{{ name }}" class="nosubmit" /> 5</label>
            <label class="input inline">
                <input type="text" id="block-{{name}}-agree{{ lang_code_with_dollar }}" name="block-{{name}}-agree{{ lang_code_with_dollar }}" 
                      class="span3" value="{{ blk.agree[lang_code] }}"
                      placeholder="{_ Strongly Agree _}" />
            </label>
        </div>
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
        <label class="checkbox">
            <input type="checkbox" id="block-{{name}}-is_required" name="block-{{name}}-is_required" value="1" {% if blk.is_required or is_new %}checked="checked"{% endif %} />
            {_ Required, this question must be answered. _}
        </label>
    </div>
</fieldset>
{% endblock %}

