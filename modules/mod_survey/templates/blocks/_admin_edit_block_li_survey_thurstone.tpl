{% extends "admin_edit_widget_i18n.tpl" %}

{% block widget_title %}{_ Block _}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-block-{{ name }}{% endblock %}
{% block widget_header %}{% endblock %}

{% block widget_content %}
{% with m.rsc[id] as r %}
<fieldset class="form-vertical">
    <div class="control-group">
    {% if is_editable %}
        <input type="text" id="block-{{name}}-prompt{{ lang_code_with_dollar }}" name="block-{{name}}-prompt{{ lang_code_with_dollar }}" 
               class="span8" value="{{ blk.prompt[lang_code]  }}"
               placeholder="{_ Prompt _} ({{ lang_code }})" />

       <textarea id="block-{{name}}-explanation{{ lang_code_with_dollar }}" name="block-{{name}}-explanation{{ lang_code_with_dollar }}" 
              class="span8" rows="2"
              placeholder="{_ Explanation _} ({{ lang_code }})" >{{ blk.explanation[lang_code]  }}</textarea>

       <label>{_ List of possible answers, one per line. Use <em>value#answer</em> for selecting values. _}</label>
       <textarea id="block-{{name}}-answers{{ lang_code_with_dollar }}" name="block-{{name}}-answers{{ lang_code_with_dollar }}" 
              class="span8" rows="6"
              placeholder="{_ Answers, one per line _} ({{ lang_code }})" >{{ blk.answers[lang_code]  }}</textarea>
    {% else %}
        <p>{{ blk.prompt[lang_code]  }}</p>
    {% endif %}
    </div>
</fieldset>
{% endwith %}
{% endblock %}

{% block widget_content_nolang %}
<fieldset class="form-vertical">
    <div class="control-group">
        <label class="radio">
            <input type="radio" id="block-{{name}}-input_type" name="block-{{name}}-input_type" value="" {% if not blk.input_type %}checked="checked"{% endif %} />
            {_ Single answer possible _}
        </label>
        <label class="radio">
            <input type="radio" id="block-{{name}}-input_type" name="block-{{name}}-input_type" value="multi" {% if blk.input_type == 'multi' %}checked="checked"{% endif %} />
            {_ Multiple answers possible _}
        </label>
        <label class="radio">
            <input type="radio" id="block-{{name}}-input_type" name="block-{{name}}-input_type" value="submit" {% if blk.input_type == 'submit' %}checked="checked"{% endif %} />
            {_ Submit on clicking an option _}
        </label>

        <label class="checkbox">
            <input type="checkbox" id="block-{{name}}-is_required" name="block-{{name}}-is_required" value="1" {% if blk.is_required or is_new %}checked="checked"{% endif %} />
            {_ Required, this question must be answered. _}
        </label>
    </div>
</fieldset>
{% endblock %}

