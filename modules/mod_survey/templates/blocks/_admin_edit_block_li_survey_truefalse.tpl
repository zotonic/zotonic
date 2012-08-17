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
               placeholder="{_ The earth is flat. _} ({{ lang_code }})" />

        <div class="controls">
            <label class="radio inline"><input type="radio" name="{{ name }}" class="nosubmit" />
                <input type="text" id="block-{{name}}-yes{{ lang_code_with_dollar }}" name="block-{{name}}-yes{{ lang_code_with_dollar }}" 
                      class="span3" value="{{ blk.yes[lang_code] }}"
                      placeholder="{_ True _}" />
            </label>
            <label class="radio inline"><input type="radio" name="{{ name }}" class="nosubmit" />
                <input type="text" id="block-{{name}}-no{{ lang_code_with_dollar }}" name="block-{{name}}-no{{ lang_code_with_dollar }}" 
                      class="span3" value="{{ blk.no[lang_code] }}"
                      placeholder="{_ False _}" />
            </label>
        </div>

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
        <label class="checkbox">
            <input type="checkbox" id="block-{{name}}-is_required" name="block-{{name}}-is_required" value="1" {% if blk.is_required or is_new %}checked="checked"{% endif %} />
            {_ Required, this question must be answered. _}
        </label>
    </div>
</fieldset>
{% endblock %}

