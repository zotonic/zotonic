{% extends "admin_edit_widget_i18n.tpl" %}

{% block widget_title %}{_ Block _}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-block-{{ name }}{% endblock %}
{% block widget_header %}{% endblock %}

{% block widget_content %}
{% with m.rsc[id] as r %}
    {% if is_editable %}
    <div class="form-group">
        <input class="form-control" type="text" id="block-{{name}}-prompt{{ lang_code_for_id }}" name="block-{{name}}-prompt{{ lang_code_with_dollar }}" value="{{ blk.prompt[lang_code]  }}"
               placeholder="{_ Please make a choice. _} ({{ lang_code }})" />
    </div>

    <div class="form-group view-expanded">
        <textarea class="form-control" id="block-{{name}}-explanation{{ lang_code_for_id }}" name="block-{{name}}-explanation{{ lang_code_with_dollar }}" rows="2"
               placeholder="{_ Explanation _} ({{ lang_code }})" >{{ blk.explanation[lang_code]  }}</textarea>
    </div>

    {% else %}
        <p>{{ blk.prompt[lang_code]  }}</p>
    {% endif %}
{% endwith %}
{% endblock %}

{% block widget_content_nolang %}
    <div class="form-group view-expanded">
        <label for="block-{{name}}-choices">{_ Choices _} ({_ one per line _})</label>
        <textarea class="form-control" id="block-{{name}}-choices" name="block-{{name}}-choices" rows="4">{{ blk.choices }}</textarea>
    </div>

    <div class="form-group view-expanded">
        <div class="checkbox"><label>
            <input type="checkbox" id="block-{{name}}-is_required" name="block-{{name}}-is_required" value="1" {% if blk.is_required or is_new %}checked="checked"{% endif %} />
            {_ Required, this question must be answered. _}
        </label></div>

        <div class="checkbox"><label>
            <input type="checkbox" id="block-{{name}}-is_numeric" name="block-{{name}}-is_numeric" value="1" {% if blk.is_numeric %}checked="checked"{% endif %} />
            {_ Numeric input, show totals for this field in the survey results. _}
        </label></div>
    </div>
{% endblock %}

