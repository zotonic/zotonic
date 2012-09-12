{% extends "admin_edit_widget_i18n.tpl" %}

{% block widget_title %}{_ Block _}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-block-{{ #block }}{% endblock %}
{% block widget_header %}{% endblock %}

{% block widget_content %}
{% with m.rsc[id] as r %}
<fieldset class="form-vertical">
    <div class="control-group">
    {% if is_editable %}
        <input type="text" id="block-{{name}}-header{{ lang_code_with_dollar }}" name="block-{{name}}-header{{ lang_code_with_dollar }}" 
               class="span8" value="{{ blk.header[lang_code] }}"
               placeholder="{_ Header _} ({{ lang_code }})" />
    {% else %}
        <h3>{{ blk.header[lang_code]  }}</h3>
    {% endif %}
    </div>
</fieldset>
{% endwith %}
{% endblock %}
