{% extends "admin_edit_widget_i18n.tpl" %}

{% block widget_title %}
{_ Block _}
<div class="widget-header-tools"></div>
{% endblock %}

{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-block-{{ #block }}{% endblock %}
{% block widget_header %}{% endblock %}

{% block widget_content %}
{% with m.rsc[id] as r %}
<fieldset class="form-vertical">
    <div class="form-group">
    {% if is_editable %}
        <input class="form-control" type="text" id="block-{{name}}-header{{ lang_code_for_id }}" name="block-{{name}}-header{{ lang_code_with_dollar }}" value="{{ blk.header[lang_code] }}"
               placeholder="{_ Header _} ({{ lang_code }})" />
    {% else %}
        <h3>{{ blk.header[lang_code]  }}</h3>
    {% endif %}
    </div>
</fieldset>
{% endwith %}
{% endblock %}
