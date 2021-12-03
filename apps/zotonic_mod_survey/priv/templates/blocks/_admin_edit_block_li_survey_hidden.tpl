{% extends "admin_edit_widget_i18n.tpl" %}

{% block widget_title %}
{_ Block _}
<div class="widget-header-tools"></div>
{% endblock %}

{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-block-{{ name }}{% endblock %}
{% block widget_header %}{% endblock %}

{% block widget_content %}
    <p class="help-block">
        {_ Hidden value, saved with the survey but not shown to the person filling in the form. _}
    </p>
    {% if id.is_editable %}
        <div class="form-group view-expanded">
            <input class="form-control" type="text" id="block-{{name}}-value{{ lang_code_for_id }}" name="blocks[].value{{ lang_code_with_dollar }}" value="{{ blk.value[lang_code]  }}"
                   placeholder="{_ Input value _} ({{ lang_code }})" />
        </div>
    {% else %}
        <p>{{ blk.value[lang_code] }}</p>
    {% endif %}
{% endblock %}

{% block widget_content_nolang %}
{% endblock %}

