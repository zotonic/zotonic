{% extends "admin_edit_widget_i18n.tpl" %}

{% block widget_title %}
{_ Block _}
<div class="widget-header-tools"></div>
{% endblock %}

{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-block-{{ name }}{% endblock %}
{% block widget_header %}{% endblock %}

{% block widget_content %}
    <p class="alert alert-info view-expanded">
        {_ This block show a file upload field. This can only be used as the last question of a form. The default form routines can’t handle file upload, you will need to add your own form handler to your site or module. _}
    </p>

    {% if id.is_editable %}
    <div class="form-group">
        <input class="form-control" type="text" id="block-{{name}}-prompt{{ lang_code_for_id }}" name="blocks[].prompt{{ lang_code_with_dollar }}" value="{{ blk.prompt|translation:lang_code  }}"
               placeholder="{_ Please upload your image. _} ({{ lang_code }})" />
    </div>
    {% else %}
        <p>{{ blk.prompt|translation:lang_code  }}</p>
    {% endif %}
{% endblock %}

{% block widget_content_nolang %}
    <div class="view-expanded view-expanded">
        <div class="form-group">
            <div class="checkbox">
                <label>
                    <input type="checkbox" id="block-{{name}}-is_image" name="blocks[].is_image" value="1" {% if blk.is_image or is_new %}checked="checked"{% endif %} />
                    {_ Only accept images. _}
                </label>
            </div>

            <div class="question-options">
                {% include "blocks/_admin_survey_question_options.tpl" is_resultless %}
            </div>
        </div>
    </div>
{% endblock %}
