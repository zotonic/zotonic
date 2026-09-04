{% extends "admin_edit_widget_i18n.tpl" %}

{% block widget_title %}
{_ Block _}
<div class="widget-header-tools"></div>
{% endblock %}

{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-block-{{ name }}{% endblock %}
{% block widget_header %}{% endblock %}

{% block widget_content %}
    {% if id.is_editable %}
    <div class="form-group">
        <input class="form-control" type="text" id="block-{{name}}-prompt{{ lang_code_for_id }}" name="blocks[].prompt{{ lang_code_with_dollar }}" value="{{ blk.prompt|translation:lang_code  }}"
               placeholder="{_ Fill in the gaps _} ({{ lang_code }})" />
    </div>

    <div class="form-group view-expanded">
        <textarea class="form-control" id="block-{{name}}-explanation{{ lang_code_for_id }}" name="blocks[].explanation{{ lang_code_with_dollar }}" rows="2"
               placeholder="{_ Explanation _} ({{ lang_code }})" >{{ blk.explanation|translation:lang_code  }}</textarea>
    </div>

    <div class="form-group view-expanded">
       <textarea class="form-control" id="block-{{name}}-narrative{{ lang_code_for_id }}" name="blocks[].narrative{{ lang_code_with_dollar }}" rows="4"
              placeholder="{_ I am [age] years old. I like [icecream=vanilla|strawberry|chocolate|other] ice cream and my favorite color is [color]. _} ({{ lang_code }})" >{{ blk.narrative|translation:lang_code  }}</textarea>
                <p class="help-block"><strong>{_ Example: _}</strong> {_ I am [age] years old. I like [icecream=vanilla|strawberry|chocolate|other] ice cream and my favorite color is [color]. _}</p>
    </div>

    {% else %}
        <p>{{ blk.narrative|translation:lang_code  }}</p>
    {% endif %}
{% endblock %}

{% block widget_content_nolang %}
    <div class="form-group view-expanded question-options">
        {% include "blocks/_admin_survey_question_options.tpl" %}
    </div>
{% endblock %}
