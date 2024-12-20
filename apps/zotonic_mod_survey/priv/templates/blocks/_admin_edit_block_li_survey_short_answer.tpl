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
               placeholder="{_ Question with a short answer _} ({{ lang_code }})" />
    </div>

    <div class="form-group view-expanded">
        <textarea class="form-control" id="block-{{name}}-explanation{{ lang_code_for_id }}" name="blocks[].explanation{{ lang_code_with_dollar }}" rows="2"
               placeholder="{_ Explanation _} ({{ lang_code }})" >{{ blk.explanation|translation:lang_code  }}</textarea>
       </div>

    <div class="form-group view-expanded">
        <input class="form-control" type="text" id="block-{{name}}-placeholder{{ lang_code_for_id }}" name="blocks[].placeholder{{ lang_code_with_dollar }}" value="{{ blk.placeholder|translation:lang_code  }}"
               placeholder="{_ Input value placeholder text _} ({{ lang_code }})" />
    </div>

    {% else %}
        <p>{{ blk.prompt|translation:lang_code  }}</p>
    {% endif %}
{% endblock %}

{% block widget_content_nolang %}
    <div class="form-group view-expanded question-validation">
        <label class="control-label" for="block-{{name}}-validation">{_ Validation _}</label>
        <div>
            <select class="form-control" id="block-{{name}}-validation" name="blocks[].validation">
                 <option value=""></option>
                 <option value="email" {% if blk.validation == "email" %}selected="selected"{% endif %}>{_ must be an email address _}</option>
                 <option value="numericality" {% if blk.validation == "numericality" %}selected="selected"{% endif %}>{_ must be a number _}</option>
                 <option value="phone" {% if blk.validation == "phone" %}selected="selected"{% endif %}>{_ must be a phone number _}</option>
                 <option value="date" {% if blk.validation == "date" %}selected="selected"{% endif %}>{_ must be a date _}</option>
            </select>
        </div>
    </div>

    {% javascript %}
        $('#block-{{name}}-validation').on('input', function(e) {
            switch ($(this).val()) {
                case 'numericality':
                case 'date':
                    $('#{{ #range }}').fadeIn();
                    $('#{{ #range }} input').removeClass('nosubmit');
                    break;
                default:
                    $('#{{ #range }}').hide();
                    $('#{{ #range }} input').addClass('nosubmit');
                    break;
            }
        });
    {% endjavascript %}

    <div class="form-group view-expanded question-options">
        <div {% if blk.validation != "numericality" and blk.validation != 'date' %}style="display: none"{% endif %} class="form-group" id="{{ #range }}">
            <div class="row">
                <div class="col-sm-6">
                    <label class="control-label">{_ Minimal value _}</label>
                    <input class="form-control" type="text" id="block-{{name}}-minval" name="blocks[].minval" value="{{ blk.minval }}">
                </div>
                <div class="col-sm-6">
                    <label class="control-label">{_ Maximal value _}</label>
                    <input class="form-control" type="text" id="block-{{name}}-maxval" name="blocks[].maxval" value="{{ blk.maxval }}">
                </div>
            </div>
            <p class="help-block">{_ For date ranges you can also use relative dates, like <code>-5 year</code>. _}</p>
        </div>

        <div class="checkbox">
            <label>
                <input type="checkbox" id="block-{{name}}-is_required" name="blocks[].is_required" value="1" {% if blk.is_required or is_new %}checked="checked"{% endif %} />
                {_ Required, this question must be answered. _}
            </label>
        </div>

        <div class="checkbox">
            <label>
                <input type="checkbox" id="block-{{name}}-is_hide_result" name="blocks[].is_hide_result" value="1" {% if blk.is_hide_result %}checked="checked"{% endif %} />
                {_ Hide from results _}
            </label>
        </div>
    </div>
{% endblock %}

