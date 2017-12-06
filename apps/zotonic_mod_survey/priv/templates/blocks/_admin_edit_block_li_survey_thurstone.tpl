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
          <input class="form-control" type="text" id="block-{{name}}-prompt{{ lang_code_for_id }}" name="block-{{name}}-prompt{{ lang_code_with_dollar }}" value="{{ blk.prompt[lang_code]  }}"
                 placeholder="{_ Prompt _} ({{ lang_code }})" />
      </div>

      <div class="form-group view-expanded">
         <textarea class="form-control" id="block-{{name}}-explanation{{ lang_code_for_id }}" name="block-{{name}}-explanation{{ lang_code_with_dollar }}" rows="2"
                placeholder="{_ Explanation _} ({{ lang_code }})" >{{ blk.explanation[lang_code]  }}</textarea>
      </div>

      <div class="form-group view-expanded">
         <label class="control-label">
              {_ List of possible answers, one per line. Use <em>value#answer</em> to store “value” but to show “answer”. E.g.  “2029#The year 2029”. _}
              <span class="test-controls" {% if not blk.is_test %}style="display:none"{% endif %}>
                  {_ For quiz or test, mark correct answers with a “*””. E.g. “*value#answer” _}
              </span>
         </label>
         <textarea class="form-control" id="block-{{name}}-answers{{ lang_code_for_id }}" name="block-{{name}}-answers{{ lang_code_with_dollar }}" rows="4"
                placeholder="{_ Answers, one per line _} ({{ lang_code }})" >{{ blk.answers[lang_code]  }}</textarea>
      </div>

      {% include "_admin_block_test_feedback.tpl" %}

    {% else %}
        <p>{{ blk.prompt[lang_code]  }}</p>
    {% endif %}
{% endblock %}

{% block widget_content_nolang %}
    <div class="row">
        <div class="col-md-6">
            <div class="form-group view-expanded">
                <select class="form-control" id="block-{{name}}-input_type" name="block-{{name}}-input_type">
                  <option value="" {% if not blk.input_type %}selected{% endif %}>{_ Single answer possible _}</option>
                  <option value="select" {% if blk.input_type == "select" %}selected{% endif %}>{_ Drop-down menu _}</option>
                  <option value="multi" {% if blk.input_type == "multi" %}selected{% endif %}>{_ Multiple answers possible _}</option>
                  <option value="submit" {% if blk.input_type == "submit" %}selected{% endif %}>{_ Submit on clicking an option _}</option>
                </select>
            </div>
            <div class="form-group view-expanded">
                <div class="checkbox">
                    <label>
                        <input type="checkbox" id="block-{{name}}-is_random" name="block-{{name}}-is_random" value="1" {% if blk.is_random %}checked="checked"{% endif %} />
                        {_ Randomize answers _}
                    </label>
                </div>
                <div class="question-options">
                    <div class="checkbox">
                        <label>
                            <input type="checkbox" id="block-{{name}}-is_required" name="block-{{name}}-is_required" value="1" {% if blk.is_required or is_new %}checked="checked"{% endif %} />
                            {_ Required, this question must be answered. _}
                        </label>
                    </div>

                    <div class="checkbox">
                        <label>
                            <input type="checkbox" id="block-{{name}}-is_hide_result" name="block-{{name}}-is_hide_result" value="1" {% if blk.is_hide_result %}checked="checked"{% endif %} />
                            {_ Hide from results _}
                        </label>
                    </div>
                </div>
            </div>
        </div>

        <div class="col-md-6">
            {% include "_admin_block_test_checkbox.tpl" %}
        </div>
    </div>
{% endblock %}

