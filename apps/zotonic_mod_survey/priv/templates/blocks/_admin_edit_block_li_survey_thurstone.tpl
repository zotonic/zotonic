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
          <input class="form-control" type="text" id="block-{{name}}-prompt{{ lang_code_for_id }}" name="blocks[].prompt{{ lang_code_with_dollar }}" value="{{ blk.prompt[lang_code]  }}"
                 placeholder="{_ Multiple choice or quiz question _} ({{ lang_code }})" />
      </div>

      <div class="form-group view-expanded">
         <textarea class="form-control" id="block-{{name}}-explanation{{ lang_code_for_id }}" name="blocks[].explanation{{ lang_code_with_dollar }}" rows="2"
                placeholder="{_ Explanation _} ({{ lang_code }})" >{{ blk.explanation[lang_code]  }}</textarea>
      </div>

      <div class="form-group view-expanded">
        <p class="help-block">
              {_ Add answer options here. Each stored value must be unique. _}
        </p>
        <p class="help-block test-controls" style="{% if not blk.is_test %}display:none{% endif %}">
            {_ Check the correct answers and give each answer points. _}<br>
            {_ Single answer possible: points are given for a correct answer. Default: 1 point for a correct answer. _}<br>
            {_ Multiple answers possible: points are given for the correct answer <i>and</i> points are given for <i>not</i> choosing the wrong answer. Default: 1 point for a correct answer, 1 point for not choosing a wrong answer. _}
         </p>
      </div>
    {% else %}
        <p>{{ blk.prompt[lang_code]  }}</p>
    {% endif %}
{% endblock %}

{% block widget_content_nolang %}

    <div class="row">
        <div class="col-md-6">
            <div class="form-group view-expanded">
                <select class="form-control" id="block-{{name}}-input_type" name="blocks[].input_type">
                  <option value="" {% if not blk.input_type %}selected{% endif %}>{_ Single answer possible _}</option>
                  <option value="select" {% if blk.input_type == "select" %}selected{% endif %}>{_ Drop-down menu _}</option>
                  <option value="multi" {% if blk.input_type == "multi" %}selected{% endif %}>{_ Multiple answers possible _}</option>
                  <option value="submit" {% if blk.input_type == "submit" %}selected{% endif %}>{_ Submit on clicking an option _}</option>
                </select>
            </div>
        </div>

        <div class="col-md-6">
            <div class="checkbox">
                <label>
                    <input type="checkbox" value="1" name="blocks[].is_test" id="{{ #is_test }}" {% if blk.is_test %}checked{% endif %} />
                    {_ Quiz or test question _}
                </label>
                {% wire id=#is_test
                        action={script script="$(this).closest('.block').find('.test-controls').slideToggle();"}
                %}
            </div>
        </div>
    </div>

{% with blk|survey_prepare_thurstone as blk %}

    {% with r_language|default:m.rsc[id].language|default:[z_language] as r_language %}
    {% with edit_language|default:z_language as edit_language %}
    {% with edit_language|member:r_language|if:edit_language:(r_language[1]) as edit_language %}
        <table class="table">
            <thead>
                <tr>
                    <th style="width: 5ch">
                    </th>
                    <th style="width: 5ch; {% if not blk.is_test %}display:none{% endif %}" class="test-controls">
                        {_ Correct _}
                    </th>
                    <th style="width: 5ch; {% if not blk.is_test %}display:none{% endif %}" class="test-controls">
                        {_ Points _}
                    </th>
                    <th style="width: 15ch">
                        {_ Stored value _}
                    </th>
                    <th>
                        {_ Answer _}
                    </th>
                </tr>
            </thead>
            <tbody id="{{ #answers }}">
                {% for ans in blk.answers %}
                    {% include "_admin_block_thurstone_answer.tpl" n=forloop.counter %}
                {% empty %}
                    {% include "_admin_block_thurstone_answer.tpl" n=1 %}
                {% endfor %}
            </tbody>
        </table>

        <p>
            <a id="{{ #answers_add }}" href="#" class="btn btn-default">{_ Add answer _}</a>
        </p>

        {% for lang_code,_lang in m.translation.language_list_editable %}
            <div class="widget-content-lang-{{ lang_code }}" {% if lang_code != edit_language %}style="display:none"{% endif %}>
                {% include "_admin_block_test_feedback.tpl" %}
            </div>
        {% endfor %}

    {% endwith %}
    {% endwith %}
    {% endwith %}

    <table style="display: none">
        <tbody id="{{ #answer_tpl }}">
            {% include "_admin_block_thurstone_answer.tpl" nosubmit %}
        </tbody>
    </table>

    {% javascript %}
        $('#{{ #answers }}').on('click', 'a[href="#delete-answer"]', function(e) {
            let row = $(this).closest('tr');
            e.preventDefault();
            z_dialog_confirm({
                text: "{_ Are you sure you want to delete this answer? _}",
                ok: "{_ Delete _}",
                on_confirm: function() {
                    $(row).remove();
                    $('#{{ #answers }}')
                        .find('tr')
                        .each(function(idx, tr) {
                            $(tr)
                                .find('input[name="blocks[].answers[].value"]')
                                .attr('placeholder', idx+1)
                        });
                }
            });
        });

        $('#{{ #answer_tpl }}').find('input').prop('disabled', true);

        $('#{{ #answers_add }}').click(function(e) {
            e.preventDefault();
            const nth = $('#{{ #answers }}').find('tr').length + 1;
            $('#{{ #answer_tpl }}')
                .find('input[name="blocks[].answers[].value"]')
                .attr("value", "" + nth)
                .attr("placeholder", "" + nth);
            $('#{{ #answers }}')
                .append( $('#{{ #answer_tpl }}').html() )
                .find('input').prop('disabled', false);
            console.log($('#{{ #answer_tpl }}')
                .find('input[name="blocks[].answers[].value"]'));
        });
    {% endjavascript %}

    <div class="row">
        <div class="col-md-6">
            <div class="form-group view-expanded">
                <div class="checkbox">
                    <label>
                        <input type="checkbox" id="block-{{name}}-is_random" name="blocks[].is_random" value="1" {% if blk.is_random %}checked="checked"{% endif %} />
                        {_ Randomize answers _}
                    </label>
                </div>
                <div class="question-options">
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
            </div>
        </div>

        <div class="col-md-6">
            <div class="form-group view-expanded survey-test-checkbox">
                <div class="checkbox">
                    <label class="test-controls" {% if not blk.is_test %}style="display:none"{% endif %}>
                        <input type="checkbox" value="1" name="blocks[].is_test_direct" id="{{ #is_test_direct }}" {% if blk.is_test_direct %}checked{% endif %} />
                        {_ Instant feedback (Learning Mode) _}
                    </label>
                </div>

                <div class="checkbox">
                    <label class="test-controls" {% if not blk.is_test %}style="display:none"{% endif %}>
                        <input type="checkbox" value="1" name="blocks[].is_test_neg" id="{{ #is_test_neg }}" {% if blk.is_test_neg %}checked{% endif %} />
                        {_ Subtract points for wrong answers (total never less than 0) _}
                    </label>
                </div>
            </div>
        </div>
    </div>
{% endwith %}
{% endblock %}

