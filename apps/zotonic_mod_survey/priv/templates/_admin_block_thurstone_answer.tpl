{# Thurstone test answer #}
<tr id="{{ #r }}">
    <td>
        {# push new answer entry #}
        <input type="hidden" name="blocks[].answers[]." value="">
        <a href="#delete-answer" class="btn"><span class="fa fa-trash"></span></a>
    </td>
    <td class="test-controls" {% if not blk.is_test %}style="display: none"{% endif %}>
        <label class="checkbox">
            <input type="checkbox" class="checkbox" style="height:32px;width:32px" value="1"
                   name="blocks[].answers[].is_correct"
                   {% if ans.is_correct %}checked{% endif %}>
            &nbsp;
        </label>
    </td>
    <td class="test-controls" {% if not blk.is_test %}style="display: none"{% endif %}>
        <input type="number" class="form-control" value="{{ ans.points_int }}"
               name="blocks[].answers[].points_int" style="width: 5ch">
    </td>
    <td>
        <input type="text" class="form-control" value="{{ ans.value|default:n }}" name="blocks[].answers[].value" placeholder="{{ n }}">
    </td>
    <td>
        {% for code,_lang in m.translation.language_list_editable %}
            <div class="widget-content-lang-{{ code }}" {% if code != edit_language %}style="display:none"{% endif %}>
                <input class="form-control"
                       name="blocks[].answers[].option${{ code }}"
                       placeholder="{_ Answer _} ({{ code }})"
                       value="{{ ans.option[code ]}}">

                <div class="test-controls" {% if not blk.is_test %}style="display: none"{% endif %}>
                    <input class="form-control widget-content-lang-{{ code }}"
                           name="blocks[].answers[].feedback${{ code }}"
                           placeholder="{_ Feedback if correct _} ({{ code }})"
                           value="{{ ans.feedback[code ]}}">
                </div>
            </div>
        {% endfor %}
    </td>
</tr>
