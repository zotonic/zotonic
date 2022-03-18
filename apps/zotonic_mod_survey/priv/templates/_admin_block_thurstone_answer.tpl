{# Thurstone test answer #}
<tr id="{{ #r }}">
    <td>
        <input type="hidden" name="blocks[].answer[]." value="">
        <a href="#delete-answer" class="btn"><span class="fa fa-trash"</span></a>
    </td>
    <td class="test-controls" {% if not blk.is_test %}style="display: none"{% endif %}>
        <input type="checkbox" class="form-control" value="1"
               name="blocks[].answer[].is_correct"
               {% if ans.is_correct %}checked{% endif %}>
    </td>
    <td class="test-controls" {% if not blk.is_test %}style="display: none"{% endif %}>
        <input type="number" class="form-control" value="{{ ans.points_int }}"
               name="blocks[].answer[].points_int" style="width: 5ch">
    </td>
    <td>
        <input type="text" class="form-control" value="{{ ans.value|default:n }}" name="blocks[].answer[].value" placeholder="{{ n }}">
    </td>
    <td>
        {% for code,_lang in m.translation.language_list_editable %}
            <div class="widget-content-lang-{{ code }}" {% if code != edit_language %}style="display:none"{% endif %}>
                <input class="form-control"
                       name="blocks[].answer[].text${{ code }}"
                       placeholder="{_ Answer _} ({{ code }})"
                       value="{{ ans.text[code ]}}">

                <div class="test-controls" {% if not blk.is_test %}style="display: none"{% endif %}>
                    <input class="form-control widget-content-lang-{{ code }}"
                           name="blocks[].answer[].feedback_correct${{ code }}"
                           placeholder="{_ Feedback if correct _} ({{ code }})" 
                           value="{{ ans.feedback_correct[code ]}}">

                    <input class="form-control widget-content-lang-{{ code }}"
                           name="blocks[].answer[].feedback_wrong${{ code }}"
                           placeholder="{_ Feedback if wrong _} ({{ code }})"
                           value="{{ ans.feedback_wrong[code ]}}">
                </div>
            </div>
        {% endfor %}
    </td>
</tr>
