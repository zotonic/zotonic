{% if id.is_editable %}
{% with m.survey.all_results[id] as r %}
{% with r|first as columns %}
{% with r|tail as results %}
{% with m.survey.captions[id] as captions %}

<table class="table table-striped">
    <thead>
        <tr>
            <th>&nbsp;</th>
            {% for name in columns %}
            <th>{{ name|capfirst }}</th>
            {% endfor %}
            <th>&nbsp;</th>
        </tr>
    </thead>
    <tbody>
        {% for answer_id,r in results %}
        <tr id="survey-result-{{ answer_id }}">
            <td align="right">{{ forloop.counter }}.&nbsp;&nbsp;</td>
            {% for value in r %}
                <td>{{ value|escape }}</td>
            {% endfor %}
            <td>
                <div class="pull-right">
                    {% button class="btn btn-mini" text=_"Edit"
                              action={dialog_open template="_dialog_survey_editor.tpl" id=id answer_id=answer_id title=_"Edit survey result"}
                    %}
                    {% button class="btn btn-mini" text=_"Delete"
                              postback={survey_remove_result id=id answer_id=answer_id}
                              delegate="mod_survey"
                    %}
                </div>
            </td>
        </tr>
        {% endfor %}
    </tbody>
</table>

{% endwith %}
{% endwith %}
{% endwith %}
{% endwith %}
{% endif %}
