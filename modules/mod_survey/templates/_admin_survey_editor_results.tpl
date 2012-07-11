{% with q.id|default:id as id %}
{% with m.survey.all_results[id] as r %}
{% with r|first as columns %}
{% with r|tail as results %}
{% with m.survey.captions[id] as captions %}

<table class="table table-striped">
    <thead>
        <tr>
            <th>&nbsp;</th>
            {% for name in columns|tail|tail %}
            <th>{{ name|capfirst }}</th>
            {% endfor %}
            <th>&nbsp;</th>
        </tr>
    </thead>
    <tbody>
        {% for r in results %}
        <tr id="survey-result-{{ r[1] }}-{{ r[2] }}">
            <td align="right">{{ forloop.counter }}.&nbsp;&nbsp;</td>
            {% for value in r|tail|tail %}
            <td>{{ value }}</td>
            {% endfor %}
            <td>
                <div class="pull-right">
                    {% button class="btn btn-mini" text=_"Edit" action={dialog_open template="_dialog_survey_editor.tpl" id=id persistent_id=r[2] user_id=r[1] title=_"Edit survey result"} %}
                    {% button class="btn btn-mini" text=_"Delete" postback={survey_remove_result id=id persistent_id=r[2] user_id=r[1]} delegate="mod_survey" %}
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
{% endwith %}
