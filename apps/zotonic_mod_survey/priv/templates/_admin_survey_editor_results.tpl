<p class="help-block">
    {_ Click on “Edit” to see and change the entered data. _}
    {_ The result can be coupled to a known person or to a newly created person. _}<br>
    {_ The email address is shown if there is a question “email” or if a person with an email address is coupled. _}
</p>

{% with m.rsc[q.id].id|default:id as id %}
{% if id.is_editable %}
    {% with m.survey.list_results[id] as rs %}
    {% with id|survey_test_max_points as max_points %}
    {% if rs %}
        <table class="table table-striped">
            <tr>
                <th style="text-align:right">#</th>
                <th>{_ Date _}</th>
                <th>{_ Person _}</th>
                <th>{_ Email _}</th>
                {% if id.survey_test_percentage %}
                    <th style="text-align: right; width: 10%; min-width: 30px;">{_ Score _}</th>
                    <th style="text-align: right; width: 10%; min-width: 30px;">{_ Points _}</th>
                    <th style="width: 10%; min-width: 30px;">{_ Pass? _}</th>
                {% endif %}
                <th></th>
            </tr>
            {% for r in rs %}
            {% with r.id as r_id %}
            {% with r.props.answers as answers %}
                <tr class="do_clickable" id="survey-result-{{ r_id }}">
                    <td style="text-align:right"><span class="muted">{{ forloop.counter }}.</span></td>
                    <td>
                        {{ r.created|date:_"Y-m-d H:i" }}
                    </td>
                    <td>
                        {% if r.user_id %}
                            <a href="{% url admin_edit_rsc id=r.user_id %}">
                                {% include "_name.tpl" id=r.user_id sudo %}
                            </a>
                            <a id="{{ #link.r_id }}" class="btn btn-default btn-xs">{_ Change _}</a>
                            {% wire id=#link.r_id
                                    action={dialog_open
                                        template="_dialog_survey_link_person.tpl"
                                        id=id
                                        answer_id=r_id
                                        answers=answers
                                        title=_"Link with person"
                                    }
                            %}
                        {% else %}
                            <span>
                                {{ answers['name_first'].answer|escape }}
                                {{ answers['name_surname_prefix'].answer|escape }}
                                {{ answers['name_surname'].answer|escape }}
                            </span>
                            <a id="{{ #link.r_id }}" class="btn btn-default btn-xs">{_ Link with person _}</a>
                            {% wire id=#link.r_id
                                    action={dialog_open
                                        template="_dialog_survey_link_person.tpl"
                                        id=id
                                        answer_id=r_id
                                        answers=answers
                                        title=_"Link with person"
                                    }
                            %}
                        {% endif %}
                    </td>
                    <td>
                        {% if answers['email'].answer %}
                            <a href="mailto:{{ answers['email'].answer|urlencode }}">
                                {{ answers['email'].answer|escape }}
                            </a>
                        {% elseif answers['Email'].answer %}
                            <a href="mailto:{{ answers['Email'].answer|urlencode }}">
                                {{ answers['Email'].answer|escape }}
                            </a>
                        {% elseif r.user_id.email %}
                            <a href="mailto:{{ r.user_id.email }}">{{ r.user_id.email }}</a>
                        {% endif %}
                    </td>
                    {% if id.survey_test_percentage %}
                        <td style="text-align: right">{{ (r.points / max_points * 100)|round }}%</td>
                        <td style="text-align: right">{{ r.points }} / {{ max_points }}</td>
                        <td>
                            {% if r.points >= max_points * (id.survey_test_percentage / 100) %}
                                {_ Passed _}
                            {% else %}
                                {_ Failed _}
                            {% endif %}
                        </td>
                    {% endif %}
                    <td>
                        <div class="pull-right">
                            {% button class="btn btn-default" text=_"Edit"
                                      action={dialog_open template="_dialog_survey_editor.tpl" id=id answer_id=r_id title=_"Edit survey result"}
                            %}
                            {% button class="btn btn-default" text=_"Delete"
                                      postback={survey_remove_result_confirm id=id answer_id=r_id}
                                      delegate="mod_survey"
                            %}
                        </div>
                    </td>
                </tr>
            {% endwith %}
            {% endwith %}
            {% endfor %}
        </table>
    {% else %}
        <p>{_ No results yet. _}</p>
    {% endif %}
    {% endwith %}
    {% endwith %}
{% endif %}
{% endwith %}
