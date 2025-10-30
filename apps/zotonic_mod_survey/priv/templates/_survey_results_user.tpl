{# Personal results #}

{% with m.survey.did_survey_results[id] as result %}
    {% if id.survey_test_percentage %}
        {% if id|survey_test_max_points as max_points %}
            {% with result.points >= max_points * (id.survey_test_percentage / 100) as is_passed %}
                <h2>
                    {{ (result.points / max_points * 100)|round }}% &ndash;
                    {% if is_passed %}
                        {_ Passed _}
                    {% else %}
                        {_ Failed _}
                    {% endif %}
                </h2>

                <table class="table" style="width: auto">
                    <tr>
                        <td>{_ Points _}</td>
                        <th style="text-align: right">{{ result.points }} / {{ max_points }}</th>
                    </tr>
                    <tr>
                        <td>{_ Needed for pass _}</td>
                        <th style="text-align: right">{{ id.survey_test_percentage }}%</th>
                    </tr>
                    <tr>
                        <td>{_ Your result _}</td>
                        <th style="text-align: right">{{ (result.points / max_points * 100)|round }}%</th>
                    </tr>
                </table>

                {% if (is_passed or id.survey_show_results /= 3) %}
                    {% for blk in id.blocks %}
                        {% if blk.is_hide_result %}
                            {# nothing #}
                        {% elseif blk.name != 'survey_feedback' %}
                            {% optional include "blocks/_block_view_"++blk.type++".tpl" blk=blk is_survey_answer_view result=result %}
                        {% endif %}
                    {% endfor %}
                {% endif %}
            {% endwith %}
        {% endif %}
    {% else %}
        {% for blk in id.blocks %}
            {% if blk.is_hide_result %}
                {# nothing #}
            {% elseif blk.name != 'survey_feedback' %}
                {% optional include "blocks/_block_view_"++blk.type++".tpl" blk=blk is_survey_answer_view result=result %}
            {% endif %}
        {% endfor %}
    {% endif %}
{% endwith %}
