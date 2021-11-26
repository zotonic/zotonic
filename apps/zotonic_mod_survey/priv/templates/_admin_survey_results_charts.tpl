{% extends "admin_edit_widget_std.tpl" %}

{# Admin controls for the survey #}

{% block widget_title %}
{_ Charts _}
<div class="widget-header-tools"></div>
{% endblock %}

{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}content-survey-charts{% endblock %}

{% block widget_content %}
    <div class="survey-results">
        {% for result, chart, question in m.survey.results[id] %}
            <div class="survey-result">
                {% if question.is_hide_result %}
                    {# nothing #}
                {% elseif not result %}
                    {% if question.type == 'header' %}
                        {% optional include ["blocks/_block_view_",question.type,".tpl"]|join id=id blk=question answers=answers %}
                    {% endif %}
                {% else %}
                    {% if chart.question %}
                        <h4>{{ chart.question }}</h4>
                    {% endif %}
                    {% if chart.charts %}
                        {% for c in chart.charts %}
                            {% if c.question %}
                                <h5>{{ c.question }}</h5>
                            {% endif %}
                            {% include "_survey_result_chart.tpl" chart=c %}
                        {% endfor %}
                    {% else %}
                        {% include "_survey_result_chart.tpl" %}
                    {% endif %}
                {% endif %}
            </div>
        {% endfor %}
    </div>
{% endblock %}
