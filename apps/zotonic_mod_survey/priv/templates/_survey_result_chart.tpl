{% if chart.type == "pie" %}
    <div class="graph survey-chart clearfix">
        {% if chart.name %}
            <h5 class="survey-chart__title">{{ chart.name }}:</h5>
        {% endif %}

        <div class="survey-chart__body">
            <div class="graph__pie survey-chart__plot">
                {% chart type="pie"
                         height=180
                         width=400
                         data=chart.values
                         title=chart.name|default:chart.question
                         aria_describedby=#chart_data
                         hide_table
                %}
            </div>

            <table id="{{ #chart_data }}" class="table table-compact graph__results survey-chart__results">
                <caption class="sr-only">{{ chart.name|default:chart.question }}</caption>
                <thead class="sr-only">
                    <tr>
                        <th scope="col">{_ Answer _}</th>
                        <th scope="col">{_ Responses _}</th>
                    </tr>
                </thead>
                <tbody>
                {% for label,value in chart.values %}
                    <tr>
                        <th scope="row">
                            {% with chart.answers[label] as text %}
                                {% if text %}
                                    {{ label }}) {{ text }}
                                {% else %}
                                    {{ label }}
                                {% endif %}
                            {% endwith %}
                        </th>
                        <td>
                            {{ value }}
                        </td>
                   </tr>
                {% endfor %}
                {% if chart.has_totals %}
                    <tr>
                        <th scope="row">{_ Totals _}</th>
                        <td>{{ chart.totals }}</td>
                    </tr>
                {% endif %}
                </tbody>
            </table>
        </div>
    </div>
{% endif %}
