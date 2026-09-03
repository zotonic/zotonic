{% if chart.type == "pie" %}
    <div class="graph clearfix">
        {% if chart.name %}
            <h4>{{ chart.name }}:</h4>
        {% endif %}

        <div class="pull-left clearfix">
            {% chart type="pie"
                     height=180
                     width=400
                     data=chart.values
                     title=chart.name|default:chart.question
                     aria_describedby=#chart_data
                     hide_table
            %}
        </div>

        <table id="{{ #chart_data }}" class="table table-compact pull-left" style="width:auto">
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
                    <th>{_ Totals _}</th>
                    <th>{{ chart.totals }}</th>
                </tr>
            {% endif %}
            </tbody>
        </table>
    </div>
{% endif %}
