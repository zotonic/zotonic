{% if chart.type == "pie" %}
    <div class="graph clearfix">
        {% if chart.name %}
            <h4>{{ chart.name }}:</h4>
        {% endif %}

        <div class="pull-left clearfix">
            {% chart_pie3d height=100 width=400 data=chart.data %}
        </div>

        <table class="table table-compact pull-left" style="width:auto">
            {% for label,value in chart.values %}
                <tr>
                    <td>
                        {% with chart.answers[label] as text %}
                            {% if text %}
                                {{ label }}) {{ text }}
                            {% else %}
                                {{ label }}
                            {% endif %}
                        {% endwith %}
                    </td>
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
        </table>
    </div>
{% endif %}
