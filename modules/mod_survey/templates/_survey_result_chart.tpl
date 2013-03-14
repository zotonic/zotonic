{% if chart.type == "pie" %}
    <div class="graph">

        {% if chart.name %}
            <h4>{{ chart.name }}:</h4>

        {% endif %}

	    {% chart_pie3d height=100 width=400 data=chart.data %}
	    
	    <div class="values">
		    <table>
		        {% for label,value in chart.values %}
			        <tr><th>{{ label }}</th><td>{{ value }}</td></tr>
		        {% endfor %}
                {% if chart.has_totals %}
                    <tr><th>{_ Totals _}</th><td>{{ chart.totals }}</td></tr>
                {% endif %}
		    </table>
	    </div>
    </div>
{% endif %}
