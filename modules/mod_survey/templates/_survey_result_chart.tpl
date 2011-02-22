{% if chart.type == "pie" %}
<div class="graph">
	{% chart_pie3d height=100 width=400 data=chart.data %}
	
	<div class="values">
		<table>
		{% for label,value in chart.values %}
			<tr><th>{{ label }}</th><td>{{ value }}</td></tr>
		{% endfor %}
		</table>
	</div>
</div>
{% endif %}
