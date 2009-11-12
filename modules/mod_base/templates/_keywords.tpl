{% if m.edge.o[id].subject %}
	<h4>keywords</h4>
	<ul class="list">
		{% for keyword_id, edge_id in m.edge.o[id].subject %}
			<li>{{ m.rsc[keyword_id].title }}{% if not forloop.last %},{% endif %}</li>
		{% endfor %}
	</ul>
{% endif %}