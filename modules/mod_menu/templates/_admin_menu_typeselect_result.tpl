{% for id, rank in result %}

	<li>
		{% with m.rsc[id] as r %}
			<a id="{{ #connect.id }}" href="#drag-item">{{ r.title }} (in <em>{{ m.rsc[r.category_id].title }})</em></a>
		{% endwith %}
		{% draggable id=#connect.id clone tag=["new", id] %}
	</li>

{% empty %}

	<li class="suggestions-result"><a href="#">Nothing found.</a></li>

{% endfor %}
