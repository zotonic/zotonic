{% for id, rank in result %}
	<li class="suggestions-result clearfix">
		{% with m.rsc[id] as r %}
		<a id="{{ #connect.id }}" href="#add-connection">
            {% image r.depiction width=40 height=18 crop %} 
            {{ r.title }}
            <span>&mdash; {{ m.rsc[r.category.id].title|default:r.category.name }}</span>
        </a>
		{% endwith %}
	</li>

	{% wire_args id=#connect.id action=action_with_id select_id=id %}
	{% wire id=#connect.id action=action %}
{% empty %}
	<li class="suggestions-result"><a href="javascript:void(0);">Nothing found.</a></li>
{% endfor %}
