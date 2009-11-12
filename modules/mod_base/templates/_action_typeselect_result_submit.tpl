{% for id, rank in result %}
	<li class="suggestions-result">
		{% if forloop.first %}
			<input id="{{ #typeselect }}" type="hidden" name="typeselect_id" />
		{% endif %}
		
		{% with m.rsc[id] as r %}
		<a id="{{ #connect.id }}" href="#add-connection">{{ r.title }} (in <span>{{ r.category.title|default:r.category.name }})</span></a>
		{% endwith %}
	</li>

	{% wire id=#connect.id 
			action={set_value id=#typeselect value=id}
			action={submit closest=#typeselect}
	%}

{% empty %}
	<li class="suggestions-result"><a href="javascript:void(0);">Nothing found.</a></li>
{% endfor %}
