{% for id, rank in result %}
	<li class="suggestions-result">
		{% if forloop.first %}
			<input id="{{ #typeselect }}" type="hidden" name="typeselect_id" />
		{% endif %}

		<a id="{{ #connect.id }}" href="#add-connection">{{ id.title }} (in <span>{{ id.category.title|default:id.category.name }})</span></a>
	</li>

	{% wire id=#connect.id
			action={set_value id=#typeselect value=id}
			action={submit closest=#typeselect}
	%}

{% empty %}
	<li class="suggestions-result"><a href="#">Nothing found.</a></li>
{% endfor %}
