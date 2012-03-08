{% with answers[name]|default:value as value %}
{% if question.question %}
<p>{{ question.question|escape }}</p>
{% endif %}
<ul>
{% for n in question.parts %}
	{% with forloop.counter as index %}
		<li><label class="choices"><input id="{{ #thur.index }}" name="{{ name }}" type="radio" value="{{ index }}" {% if value == index %}checked="checked" {% endif %}/> {{ n }}</label></li>
		{% if forloop.first %}
            {% if question.is_required %}{% validate id=#thur.index name=name type={presence} %}{% endif %}
		{% endif %}
	{% endwith %}
{% endfor %}
</ul>
{% endwith %}
