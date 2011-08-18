{% if question.question %}
<p>{{ question.question|escape }}</p>
{% endif %}
{% with question.parts.options|randomize as options %}
{% with question.parts.items as items %}
	<ul class="matching">
	{% for n in items %}
		{% with forloop.counter as index %}
			<li>
				<label>
					{{ n }}
				</label>
				{% with [name, "_", index|make_list]|join as nm %}
				{% with answers[nm] as ans %}
				<select id="{{ #match.index }}" name="{{ nm }}">
					<option></option>
					{% for opt in options %}
						<option {% if ans == opt %}selected="selected" {% endif %}>{{ opt }}</option>
					{% endfor %}
				</select>
				{% if question.is_required %}{% validate id=#match.index name=nm type={presence} %}{% endif %}
				{% endwith %}
				{% endwith %}
			</li>
		{% endwith %}
	{% endfor %}
	</ul>
{% endwith %}
{% endwith %}
