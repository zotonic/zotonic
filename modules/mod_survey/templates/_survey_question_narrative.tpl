<p>
	{% for type,name,value in question.parts %}
	{% with forloop.counter, answers[name] as index, ans %}
		{% if type == "html" %}
			{{ value }}
		{% endif %}
		{% if type == "input" %}
			<input id="{{ #inp.index }}" name="{{ name }}" length="{{ value }}" style="width: {{ value }}em" value="{{ ans|escape }}" />
    {% if question.is_required %}{% validate id=#inp.index name=name type={presence} %}{% endif %}
		{% endif %}
		{% if type == "select" %}
			<select id="{{ #sel.index }}" name="{{ name }}">
				{% for p in value %}
					<option {% if p == "" %}disabled="disabled"{% else %}{% if ans == p %}selected="selected"{% endif %}{% endif %}>{{ p|escape }}</option>
				{% endfor %}
			</select>
    {% if question.is_required %}{% validate id=#sel.index name=name type={presence} %}{% endif %}
		{% endif %}
	{% endwith %}
	{% endfor %}
</p>
