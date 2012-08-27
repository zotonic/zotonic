{% if blk.explanation %}
     <p class="help-block">{{ blk.explanation }}</p>
{% endif %}
<p class="survey-narrative question-{{ nr }}">
{% with blk|survey_prepare_narrative as props %}
{% for type,name,value in props.parts %}
	{% with forloop.counter, answers[name] as index, ans %}
		{% if type == `html` %}
			{{ value }}
		{% elseif type == `input` %}
			<input id="{{ #inp.index }}" name="{{ name }}" length="{{ value }}" style="width: {{ value }}em" value="{{ ans|escape }}" />
            {% if blk.is_required %}{% validate id=#inp.index name=name type={presence} %}{% endif %}
		{% elseif type == `select` %}
			<select id="{{ #sel.index }}" name="{{ name }}">
				{% for v,p in value %}
					<option {% if v == "" %}disabled="disabled"{% else %}{% if ans == v %}selected="selected"{% endif %}{% endif %} value="{{v|escape}}">
					    {{ p|escape }}
					</option>
				{% endfor %}
			</select>
            {% if blk.is_required %}{% validate id=#sel.index name=name type={presence} %}{% endif %}
		{% endif %}
	{% endwith %}
{% endfor %}
{% endwith %}
</p>
