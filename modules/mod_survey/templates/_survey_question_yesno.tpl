<p>{{ question.question|escape }}</p>
<ul>
	<li><label class="choices"><input id="{{ #yes }}" name="{{ name }}" type="radio" value="1" {% if answers[name] == "1" %}checked="checked"{% endif %} /> {_ Yes _}</label></li>
	<li><label class="choices"><input id="{{ #no }}" name="{{ name }}" type="radio" value="0" {% if answers[name] == "0" %}checked="checked"{% endif %} /> {_ No _}</label></li>
</ul>
{% if question.is_required %}{% validate id=#yes name=name type={presence} %}{% endif %}
