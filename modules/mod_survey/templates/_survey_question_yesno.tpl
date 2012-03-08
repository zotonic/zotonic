{% with answers[name]|default:value as value %}
<p>{{ question.question|escape }}</p>
<ul>
	<li><label class="choices"><input id="{{ #yes }}" name="{{ name }}" type="radio" value="1" {% if value == "1" %}checked="checked"{% endif %} /> {_ Yes _}</label></li>
	<li><label class="choices"><input id="{{ #no }}" name="{{ name }}" type="radio" value="0" {% if value == "0" %}checked="checked"{% endif %} /> {_ No _}</label></li>
</ul>
{% endwith %}
{% if question.is_required %}{% validate id=#yes name=name type={presence} %}{% endif %}
