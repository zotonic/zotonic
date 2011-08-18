{% with answers[name] as ans %}
<p>{{ question.question|escape }}</p>
<ul>
	<li class="first">{_ Strongly Disagree _}</li>
	<li><label class="choices"><input id="{{ #q1 }}" name="{{ name }}" type="radio" value="1" {% if ans == "1" %}checked="checked"{% endif %} /> 1</label></li>
	<li><label class="choices"><input id="{{ #q2 }}" name="{{ name }}" type="radio" value="2" {% if ans == "2" %}checked="checked"{% endif %} /> 2</label></li>
	<li><label class="choices"><input id="{{ #q3 }}" name="{{ name }}" type="radio" value="3" {% if ans == "3" %}checked="checked"{% endif %} /> 3</label></li>
	<li><label class="choices"><input id="{{ #q4 }}" name="{{ name }}" type="radio" value="4" {% if ans == "4" %}checked="checked"{% endif %} /> 4</label></li>
	<li><label class="choices"><input id="{{ #q5 }}" name="{{ name }}" type="radio" value="5" {% if ans == "5" %}checked="checked"{% endif %} /> 5</label></li>
	<li class="first">{_ Strongly Agree _}</li>
</ul>
{% endwith %}
{% if question.is_required %}{% validate id=#q1 name=name type={presence} %}{% endif %}

