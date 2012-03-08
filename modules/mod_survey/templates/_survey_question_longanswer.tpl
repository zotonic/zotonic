{% with answers[question.name]|default:value as value %}
<p>{{ question.question|escape }}</p>
<textarea id="{{ #long }}" name="{{ question.name }}">{{ value|escape }}</textarea>
{% endwith %}
{% if question.is_required %}{% validate id=#long name=question.name type={presence} %}{% endif %}
