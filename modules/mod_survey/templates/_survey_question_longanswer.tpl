<p>{{ question.question|escape }}</p>
<textarea id="{{ #long }}" name="{{ question.name }}">{{ answers[question.name]|escape }}</textarea>

{% if question.is_required %}{% validate id=#long name=question.name type={presence} %}{% endif %}
