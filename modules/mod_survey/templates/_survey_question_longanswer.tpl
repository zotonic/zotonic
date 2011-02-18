<p>{{ question.question|escape }}</p>
<textarea id="{{ #long }}" name="{{ question.name }}">{{ answers[question.name]|escape }}</textarea>

{% validate id=#long name=question.name type={presence} %}
