<p>{{ question.question|escape }}</p>
<input id="{{ #short }}" name="{{ question.name }}" type="text" value="{{ answers[question.name]|escape }}" />

{% validate id=#short name=question.name type={presence} %}
