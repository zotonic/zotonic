<p>{{ question.question|escape }}</p>
<input id="{{ #short }}" name="{{ question.name }}" type="text" value="{{ answers[question.name]|escape }}" />
{% if question.is_required %}{% validate id=#short name=question.name type={presence} %}{% endif %}
{% if question.name == "email" %}{% validate id=#short name=question.name type={email} %}{% endif %}
