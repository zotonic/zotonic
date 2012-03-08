{% with answers[question.id][question.name][2]|default:value  as value %}
<p>{{ question.question|escape }}</p>
<input id="{{ #short }}" name="{{ question.name }}" type="text" value="{{ value|escape }}" />
{% if question.name == "email" %}
  {% if question.is_required %}
    {% validate id=#short name=question.name type={presence} type={email} %}
  {% else %}
    {% validate id=#short name=question.name type={email} %}
  {% endif %}
{% else %}
  {% if question.is_required %}
    {% validate id=#short name=question.name type={presence} %}
  {% endif %}
{% endif %}

{% endwith %}
