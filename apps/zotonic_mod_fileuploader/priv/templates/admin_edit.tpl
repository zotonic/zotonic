{% overrules %}

{% block bodyattr %}
    {% inherit %}
    {% if id.is_a.media %}data-fileuploader="{{ %{ id: id.id, is_medium: true }|escape }}"
    {% else %}data-fileuploader="{{ %{ subject_id: id.id, predicate: "depiction" }|escape }}"
    {% endif %}
{% endblock %}