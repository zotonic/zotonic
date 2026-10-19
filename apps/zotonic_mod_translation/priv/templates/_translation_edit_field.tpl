{# Override these blocks per site; editor selection never changes server sanitization. #}
{% if field.type == `html` %}
    {% block html %}
    <textarea id="{{ field_id }}" name="translation_text" class="form-control z_editor-init"
              rows="12" {% include "_language_attrs.tpl" language=language %}>{{ field.text|escape }}</textarea>
    {% endblock %}
{% elseif field.type == `input` %}
    {% block input %}
    <input id="{{ field_id }}" name="translation_text" class="form-control" type="text" autofocus
           value="{{ field.text|escape }}" {% include "_language_attrs.tpl" language=language %}>
    {% endblock %}
{% else %}
    {% block textarea %}
    <textarea id="{{ field_id }}" name="translation_text" class="form-control" rows="6" autofocus
              {% include "_language_attrs.tpl" language=language %}>{{ field.text|escape }}</textarea>
    {% endblock %}
{% endif %}
