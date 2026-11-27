{# A labelled operation with controls followed by its explanation. #}
<div class="admin-maintenance-action">
    {% if title %}<h4 class="admin-maintenance-title">{{ title|escape }}{% if scope %} <small>{{ scope|escape }}</small>{% endif %}</h4>{% endif %}
    <div class="admin-maintenance-controls">
        {% block action %}{% endblock %}
    </div>
    <div class="admin-maintenance-explanation">
        <p class="help-block">{{ description|escape }}</p>
        {% block status %}{% endblock %}
    </div>
</div>
