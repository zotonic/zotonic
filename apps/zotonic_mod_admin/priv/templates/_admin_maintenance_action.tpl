{# A labelled operation with its explanation and controls in one responsive row. #}
<div class="row form-group">
    <div class="col-md-8">
        <h4>{{ title|escape }}</h4>
        <p class="help-block">{{ description|escape }}</p>
    </div>
    <div class="col-md-4 z-button-row">
        {% block action %}{% endblock %}
    </div>
</div>
