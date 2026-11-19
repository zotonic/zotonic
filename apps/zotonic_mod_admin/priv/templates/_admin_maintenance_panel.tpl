{# Shared wrapper for module-owned maintenance tools. #}
<section class="widget" {% if panel_id %}id="{{ panel_id|escape }}"{% endif %}>
    <h3 class="widget-header">{{ title|escape }} <small>{{ scope|default:_"This site"|escape }}</small></h3>
    <div class="widget-content">{% block tools %}{% endblock %}</div>
</section>
