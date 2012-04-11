{# Parent template for admin edit widgets. #}

{% block widget_before %}{% endblock %}

<div id="{% block widget_id %}{% endblock %}" class="{% if in_dialog %}dialog-{% endif %}widget {% block widget_class %}{% endblock %} do_adminwidget" data-adminwidget="minifiedOnInit: {% block widget_show_minimized %}false{% endblock %}">
    {% if not in_dialog %}
    <h3 class="widget-header">
        {% block widget_title %}{% endblock %}
    </h3>
    {% endif %}
    <div class="widget-content">
	{% block widget_content %}{% endblock %}
    </div>
</div>

{% block widget_after %}{% endblock %}
