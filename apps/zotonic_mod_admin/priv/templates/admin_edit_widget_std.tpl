{# Parent template for admin edit widgets. #}

{% block widget_wrapper %}
{% block widget_before %}{% endblock %}

<div id="{% block widget_id %}{% endblock %}" class="widget {% if in_dialog %}dialog-widget{% endif %} do_adminwidget {% block widget_class %}{% endblock %}" data-adminwidget='{ "minifiedOnInit": {% block widget_show_minimized %}false{% endblock %}, "minifier": {% if show_opened or in_dialog or noheader %}false{% else %}true{% endif %} }'>
    {% block widget_header %}
        {% if not in_dialog and not noheader %}
            <div class="widget-header">
                {% block widget_title %}<div class="widget-header-tools"></div>{% endblock %}
            </div>
        {% endif %}
    {% endblock %}
    <div class="widget-content">
        {% block widget_content_nolang_before %}{% endblock %}
    	{% block widget_content %}{% endblock %}
        {% block widget_content_nolang %}{% endblock %}
    </div>
</div>


{% block widget_after %}{% endblock %}

{% endblock %}
