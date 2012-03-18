{# Parent template for admin edit widgets. #}

{% block widget_before %}{% endblock %}

<div id="{% block widget_id %}{% endblock %}" class="widget {% block widget_class %}{% endblock %} do_adminwidget" data-adminwidget="minifiedOnInit: {% block widget_show_minimized %}false{% endblock %}">
    <h3 class="{% block widget_headline_class %}{% endblock %}">{% block widget_title %}{% endblock %}</h3>

    <div class="widget-content">
	{% block widget_content %}{% endblock %}
    </div>
</div>

{% block widget_after %}{% endblock %}
