{# Parent template for admin edit widgets. #}

{% block widget_before %}{% endblock %}

<div class="item-wrapper" id="{% block widget_id %}{% endblock %}">
	{% if headline == "simple" %}
		<h3 class="above-item {% block widget_headline_class %}{% endblock %}">{% block widget_title %}{% endblock %}</h3>
	{% endif %}

	{% if headline == "normal" or not headline %}
		<h3 class="above-item clearfix do_blockminifier 
			{% if show_minimized %}above-item-all-corners{% endif %} 
			{% block widget_headline_class %}{% endblock %}"
			{% if show_minimized %}data-blockminifier="minifiedOnInit: true"{% endif %}>

			<span class="title">{% block widget_title %}{% endblock %}</span>
			<span class="arrow">{_ make smaller _}</span>
		</h3>
	{% endif %}

	<div class="item {{ content_class|default_if_undefined:"clearfix"}}">
		{% block widget_content %}{% endblock %}
	</div>
</div>

{% block widget_after %}{% endblock %}
