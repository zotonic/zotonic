{% optional include "_meta.tpl" %}

{% optional include "_address.tpl" %}

{% block subnav %}
	{% optional include "_subnav.tpl" %}
{% endblock %}

<div class="body">
	{{ id.body|show_media }}
</div>
