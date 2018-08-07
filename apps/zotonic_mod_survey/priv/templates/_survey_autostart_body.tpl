{% include "_meta.tpl" %}

{% include "_address.tpl" %}

{% block subnav %}
	{% include "_subnav.tpl" %}
{% endblock %}

<div class="body">
	{{ id.body|show_media }}
</div>
