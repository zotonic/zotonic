{% extends "page.tpl" %}

{% block below_summary %}
{% if m.rsc[id].medium %}
	<figure class="image-wrapper block-level-image">
		{% media m.rsc[id].medium width=445 crop class=align alt=m.rsc[id].title %}
	</figure>
{% endif %}
{% endblock %}
