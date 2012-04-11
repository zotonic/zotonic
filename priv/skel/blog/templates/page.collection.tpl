{% extends "page.tpl" %}

{% block below_body %}
<div id="list-articles">
	{% for id in m.rsc[id].o.haspart %}
		{% include "_article_summary.tpl" id=id %}
	{% endfor %}
</div>
{% endblock %}
