{% extends "page.tpl" %}

{% block below_body %}
	{% with m.search.paged[{latest cat=id.name pagelen=20 page=q.page}] as result %}
		{% include "_content_list.tpl" list=result %}
		{% pager id=id result=result page=q.page %}
	{% endwith %}
{% endblock %}
