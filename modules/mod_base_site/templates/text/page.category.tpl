{% extends "page.tpl" %}

{% block related %}
	{% with m.search.paged[{latest cat=id.name pagelen=20 page=q.page}] as result %}
		{% include "_content_list.tpl" list=result %}
		{% pager id=id result=result page=q.page %}
	{% endwith %}
{% endblock %}
