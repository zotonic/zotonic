{% block seealso %}
	{% optional include "_content_list.tpl" list=id.o.hasfeatured %}
    {% optional include "_content_list.tpl" list=id.o.haspart in_collection=id %}
    {% optional include "_content_list.tpl" list=id.o.relation %}
{% endblock %}

{% block thumbnails %}
    {% optional include "_page_thumbnails.tpl" %}
{% endblock %}

{% block sidebar_collection %}
	{% with m.rsc.sidebar_collection.id as id %}
	{% optional include "_content_list.tpl" list=id.o.haspart %}
	{% endwith %}
{% endblock %}
