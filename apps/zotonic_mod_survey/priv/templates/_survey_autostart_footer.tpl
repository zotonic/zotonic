{% block seealso %}
	{% include "_content_list.tpl" list=id.o.hasfeatured %}
    {% include "_content_list.tpl" list=id.o.haspart in_collection=id %}
    {% include "_content_list.tpl" list=id.o.relation %}
{% endblock %}

{% block thumbnails %}
    {% include "_page_thumbnails.tpl" %}
{% endblock %}

{% block sidebar_collection %}
	{% with m.rsc.sidebar_collection.id as id %}
	{% include "_content_list.tpl" list=id.o.haspart %}
	{% endwith %}
{% endblock %}
