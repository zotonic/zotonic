{% extends "page.tpl" %}

{% block main %}
{% inherit %}
{% with m.search.paged[{query query_id=id pagelen=20 page=q.page}] as result%}
    {% include "_content_list.tpl" list=result is_large %}
    {% pager id=id result=result in_collection=q.in_collection %}
{% endwith %}

    {% if not id.body %}
        {% include "_page_depiction.tpl" %}
    {% endif %}
    {% include "_page_thumbnails.tpl" %}
{% endblock %}

{% block depiction %}
{% if id.body %}
	{% inherit %}
{% endif %}
{% endblock %}

{% block thumbnails %}
{% endblock %}

