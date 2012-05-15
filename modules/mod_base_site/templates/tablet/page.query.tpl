{% extends "page.tpl" %}

{% block main %}
    {% inherit %}
    {% with m.search.paged[{query query_id=id pagelen=20 page=q.page}] as result%}
    <div id="content-pager">
        {% include "_content_list.tpl" list=result %}
    
        {% pager id=id result=result in_collection=q.in_collection %}
    </div>
    {% endwith %}
{% endblock %}
