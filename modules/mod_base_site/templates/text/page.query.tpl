{% extends "page.tpl" %}

{# text #}

{% block related %}
    {% with m.search.paged[{query query_id=id pagelen=10 page=q.page}] as result%}
    <div id="content-pager">
        {% include "_content_list.tpl" list=result %}
        
        {% pager id=id result=result %}
    </div>
    {% endwith %}
    
    {% include "_content_list.tpl" list=id.o.hasdocument title=_"Documents"%}
    {% include "_content_list.tpl" list=id.o.depiction title=_"Media"%}
    {% include "_content_list.tpl" list=id.s.haspart title=_"Related" %}
{% endblock %}
