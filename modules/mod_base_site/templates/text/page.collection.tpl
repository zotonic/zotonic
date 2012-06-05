{% extends "page.tpl" %}

{% block related %}
    {% include "_content_list.tpl" list=id.o.haspart in_collection=id %}

    {% include "_content_list.tpl" list=id.o.hasdocument title=_"Documents"%}
    {% include "_content_list.tpl" list=id.o.depiction title=_"Media"%}
    {% include "_content_list.tpl" list=id.s.haspart title=_"Related" %}
{% endblock %}
