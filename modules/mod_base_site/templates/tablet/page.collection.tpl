{% extends "page.tpl" %}

{% block main %}
    {% inherit %}
    {% include "_content_list.tpl" list=id.o.haspart %}
{% endblock %}

{% block related %}
    {% include "_content_list.tpl" list=id.o.hasdocument title=_"Documents"%}
    {% include "_content_list.tpl" list=id.o.depiction title=_"Media"%}
    {% include "_content_list.tpl" list=id.s.haspart title=_"Related" %}
{% endblock %}
