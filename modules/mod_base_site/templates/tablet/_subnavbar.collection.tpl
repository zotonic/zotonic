{% include "_subnav.tpl" %}

{% include "_content_list.tpl" list=id.o.hasfeatured %}
{% if id.body %}
    {% include "_content_list.tpl" list=id.o.haspart in_collection=id %}
{% endif %}
{% include "_content_list.tpl" list=id.o.relation %}
