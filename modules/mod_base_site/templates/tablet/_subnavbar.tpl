{# featured stuff #}
{# prev/next in collection and optional sub menu #}
{% include "_subnav.tpl" %}

{% include "_content_list.tpl" list=id.o.hasfeatured %}

{% with id.o.haspart as o_haspart %}
{% with id.s.haspart as s_haspart %}

    {# The current id's collection #}
    {% include "_content_list.tpl" list=o_haspart in_collection=id %}

    {# The collection the id is part of #}
    {% with q.in_collection|default:s_haspart[1] as in_coll %}
        {% if in_coll %}
            {% include "_content_list.tpl" list=m.rsc[in_coll].o.haspart exclude=id in_collection=in_coll %}
            {% if s_haspart|length > 1 %}
            {% include "_content_list.tpl" list=s_haspart exclude=m.rsc[q.in_collection].id %}
            {% endif %}
        {% endif %}
    {% endwith %}

    {% if not o_haspart and not s_haspart %}
    	{% include "_content_list.tpl" list=m.search[{query pagelen=10 cat=`tweet` cat=`news` sort="-rsc.publication_start"}] %}
    {% endif %}

{% endwith %}
{% endwith %}
