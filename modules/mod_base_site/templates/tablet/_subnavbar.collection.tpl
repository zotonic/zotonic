{# prev/next in collection and optional sub menu #}
{% include "_subnav.tpl" %}

{# featured stuff #}
{% include "_content_list.tpl" list=id.o.hasfeatured %}

{% if id.body %}

	{% include "_content_list.tpl" list=id.o.haspart in_collection=id %}

{% else %}

    {% with id.s.haspart as s_haspart %}
        {# The collection the id is part of #}
        {% with q.in_collection|default:s_haspart[1] as in_coll %}
            {% if in_coll %}
                {% include "_content_list.tpl" list=m.rsc[in_coll].o.haspart exclude=id in_collection=in_coll %}
                {% if s_haspart|length > 1 %}
                {% include "_content_list.tpl" list=s_haspart exclude=m.rsc[q.in_collection].id %}
                {% endif %}
            {% endif %}
        {% endwith %}
        
        {% if not s_haspart %}
            {% include "_content_list.tpl" list=m.search[{query pagelen=10 cat=id.category.name cat_exclude=`tweet` sort="-rsc.publication_start"}] %}
        {% endif %}
    {% endwith %}

    
{% endif %}


