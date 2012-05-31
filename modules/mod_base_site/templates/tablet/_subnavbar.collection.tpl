{# featured stuff #}
{% include "_content_list.tpl" list=id.o.hasfeatured %}

{# prev/next in collection and optional sub menu #}
{% include "_subnav.tpl" %}

{% if id.body %}
    {# The current id's collection, only when it is not shown in the main content #}
    {% include "_content_list.tpl" list=m.rsc.o.haspart in_collection=id %}

    {% with id.s.haspart as s_haspart %}
        {# The collection the id is part of #}
        {% with q.in_collection|default:s_haspart[1] as in_coll %}
            {% if in_coll %}
                {% include "_content_list.tpl" list=m.rsc[in_coll].o.haspart %}
                {% if s_haspart|length > 1 %}
                {% include "_content_list.tpl" list=s_haspart exclude=m.rsc[q.in_collection].id %}
                {% endif %}
            {% endif %}
        {% endwith %}
    {% endwith %}

{% else %}

    {% with id.s.haspart as s_haspart %}
        {# The collection the id is part of #}
        {% with q.in_collection|default:s_haspart[1] as in_coll %}
            {% if in_coll %}
                {% include "_content_list.tpl" list=m.rsc[in_coll].o.haspart %}
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


