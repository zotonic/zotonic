{# Sidebar sub navigation for tablet+ #}

{% with q.in_collection|default:id.s.haspart[1] as in_collection %}
{% if in_collection %}
{% with m.rsc[in_collection].o.haspart as ps %}
    {% for p in ps %}
    {% if p == id %}
        {% with ps[forloop.counter-1], ps[forloop.counter+1] as prev, next %}
        <ul class="pager-collection">
            <li class="up"><a href="{{ m.rsc[in_collection].page_url }}">{{ m.rsc[in_collection].title }}</a></li>
            {% if prev %}<li class="previous"><a href="{% url page id=prev in_collection=in_collection %}">&larr; {_ Previous _}</a></li>{% endif %}
            {% if next %}<li class="next"><a href="{% url page id=next in_collection=in_collection %}">{_ Next _} &rarr;</a></li>{% endif %}
        </ul>
        {% endwith %}
    {% endif %}
    {% endfor %}
{% endwith %}
{% endif %}
{% endwith %}


{% if q.in_menu and q.in_menu != id and q.in_menu != m.rsc.main_menu.id %}
    {% include "_subnav_menu.tpl" menu_id=m.rsc[q.in_menu].id %}
{% endif %}

{% if id.is_a.menu %}
    <ul class="nav nav-tabs nav-stacked">
    {% for pid,_sub in id.menu %}
        <li><a href="{{ pid.page_url with in_menu=id }}">{{ pid.short_title|default:(pid.title) }}</a></li>
    {% endfor %}
    </ul>
{% endif %}

