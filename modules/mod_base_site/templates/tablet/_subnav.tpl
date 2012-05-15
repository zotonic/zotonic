{# Sidebar sub navigation for tablet+ #}
{% include "_subnav_menu.tpl" item_offset=0 %}

{% if q.in_menu and q.in_menu != id and q.in_menu != m.rsc.main_menu.id %}
    {% include "_subnav_menu.tpl" menu_id=m.rsc[q.in_menu].id %}
{% endif %}

{% if id.is_a.menu %}
    <ul class="nav nav-tabs nav-stacked">
    {% for pid,_sub in id.menu %}
        <li><a href="{% url page id=pid in_menu=id slug=pid.slug %}">{{ pid.short_title|default:(pid.title) }}</a></li>
    {% endfor %}
    </ul>
{% endif %}
