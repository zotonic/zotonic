{% if id.is_a.menu %}
    <ul class="nav nav-tabs nav-stacked">
    {% for pid,_sub in id.menu %}
        <li><a href="{% url page id=pid in_menu=id slug=id.slug %}">{{ pid.short_title|default:(pid.title) }}</a></li>
    {% endfor %}
    </ul>
{% elseif q.in_menu %}
    {% include "_subnav_menu.tpl" menu_id=m.rsc[q.in_menu].id %}
{% else %}
    {% include "_subnav_menu.tpl" menu_id=m.rsc.main_menu.id item_offset=0 %}
{% endif %}

