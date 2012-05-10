{% if id.is_a.menu %}
    <ul class="nav nav-tabs nav-stacked">
    {% for pid,_sub in id.menu %}
        <li><a href="{% url page id=pid in_menu=id %}">{{ pid.short_title|default:(pid.title) }}</a></li>
    {% endfor %}
    </ul>
{% elseif q.in_menu %}
    {% include "_subnav_menu.tpl" menu_id=m.rsc[q.in_menu].id %}
{% endif %}
