{% with m.rsc[menu_id] as r_menu %}
{% with r_menu.menu as in_menu %}
{% if in_menu %}
    {% with id|menu_trail:in_menu as breadcrumb %}
    {% if breadcrumb %}
     <ul class="nav nav-tabs nav-stacked breadcrumb">
     {% if not main_menu %}
        <li {% include "_language_attrs.tpl" id=r_menu.id %}>
            <a class="item-1" href="{% url page id=r_menu.id %}">{{ r_menu.id.short_title|default:(r_menu.id.title) }} <span class="divider">/</span></a>
        </li>
     {% endif %}
     {% for pid in breadcrumb %}
         <li {% include "_language_attrs.tpl" id=pid %}>
             <a class="item-{{ forloop.counter+1 }} {% if forloop.last %}last{% endif %}" href="{% url page id=pid in_menu=r_menu.id %}">{{ pid.short_title|default:(pid.title) }} <span class="divider">/</span></a>
        </li>
     {% endfor %}
     {% with id|menu_subtree:in_menu as subtree %}
     {% if subtree %}
        {% with breadcrumb|length+2 as n %}
        {% for pid,_m in subtree %}
            <li {% include "_language_attrs.tpl" id=pid %}><a class="item-{{n}}" href="{% url page id=pid in_menu=r_menu.id %}">{{ pid.short_title|default:(pid.title) }}</a></li>
        {% endfor %}
        {% endwith %}
     {% endif %}
     {% endwith %}
     </ul>
    {% endif %}
    {% endwith %}
{% endif %}
{% endwith %}
{% endwith %}
