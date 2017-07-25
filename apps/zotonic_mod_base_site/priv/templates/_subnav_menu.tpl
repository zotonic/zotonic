{% with m.rsc[menu_id|default:`main_menu`] as r_menu %}
{% with r_menu.menu as in_menu %}
{% if in_menu %}
    {% with id|menu_trail:in_menu as breadcrumb %}
    {% with breadcrumb|length as n %}
    {% if n %}
        {% with id|menu_subtree:in_menu:(n > 1) as subtree %}
        {% if n > 1 or subtree %}
             <ul class="nav nav-tabs nav-stacked">
             {% if r_menu.name != 'main_menu' %}
                <li {% include "_language_attrs.tpl" id=r_menu.id %}>
                    <a class="item-1" href="{{ r_menu.id.page_url with in_menu=r_menu.id }}">{{ r_menu.id.short_title|default:(r_menu.id.title) }} <span class="divider">/</span></a>
                </li>
             {% endif %}
             {% for pid in breadcrumb %}
             {% if n == 1 or not forloop.last %}
                 <li {% include "_language_attrs.tpl" id=pid class=(pid==id)|if:"active":"" %}>
                     <a class="item-{{ forloop.counter+(item_offset|default_if_none:1) }} {% if forloop.last %}last{% endif %} {% if id==pid %}active{% endif %}" href="{{ pid.page_url with in_menu=r_menu.id }}">{{ pid.short_title|default:(pid.title) }} <span class="divider">/</span></a>
                </li>
             {% endif %}
             {% endfor %}
             {% if subtree %}
                {% for pid,m in subtree %}
                    <li {% include "_language_attrs.tpl" id=pid class=(pid==id)|if:"active":"" %}>
                        <a class="item-{{(n==1)|if:(n+1):n}}" href="{{ pid.page_url with in_menu=r_menu.id }}">{{ pid.short_title|default:(pid.title) }}</a>
                    </li>
                    {% if n > 1%}
                    {% for sid,_ in m %}
                        <li {% include "_language_attrs.tpl" id=sid class=(sid==id)|if:"active":"" %}>
                            <a class="item-{{n+1}}" href="{{ sid.page_url with in_menu=r_menu.id }}">{{ sid.short_title|default:(sid.title) }}</a>
                        </li>
                    {% endfor %}
                    {% endif %}
                {% endfor %}
             {% endif %}
             </ul>
        {% endif %}
        {% endwith %}
    {% endif %}
    {% endwith %}
    {% endwith %}
{% endif %}
{% endwith %}
{% endwith %}
