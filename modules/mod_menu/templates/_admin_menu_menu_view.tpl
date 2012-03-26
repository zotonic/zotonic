<h3 class="above-list">{_ Current menu _}</h3>

<ul class="tree-list do_menuedit" id="menu-{{ id }}" data-menuedit="connectWith: '#trash'">
    {% for mid, path, action in id.menu|menu_flat %}
    {% with forloop.counter as c %}
    {% if mid %}
    <li id="{{ #menu.c }}-{{ mid }}">
        {% include "_menu_edit_item.tpl" id=mid %}
        {% if action == `down` %}
        <ul>
            {% else %}
        </li>
        {% endif %}
        {% else %}
    </ul></li>
    {% endif %}
    {% endwith %}
    {% endfor %}
</ul>
