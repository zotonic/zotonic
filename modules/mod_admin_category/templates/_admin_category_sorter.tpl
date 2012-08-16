{% with m.acl.is_admin as editable %}

<ul id="category" class="tree-list categories {% if editable %}do_menuedit{% endif %}" data-menuedit="connectWith: '#trash'">
    {% for mid, path, action in m.category.menu|menu_flat %}
    {% with forloop.counter as c %}
        {% include "_menu_edit_item.tpl" c=forloop.counter id=mid %}
    {% endwith %}
    {% endfor %}
</ul>
{% endwith %}

{% include "_menu_edit_scripts.tpl" menu_id="category" in_sorter="category" %}
