<ul id="hierarchy-{{ id.name }}" class="tree-list menu_hierarchy {% if editable %}do_menuedit{% endif %}">
{% for mid, path, action in m.hierarchy[id.name].menu_ensured|menu_flat %}
    {% with forloop.counter as c %}
        {% include "_menu_edit_item.tpl" c=forloop.counter id=mid editable=editable %}
    {% endwith %}
{% endfor %}
</ul>

{% include "_menu_edit_scripts.tpl" in_sorter="hierarchy-"++id.name menu_id=sorter is_hierarchy %}
