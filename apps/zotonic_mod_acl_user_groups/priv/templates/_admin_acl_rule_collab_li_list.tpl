{% if text %}
    {% for id in m.search[{query cat=cat text=text pagelen=10}] %}
        <li>
            <a href="#" id="{{ #cg.id }}">{{ id.title }}</a>
        </li>
        {% wire id=#cg.id
                postback={collab_select id=id}
                delegate=`admin_acl_rules`
                action={set_value target="dialog-collab-search" value=""}
        %}
    {% endfor %}
{% endif %}
