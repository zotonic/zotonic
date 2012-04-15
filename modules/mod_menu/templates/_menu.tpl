{% if menu %}
{% with id|menu_trail:menu_id as parents %}
    <ul id="{{ id_prefix }}navigation" class="nav">
    {% for mid, path, action in menu %}

    {% if mid and action == "down" %}
    <li id="{{ #nav.mid }}" class="dropdown"{% if mid|member:parents %} class="active"{% endif %}>
        <a class="{{ m.rsc[mid].name }}" href="{{ m.rsc[mid].page_url }}">
            {{ mid.short_title|default:mid.title }}
            <b class="caret"></b>
        </a>
        <ul class="dropdown-menu">
    {% elseif mid %}
    <li{% if mid|member:parents %} class="active"{% endif %}>
        <a href="{{ m.rsc[mid].page_url }}"
           class="{{ m.rsc[mid].name }}">{{ m.rsc[mid].short_title|default:m.rsc[mid].title }}</a>
    </li>
    {% else %}
    </ul></li>
    {% endif %}
    {% if forloop.last %}{% include "_menu_extra.tpl" %}{% endif %}
    {% endfor %}
    </ul>
{% endwith %}
{% endif %}
