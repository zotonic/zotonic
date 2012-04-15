{% if menu %}
{% with id|menu_trail:menu_id as parents %}
    <ul id="{{ id_prefix }}navigation" class="clearfix sf-menu do_superfish">
    {% for mid, path, action in menu %}
        {% if mid %}
        <li>
            <a href="{{ m.rsc[mid].page_url }}" 
               class="{{ m.rsc[mid].name }}{% if mid|member:parents %}{% if m.rsc[mid].name %} {% endif %}current{% endif %}">{{ m.rsc[mid].short_title|default:m.rsc[mid].title }}</a>
            {% if action == "down" %}
            <ul>
            {% else %}
            </li>
            {% endif %}
        {% else %}
        </ul></li>
        {% endif %}
    {% if forloop.last %}{% include "_menu_extra.tpl" %}{% endif %}
    {% endfor %}
    </ul>
{% endwith %}
{% endif %}
