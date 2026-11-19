{# Render subgroups as section headings within the same dropdown. #}
{% for id, item in items|admin_menu_items %}
    {% if item.separator %}
        <li class="divider"></li>
    {% elseif item.header %}
        <li class="dropdown-header">
            {% if item.icon %}<i class="glyphicon glyphicon-{{ item.icon|escape }}"></i>{% endif %}
            {{ item.label|escape_check }}
        </li>
    {% else %}
        <li>
            <a href="{{ item.url|escape }}">
                {% if item.icon %}<i class="glyphicon glyphicon-{{ item.icon|escape }}"></i>{% endif %}
                {{ item.label|escape_check }}
            </a>
        </li>
    {% endif %}
{% endfor %}
