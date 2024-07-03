{% if result %}
    <ul>
    {% for id in result %}
        <li class="rsc-edge">
            <a href="{% url admin_edit_rsc id=id %}" target="_blank">
                {{ m.rsc[id].title|default:_"<em>Untitled</em>" }}
                <span class="text-muted">
                    {{ id.category_id.title }}
                </span>
            </a>
        </li>
    {% endfor %}
    </ul>

    <p>
        {% if result.is_total_estimated %}{% trans "About {n} items found." n=result.total|round_significant:2 %}
        {% else %}{% trans "{n} items found." n=result.total %}
        {% endif %}
    </p>
{% else %}
    <p class="text-muted">{_ No results. _}</p>
{% endif %}
