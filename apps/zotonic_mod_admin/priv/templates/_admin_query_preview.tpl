{% if result %}
    <ul>
    {% for id in result %}
        <li class="rsc-edge">
            {{ m.rsc[id].title|default:_"<em>Untitled</em>" }}
            <span class="text-muted">
                {{ id.category_id.title }}
            </span>
        </li>
    {% endfor %}
    </ul>
{% else %}
    <p class="text-muted">{_ No results. _}</p>
{% endif %}
