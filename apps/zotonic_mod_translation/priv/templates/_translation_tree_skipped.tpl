<h4>{_ Skipped pages _}</h4>
<ul>
    {% for id in ids %}
        {% if id.is_visible %}
        <li>
            <a href="{% url admin_edit_rsc id=id %}" target="_blank" rel="noopener">
                {{ id.title|default:_"Untitled" }} ({{ id }})
                <span class="fa fa-external-link" aria-hidden="true"></span>
                <span class="sr-only">{_ Opens in a new tab _}</span>
            </a>
        </li>
        {% endif %}
    {% endfor %}
</ul>
