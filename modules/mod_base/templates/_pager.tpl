<div class="pagination pagination-centered">
    <ul>
        <li {% if not prev_url %}class="disabled"{% endif %}><a href="{{ prev_url }}">←</a></li>
        {% for nr, url in pages %}
            {% if nr %}
                <li {% if nr == page %}class="active"{% endif %}><a href="{{ url }}">{{ nr }}</a></li>
            {% else %}
                <li class="disabled"><a href="#">…</a></li>
            {% endif %}
        {% endfor %}
        <li {% if not next_url %}class="disabled"{% endif %}><a href="{{ next_url }}">→</a></li>
    </ul>
</div>
