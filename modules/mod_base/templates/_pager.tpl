<div class="pagination pagination-centered">
    <ul>
        <li {% if not prev_url %}class="disabled"{% endif %}><a href="{{ prev_url }}#content-pager">←</a></li>
        {% for nr, url in pages %}
            {% if nr %}
                <li {% if nr == page %}class="active"{% endif %}><a href="{{ url }}#content-pager">{{ nr }}</a></li>
            {% else %}
                <li class="disabled"><a href="#">…</a></li>
            {% endif %}
        {% endfor %}
        <li {% if not next_url %}class="disabled"{% endif %}><a href="{{ next_url }}#content-pager">→</a></li>
    </ul>
</div>
