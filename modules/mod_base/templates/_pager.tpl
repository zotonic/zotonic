<div class="pagination pagination-centered">
    <ul>
        <li><a href="{{ prev_url }}" {% if not prev_url %}class="disabled"{% endif %}>←</a></li>
        {% for nr, url in pages %}
            {% if nr %}
                <li><a href="{{ url }}" {% if nr == page %}class="active"{% endif %}>{{ nr }}</a></li>
            {% else %}
                <li><a href="#" class="disabled">…</a></li>
            {% endif %}
        {% endfor %}
        <li><a href="{{ next_url }}" {% if not next_url %}class="disabled"{% endif %}>→</a></li>
    </ul>
</div>
