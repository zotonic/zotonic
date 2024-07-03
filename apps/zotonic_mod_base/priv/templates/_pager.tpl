<ul class="pagination pagination-centered" {% if topic %}data-onclick-topic="{{ topic|escape }}"{% endif %}>
    <li {% if not prev_url %}class="disabled"{% endif %}><a href="{{ prev_url }}{{ hash }}">←</a></li>
    {% for nr, url in pages %}
        {% if nr %}
            <li {% if nr == page %}class="active"{% endif %}><a href="{{ url }}{{ hash }}">{{ nr }}</a></li>
        {% else %}
            <li><a href="{{ url }}{{ hash }}">…</a></li>
        {% endif %}
    {% endfor %}
    <li {% if not next_url %}class="disabled"{% endif %}><a href="{{ next_url }}{{ hash }}">→</a></li>
</ul>
