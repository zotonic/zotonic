<nav class="navigation">
    <ul class="pagination justify-content-center" {% if topic %}data-onclick-topic="{{ topic|escape }}"{% endif %}>
        <li class="page-item {% if not prev_url %}disabled{% endif %}">
            <a href="{{ prev_url }}{{ hash }}" class="page-link">←</a>
        </li>
        {% for nr, url in pages %}
            {% if nr %}
                <li class="page-item {% if nr == page %}active{% endif %}">
                    <a href="{{ url }}{{ hash }}" class="page-link">{{ nr }}</a>
                </li>
            {% else %}
                <li class="page-item"><a href="{{ url }}{{ hash }}" class="page-link">…</a></li>
            {% endif %}
        {% endfor %}
        <li class="page-item {% if not next_url %}disabled{% endif %}">
            <a href="{{ next_url }}{{ hash }}" class="page-link">→</a>
        </li>
    </ul>
</nav>