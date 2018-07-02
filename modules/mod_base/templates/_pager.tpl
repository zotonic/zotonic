<div class="pagination pagination-centered">
    <ul>
        <li {% if not prev_url %}class="disabled"{% endif %}>
            <a href="{{ prev_url }}#content-pager">← {_ Previous _}</a>
        </li>
        {% for nr, url in pages %}
            {% if nr %}
                <li class="{% if nr == page %}active {% endif %}hidden-phone">
                    <a href="{{ url }}#content-pager">{{ nr }}</a>
                </li>
            {% else %}
                <li class="disabled hidden-phone">
                    <a href="#">…</a>
                </li>
            {% endif %}
        {% endfor %}
        <li {% if not next_url %}class="disabled"{% endif %}>
            <a href="{{ next_url }}#content-pager">{_ Next _} →</a>
        </li>
    </ul>
</div>
