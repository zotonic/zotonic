<div class="pager">
    <ul>
        {% if prev_url %}
            <li class="previous"><a href="{{ prev_url }}#content-pager">← {_ Previous _}</a></li>
        {% endif %}
        {% if next_url %}
            <li class="next"><a href="{{ next_url }}#content-pager">{_ Next _} →</a></li>
        {% endif %}
    </ul>
</div>
