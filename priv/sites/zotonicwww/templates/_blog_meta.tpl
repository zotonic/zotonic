    {% with m.rsc[id].o.author as author %}
    <p class="meta">{% if author %}<a href="{{ author.page_url }}">{{ author.title }}</a>&mdash; {% endif %}{{ m.rsc[id].publication_start|date:"j F Y" }}</p>
    {% endwith %}
