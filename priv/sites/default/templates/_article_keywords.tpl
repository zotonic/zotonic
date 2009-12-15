<p class="article-keywords">
    Keywords:
    {% for id in m.rsc[id].o.subject %}
    <a href="{{ m.rsc[id].page_url }}">{{ m.rsc[id].title }}</a>{% if not forloop.last %},{% endif %}
    {% endfor %}
</p>

