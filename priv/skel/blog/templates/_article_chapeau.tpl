{% with id|menu_trail as parents %}
{% if parents %}
<h5 class="chapeau">
    {% for p in parents %}
    <a href="{{ m.rsc[p].page_url }}">{{ m.rsc[p].title }}</a>
    {% if not forloop.last %}&raquo;{% endif %}
    {% endfor %}
</h5>
{% endif %}
{% endwith %}
