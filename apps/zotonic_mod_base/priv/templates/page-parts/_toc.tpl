{% with "page-parts/_toc.tpl" as template %}
{% with level|default:1 as level %}
{% if toc %}
    <ol class="toc-level-{{ level }}">
    {% for anchor, text, children in toc %}
        <li>
            {% if anchor %}
                <a href="#{{ anchor|escape }}">{{ text }}</a>
            {% endif %}
            {% include template toc=children level=level+1 %}
        </li>
    {% endfor %}
    </ol>
{% endif %}
{% endwith %}
{% endwith %}
