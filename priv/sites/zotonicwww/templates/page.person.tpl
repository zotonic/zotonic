{% extends "page.tpl" %}

{% block below_content %}

{% with m.search[{query cat='article' sort='-publication_start' hasobject=[id, 'author']}] as articles %}
{% if articles %}

<h2>Blog posts</h2>
<ul>
    {% for id in articles %}
    <li>
        <p><a href="{{ m.rsc[id].page_url }}">{{ m.rsc[id].title }}</a> 
            &mdash; {{ m.rsc[id].publication_start|date:"j F Y" }}
        </p>
    </li>
    {% endfor %}
</ul>
{% endif %}
{% endwith %}

{% endblock %}
