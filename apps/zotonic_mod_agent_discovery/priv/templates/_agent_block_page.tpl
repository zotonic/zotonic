{% if reference %}
<section>
    <h2><a href="{{ reference.page_url|escape }}">{{ reference.title|striptags|escape }}</a></h2>
    <p>Language: {{ reference.language|escape }}</p>
    <p>{{ reference.summary }}</p>
    {{ reference.body }}
    {% if reference.license %}<p>License: {{ reference.license|escape }}</p>{% endif %}
    {% if reference.attribution %}<p>Attribution: {{ reference.attribution|escape }}</p>{% endif %}
    {% if not reference.complete %}<p>Content is incomplete; follow the linked source page.</p>{% endif %}
</section>
{% endif %}
