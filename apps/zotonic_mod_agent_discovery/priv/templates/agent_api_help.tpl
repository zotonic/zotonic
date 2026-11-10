{% extends "base.tpl" %}

{% block title %}{_ Data and API _} — {{ m.site.title|escape }}{% endblock %}

{% block content %}
<main>
    <h1>{_ Data and API _}</h1>
    <p>{_ Explore this site's public information using ordinary links and HTTP GET requests. Public content does not require an account or API key. _}</p>
    <p>{{ discovery.description|escape }}</p>
    <ul>
        <li><a href="{{ discovery.openapi|escape }}">{_ OpenAPI description _}</a></li>
        <li><a href="{{ discovery.catalog|escape }}">{_ API catalog _}</a></li>
        <li><a href="{{ discovery.llms|escape }}">llms.txt</a></li>
        {% if discovery.sitemap %}
            <li><a href="{{ discovery.sitemap|escape }}">{_ Sitemap _}</a></li>
        {% endif %}
    </ul>
    {% for operation in discovery.operations %}
        <section>
            <h2>{{ operation.title|escape }}</h2>
            <p>{{ operation.description|escape }}</p>
            <pre><code>GET {{ operation.url|escape }}</code></pre>
            {% if operation.id == "search_resources" %}
                <p><a href="{{ operation.url|escape }}?page=1&amp;pagelen=20&amp;options.properties=title,summary,page_url_abs">{_ Browse public resources _}</a></p>
            {% endif %}
        </section>
    {% endfor %}
    <p>{_ Replace {id} with an ID or unique name obtained from search results or a page's alternate link. Use small pages and follow pagination to explore further. _}</p>
    <p>{_ Responses normally contain status and result fields. Translated text can be a translation object. Resource exports provide page URLs by language, including x-default. Cite the human page URL in the appropriate language. _}</p>
    <p>{_ Existing APIs apply the caller's permissions. They return only accessible information and keep their existing indexing headers. Content and media may have their own licenses; preserve attribution and follow the stated terms. _}</p>
</main>
{% endblock %}
