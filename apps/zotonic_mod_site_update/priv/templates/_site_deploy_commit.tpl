<p class="list-group-item-heading"><strong>{{ commit.subject|escape }}</strong></p>
<p class="list-group-item-text text-muted">
    <code>{{ commit.hash|escape }}</code>
    &middot; {{ commit.author|escape }}
    &middot; <time datetime="{{ commit.date|escape }}">{{ commit.date|escape }}</time>
</p>
{% if commit.body %}
    <details>
        <summary>{_ Details _}</summary>
        <p>{{ commit.body|escape|linebreaksbr }}</p>
    </details>
{% endif %}
