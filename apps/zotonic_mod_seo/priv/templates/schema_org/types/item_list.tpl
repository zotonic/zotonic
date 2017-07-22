<script type="application/ld+json">
{
    "@context": "http://schema.org",
    "@type": "ItemList",
    "itemListElement": [
        {% for item in items %}
        {
            "@type": "ListItem",
            "position": {{ forloop.counter }},
            "url": "{{ m.rsc[item].page_url_abs }}"
        }{% if not forloop.last %},{% endif %}
        {% endfor %}
    ]
}
</script>
