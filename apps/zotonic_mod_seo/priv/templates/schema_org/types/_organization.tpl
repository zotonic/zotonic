{
        "@context": "http://schema.org",
        "@type": "Organization",
        "name": "{{ title|default:id.title }}",
        {% if logo %}
        "logo": {
            "@type": "ImageObject",
            "url": "{% image_url logo absolute_url mediaclass="schema-org-logo" %}",
            "height": 60
        },
        {% endif %}
        "url": "{{ url|default:id.website }}"
    }
