<script type="application/ld+json">
{
    "@context": "http://schema.org",
    "@type": "{{ type|default:"Article" }}",
    "mainEntityOfPage": {
        "@type": "WebPage",
        "@id": "{{ m.rsc[id].page_url_abs }}"
    },
    "headline": "{{ id.title }}",
    "description": "{{ id|summary }}",
    {% if id.depiction %}
    "image": {
        "@type": "ImageObject",
        "url": "{% image_url id absolute_url mediaclass="schema-org-image" %}",
        "width": 1024,
        "height": 800
    },
    {% endif %}
    "datePublished": "{{ id.publication_start|date:"c" }}",
    "dateModified": "{{ id.modified|date:"c" }}",
    {% if id.author %}
    "author": {
        "@type": "Person",
        "@id": "{{ id.author.page_url_abs }}",
        "name": "{{ id.author.title }}"
    },
    {% endif %}
    "publisher": {% include "schema_org/types/_organization.tpl" logo="lib/images/logo.png" title=m.site.title url=m.site.protocol ++ "://" ++ m.site.hostname %}
}
</script>
