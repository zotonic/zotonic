<script type="application/ld+json">
{
    "@context": "http://schema.org",
    "@type": "WebSite",
    "url": "{{ m.site.protocol }}://{{ m.site.hostname }}",
    "name": "{{ m.site.title }}",
    "potentialAction": {
        "@type": "SearchAction",
        "target": "{% url search absolute_url %}?qs={search_term_string}",
        "query-input": "required name=search_term_string"
    }
}
</script>
