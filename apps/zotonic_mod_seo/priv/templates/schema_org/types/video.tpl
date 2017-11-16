<script type="application/ld+json">
{
    "@context": "http://schema.org",
    "@type": "VideoObject",
    "name": "{{ id.title }}",
    "description": "{{ id|summary }}",
    "thumbnailUrl": "{% image_url id mediaclass="base-thumbnail" absolute_url %}",
    "uploadDate": "{{ id.created|date:"c" }}",
    "contentUrl": "{% url media_inline star=m.media[id].filename absolute_url %}",
    {% if id.publication_end %}
    "expires": "{{ id.publication_end|date:"c" }}",
    {% endif %}
    "publisher": {% include "schema_org/types/_organization.tpl" logo="lib/images/logo.png" title=m.site.title url=m.site.protocol ++ "://" ++ m.site.hostname %}
}
</script>
