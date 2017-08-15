<script type="application/ld+json">
{
    "@context": "http://schema.org",
    "@type": "Event",
    "name": "{{ id.title }}",
    "startDate": "{{ id.date_start|date:"c" }}",
    {% with id.location[1]|default:id as location %}
        {% if location.address_city %}
            "location": {
                "@type": "Place",
                "name": "{{ location.title }}",
                "address": {
                    "@type": "PostalAddress",
                    "streetAddress": "{{ location.address_street_1 }}",
                    "addressLocality": "{{ location.address_city }}",
                    "postalCode": "{{ location.address_postcode }}",
                    "addressCountry": "{{ location.address_country }}"
                }
            },
        {% endif %}
    {% endwith %}
    {% if id.depiction %}
    "image": "{% image_url id absolute_url mediaclass="schema-org-image" %}",
    {% endif %}
    "description": "{{ id|summary }}",
    "endDate": "{{ id.date_end|date:"c" }}"
}
</script>
