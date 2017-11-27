{% block title %}
    {% if m.site.title %}
        <meta name="twitter:site" content="{{ m.site.title }}">
    {% endif %}
{% endblock %}
{% if id %}
    <meta name="twitter:title" content="{{ id.title }}">
    <meta name="twitter:description" content="{{ id|summary:135 }}">
	{% if id.depiction %}
        <meta name="twitter:image" content="http://{{ m.site.hostname }}{% image_url id.depiction mediaclass="facebook-og" %}">
        <meta name="twitter:card" content="summary_large_image">
    {% else %}
        <meta name="twitter:card" content="summary">
    {% endif %}
{% else %}
    {% block no_id %}
        <meta name="twitter:card" content="summary">
    {% endblock %}
{% endif %}
