{% block content_types %}
    {% if page_url %}
        <li><a href="{{ page_url }}" target="_blank">{_ page _}</a></li>
    {% endif %}
    {% for content_type, url in id|content_type_urls %}
        <li><a href="{{ url }}" target="_blank">{{ content_type|content_type_label }}</a>
    {% endfor %}
{% endblock %}

{% block extra %}{% endblock %}
