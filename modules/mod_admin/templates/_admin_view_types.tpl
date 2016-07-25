{% block content_types %}
    <li><a href="{{ page_url }}">{_ page _}</a></li>
    {% for content_type, url in id|content_type_urls %}
        <li><a href="{{ url }}">{{ content_type|content_type_label }}</a>
    {% endfor %}
{% endblock %}

{% block extra %}{% endblock %}
