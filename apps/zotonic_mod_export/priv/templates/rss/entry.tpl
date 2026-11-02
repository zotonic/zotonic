{% if id and id.is_visible %}
<item>
    <guid isPermaLink="false">{{ id.uri|escapexml }}</guid>
    <link>{{ id.page_url_abs|escapexml }}</link>
    <atom:link rel="https://zotonic.com/rel/resource" type="application/json" href="{% url api_rsc_export id=id absolute_url %}" />
    <title>{{ id.title|escapexml }}</title>
    <description>{{ id.summary|escapexml }}</description>
    {% if id.body %}<content:encoded>{{ id.body|escapexml }}</content:encoded>{% endif %}
    {% if published_rfc822 %}<pubDate>{{ published_rfc822 }}</pubDate>{% endif %}
    <atom:updated>{{ id.modified|date:"c":true }}</atom:updated>
    <dc:creator>{% with id.o.author[1]|default:id.creator_id as author_id %}{% filter escapexml %}{% include "_name.tpl" id=author_id %}{% endfilter %}{% endwith %}</dc:creator>
    {% with id.medium as medium %}
        {% if medium.filename and medium.size|is_defined %}
            <enclosure type="{{ medium.mime|escapexml }}" length="{{ medium.size }}" url="{% url media_attachment star=medium.filename absolute_url %}" />
        {% endif %}
    {% endwith %}
    {% if id.category.name %}<category>{{ id.category.name|escapexml }}</category>{% endif %}
</item>
{% endif %}
