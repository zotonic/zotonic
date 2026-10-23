<rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom" xmlns:content="http://purl.org/rss/1.0/modules/content/" xmlns:dc="http://purl.org/dc/elements/1.1/">
<channel>
    <title>{{ m.site.title|escapexml }}{% with title|default:id.title as feed_title %}{% if feed_title %} - {{ feed_title|escapexml }}{% endif %}{% endwith %}</title>
    <link>{% url home absolute_url %}</link>
    <description>{{ id.summary|default:id.title|default:m.site.title|escapexml }}</description>
    <atom:link rel="self" type="application/rss+xml" href="{{ feed_url|escapexml }}" />
    <language>{{ z_language }}</language>
    <generator>Zotonic</generator>
