<feed xmlns="http://www.w3.org/2005/Atom" xml:base="{% url home absolute_url %}" xml:lang="{{ z_language }}">
    <generator uri="http://zotonic.com/">Zotonic - Atom Feed Module</generator>
    <link rel="self" type="application/atom+xml" href="{{ feed_url|escapexml }}" />
    <id>{{ feed_url|escapexml }}</id>
    <title>{{ m.site.title|escapexml }}{% with title|default:id.title as feed_title %}{% if feed_title %} - {{ feed_title|escapexml }}{% endif %}{% endwith %}</title>
