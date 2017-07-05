<feed xmlns="http://www.w3.org/2005/Atom" xml:base="{% url home absolute_url %}" xml:lang="{{ z_language }}">
    <generator uri="http://zotonic.com/">Zotonic - Atom Feed Module</generator>
    <updated>{{ now|date:"c" }}</updated>
    <logo />
    {# <link rel="self" type="application/atom+xml" href="{% url atom_feed cat=cat %}" /> #}
    <id>{% url home absolute_url %}</id>
    <title>{{ m.config.site.title.value|escapexml }}{% if title %} - {{ title|escapexml }}{% endif %}</title>
