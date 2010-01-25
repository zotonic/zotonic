<feed xmlns="http://www.w3.org/2005/Atom" xml:base="{{ site_url }}" xml:lang="en">
	<generator uri="http://zotonic.com/">Zotonic - Atom Feed Module</generator>
	<updated>{{ updated|date:"c" }}</updated>
	<logo />
	<link rel="self" type="application/atom+xml" href="{{ site_url }}{% url atom_feed cat=cat %}" />
	<id>{{ site_url }}{% url atom_feed cat=cat %}</id>
	<title>{{ m.config.site.title.value|escapexml }} - {{ m.rsc[cat].title|escapexml }}</title>

    {% if upcoming %}
	{% for id in m.search[{upcoming cat=cat}] %}
		{% catinclude "_atom_entry.tpl" id upcoming=1 %}
	{% endfor %}
    {% else %}
	{% for id in m.search[{query cat=cat sort="-publication_start"}] %}
		{% catinclude "_atom_entry.tpl" id %}
	{% endfor %}
	{% endif %}
</feed>
